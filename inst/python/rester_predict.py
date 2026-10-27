"""Inference helpers bridging R and the AcceleRest Python code.

The model architecture is imported from a downloaded copy of
https://github.com/NielsRLorenzen/AcceleRest (see `ar_download_repo()` in R)
and the pretrained weights are loaded from local files (see
`ar_download_weights()` in R), so no network access happens here.
"""
import importlib.util
import os
import sys

import numpy as np
import pandas as pd
import torch
import torch.nn as nn

if not hasattr(nn, "RMSNorm"):
    # torch.nn.RMSNorm was added in torch 2.4, but the last torch release
    # with Intel macOS wheels is 2.2.2.  This matches its definition (and
    # state dict) for the default `elementwise_affine=True`.
    class RMSNorm(nn.Module):
        def __init__(self, normalized_shape, eps=None,
                     elementwise_affine=True, device=None, dtype=None):
            super().__init__()
            if isinstance(normalized_shape, int):
                normalized_shape = (normalized_shape,)
            self.normalized_shape = tuple(normalized_shape)
            self.eps = eps
            self.elementwise_affine = elementwise_affine
            if elementwise_affine:
                self.weight = nn.Parameter(
                    torch.ones(self.normalized_shape, device=device, dtype=dtype)
                )
            else:
                self.register_parameter("weight", None)

        def forward(self, x):
            eps = torch.finfo(x.dtype).eps if self.eps is None else self.eps
            dims = tuple(range(-len(self.normalized_shape), 0))
            x = x * torch.rsqrt(x.pow(2).mean(dim=dims, keepdim=True) + eps)
            if self.weight is not None:
                x = x * self.weight
            return x

    nn.RMSNorm = RMSNorm

# Prediction head name -> hubconf builder function
HEAD_BUILDERS = {
    "linear_sleepstages": "accelerest_sleepstage",
    "lstm_sleepstages": "accelerest_sleepstage_lstm",
    "linear_respevents": "accelerest_respevent",
}


def import_hubconf(repo_dir):
    """Import `hubconf.py` from a local AcceleRest repository."""
    repo_dir = os.path.abspath(os.path.expanduser(repo_dir))
    if repo_dir not in sys.path:
        sys.path.insert(0, repo_dir)
    spec = importlib.util.spec_from_file_location(
        "accelerest_hubconf", os.path.join(repo_dir, "hubconf.py")
    )
    hubconf = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(hubconf)
    return hubconf


def load_state_dict(path):
    return torch.load(path, map_location="cpu", weights_only=True)


def load_model(repo_dir, weight_files, device="cpu"):
    """Build a multi-head AcceleRest model from local weight files.

    `weight_files` is a dict of head name -> `.pt` state dict path.
    `hubconf.accelerest_multihead()` cannot be used with local files: with
    `pretrained=False` each head gets a random backbone and the shared
    backbone check fails, so the heads are built and loaded individually.
    """
    hubconf = import_hubconf(repo_dir)
    models = []
    names = []
    for name, path in weight_files.items():
        builder = getattr(hubconf, HEAD_BUILDERS[name])
        model = builder(pretrained=False)
        model.load_state_dict(load_state_dict(path), strict=True)
        models.append(model)
        names.append(name)
    model = hubconf.MultiHeadAcceleRest(models, names)
    model.to(device)
    model.eval()
    return model


def default_device():
    return "cuda" if torch.cuda.is_available() else "cpu"


def dataframe_to_actipy(data, time_col="time"):
    """Convert an R data.frame (time, x, y, z) to the format `actipy` uses."""
    data = pd.DataFrame(data)
    index = pd.DatetimeIndex(pd.to_datetime(data[time_col]))
    if index.tz is not None:
        # actipy works on naive device clock times
        index = index.tz_localize(None)
    data = data[["x", "y", "z"]].astype(np.float32)
    data.index = index
    data.index.name = "time"
    return data


def process_dataframe(data, sample_rate, **kwargs):
    import actipy

    data = dataframe_to_actipy(data)
    return actipy.process(data, sample_rate, **kwargs)


def read_h5(path):
    import h5py

    with h5py.File(path, "r", rdcc_nbytes=1024**3) as f:
        return np.array(f["data/accelerometry"], dtype=np.float32)


def actipy_to_array(data):
    return data[["x", "y", "z"]].to_numpy(dtype=np.float32).T


def _prepared(data, info):
    start_time = str(data.index[0]) if len(data) > 0 else None
    return actipy_to_array(data), start_time, info


def prepare_dataframe(data, sample_rate, **kwargs):
    """Preprocess an R data.frame (time, x, y, z) with actipy."""
    return _prepared(*process_dataframe(data, sample_rate, **kwargs))


def predict(
    model,
    accelerometry,
    context_window_shift=1,
    max_batch_size=16,
    device="cpu",
):
    """Run every head of `model` over an (axes x samples) array.

    Context windows of `model.max_seq_len` patches are shifted by
    `context_window_shift` patches; logits for a patch are averaged over all
    windows covering it (ignoring NaN logits, e.g. from non-wear), then
    softmaxed.  Patches with no valid logits are returned as NaN.

    Returns a dict of head name -> (n_patches x n_classes) array.
    """
    context_window_shift = int(context_window_shift)
    max_batch_size = int(max_batch_size)
    data = torch.from_numpy(np.ascontiguousarray(accelerometry, dtype=np.float32))
    if data.ndim != 2 or data.shape[0] != 3:
        raise ValueError(f"Expected a (3 x n_samples) array, got {tuple(data.shape)}")

    patch_size = int(model.patch_size)
    window_patches = int(model.max_seq_len)
    window_samples = window_patches * patch_size
    if data.shape[1] < window_samples:
        raise RuntimeError(
            f"Number of samples {data.shape[1]} not enough for window size "
            f"{window_samples} ({window_samples / 30 / 3600:.2f} hours at 30Hz)."
        )

    # (axes, n_windows, window_samples) -> (n_windows, axes, window_samples)
    windows = data.unfold(-1, window_samples, context_window_shift * patch_size)
    windows = windows.permute(1, 0, 2)
    num_windows = windows.shape[0]
    num_patches = (num_windows - 1) * context_window_shift + window_patches

    sums = {}
    counts = {}
    with torch.no_grad():
        for batch_start in range(0, num_windows, max_batch_size):
            x = windows[batch_start: batch_start + max_batch_size].to(device)
            outputs = model(x)
            for name, logits in outputs.items():
                logits = logits.float().cpu()
                if name not in sums:
                    num_classes = logits.shape[-1]
                    sums[name] = torch.zeros((num_patches, num_classes))
                    counts[name] = torch.zeros((num_patches, 1))
                valid = ~torch.isnan(logits)
                logits = torch.nan_to_num(logits, nan=0.0)
                for i in range(logits.shape[0]):
                    start = (batch_start + i) * context_window_shift
                    rows = slice(start, start + window_patches)
                    sums[name][rows] += logits[i] * valid[i]
                    counts[name][rows] += valid[i].any(dim=-1, keepdim=True)

    result = {}
    for name in sums:
        soft_preds = sums[name].div(counts[name].clamp_min(1)).softmax(dim=-1)
        soft_preds[counts[name].squeeze(-1) == 0] = float("nan")
        result[name] = soft_preds.numpy()
    return result
