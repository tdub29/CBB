"""Shared project palette for charts, reports, and future UI work."""

from __future__ import annotations

from collections.abc import Iterable

from matplotlib.colors import LinearSegmentedColormap
from matplotlib.figure import Figure
import matplotlib.pyplot as plt
import matplotlib.colors as mcolors
import numpy as np

PROJECT_PALETTE = {
    "black": "#111418",
    "midnight": "#0A1622",
    "deep_ocean": "#10263A",
    "storm": "#1C3850",
    "ink": "#1D2630",
    "slate": "#5F6B78",
    "steel": "#8793A1",
    "fog": "#D6DEE6",
    "paper": "#F5F7FA",
    "navy": "#173A5E",
    "signal_blue": "#2A5B84",
    "lake_blue": "#5F89B2",
    "sky_blue": "#AFC4D6",
    "sage": "#7D9386",
}

SERIES_COLORS = [
    PROJECT_PALETTE["signal_blue"],
    PROJECT_PALETTE["lake_blue"],
    PROJECT_PALETTE["fog"],
    PROJECT_PALETTE["sage"],
    PROJECT_PALETTE["steel"],
]

ALGORITHM_COLORS = {
    "ridge": PROJECT_PALETTE["signal_blue"],
    "lasso": PROJECT_PALETTE["navy"],
    "elasticnet": PROJECT_PALETTE["lake_blue"],
    "rf": PROJECT_PALETTE["sage"],
    "gbr": PROJECT_PALETTE["ink"],
    "random_forest": PROJECT_PALETTE["sage"],
    "gradient_boosting": PROJECT_PALETTE["ink"],
}

CBB_SEQUENTIAL_CMAP = LinearSegmentedColormap.from_list(
    "cbb_sequential",
    [
        PROJECT_PALETTE["midnight"],
        PROJECT_PALETTE["deep_ocean"],
        PROJECT_PALETTE["sky_blue"],
        PROJECT_PALETTE["lake_blue"],
        PROJECT_PALETTE["signal_blue"],
    ],
)

CBB_DIVERGING_CMAP = LinearSegmentedColormap.from_list(
    "cbb_diverging",
    [
        PROJECT_PALETTE["navy"],
        PROJECT_PALETTE["sky_blue"],
        PROJECT_PALETTE["fog"],
        "#C6D2C9",
        PROJECT_PALETTE["sage"],
    ],
)


def set_project_theme() -> None:
    """Apply the shared matplotlib theme for report graphics."""
    plt.rcParams.update(
        {
            "axes.prop_cycle": plt.cycler(color=SERIES_COLORS),
            "axes.facecolor": PROJECT_PALETTE["deep_ocean"],
            "figure.facecolor": PROJECT_PALETTE["midnight"],
            "savefig.facecolor": PROJECT_PALETTE["midnight"],
            "savefig.edgecolor": PROJECT_PALETTE["midnight"],
            "axes.edgecolor": PROJECT_PALETTE["storm"],
            "axes.labelcolor": PROJECT_PALETTE["fog"],
            "axes.titlecolor": PROJECT_PALETTE["fog"],
            "axes.titlesize": 14,
            "axes.titleweight": "semibold",
            "grid.color": PROJECT_PALETTE["storm"],
            "grid.alpha": 0.8,
            "grid.linewidth": 0.8,
            "xtick.color": PROJECT_PALETTE["steel"],
            "ytick.color": PROJECT_PALETTE["steel"],
            "text.color": PROJECT_PALETTE["fog"],
            "legend.frameon": False,
            "legend.labelcolor": PROJECT_PALETTE["fog"],
        }
    )


def apply_report_background(fig: Figure, axes: Iterable | None = None, *, seed: int = 42) -> None:
    """Add a subtle gradient and grain behind chart panels."""

    width = 1400
    height = 900
    x = np.linspace(0.0, 1.0, width)
    y = np.linspace(0.0, 1.0, height)
    xx, yy = np.meshgrid(x, y)

    base_top = np.array(mcolors.to_rgb(PROJECT_PALETTE["midnight"]))
    base_bottom = np.array(mcolors.to_rgb(PROJECT_PALETTE["deep_ocean"]))
    glow = np.array(mcolors.to_rgb(PROJECT_PALETTE["signal_blue"]))

    vertical_mix = yy[..., None]
    diagonal_mix = (0.65 * xx + 0.35 * (1.0 - yy))[..., None]
    background = base_top * (1.0 - vertical_mix) + base_bottom * vertical_mix
    background = background * (1.0 - 0.14 * diagonal_mix) + base_bottom * (0.14 * diagonal_mix)

    glow_mask = np.exp(-(((xx - 0.82) / 0.28) ** 2 + ((yy - 0.16) / 0.22) ** 2))
    background = np.clip(background + glow_mask[..., None] * 0.10 * glow, 0.0, 1.0)

    rng = np.random.default_rng(seed)
    noise = rng.normal(loc=0.5, scale=0.16, size=(height, width))
    noise = np.clip(noise, 0.0, 1.0)

    background_ax = fig.add_axes([0, 0, 1, 1], zorder=-100)
    background_ax.set_axis_off()
    background_ax.set_in_layout(False)
    background_ax.imshow(background, extent=(0, 1, 0, 1), aspect="auto", interpolation="bicubic")
    background_ax.imshow(
        noise,
        extent=(0, 1, 0, 1),
        aspect="auto",
        cmap="gray",
        alpha=0.035,
        interpolation="nearest",
    )

    target_axes = list(axes) if axes is not None else list(fig.axes)
    for ax in target_axes:
        if ax is background_ax:
            continue
        ax.set_zorder(1)
        ax.patch.set_facecolor(mcolors.to_rgba(PROJECT_PALETTE["deep_ocean"], alpha=0.94))
