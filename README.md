
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DisCoPals: Distinct Color Palettes for R

<!-- badges: start -->

[![GitHub
Release](https://img.shields.io/github/release/jrotzetter/DisCoPals?include_prereleases=&sort=semver&color=blue)](https://github.com/jrotzetter/DisCoPals/releases/ "View releases")
[![License](https://img.shields.io/github/license/jrotzetter/DisCoPals)](LICENSE.md)
[![Issues -
DisCoPals](https://img.shields.io/github/issues/jrotzetter/DisCoPals)](https://github.com/jrotzetter/DisCoPals/issues "View open issues")
[![R
Version](https://img.shields.io/badge/R-%3E%3D4.5.3-blue?logo=R&logoColor=white)](https://cran.r-project.org/ "Go to CRAN homepage")
[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

## Overview

**DisCoPals** is an R package designed to help researchers, data
scientists, and designers create, evaluate, and optimize color palettes
for accessibility and perceptual distinctness. It combines **two core
components**:

1.  **Built-in Palettes**: A curated collection of ready-to-use
    **distinct qualitative palettes** optimized for categorical data.
2.  **Evaluation Functions**: Functions to convert colors to the
    perceptually uniform CIELAB space, calculate industry-standard color
    difference metrics (DeltaE), measure WCAG-compliant contrast ratios,
    and filter palettes to ensure all colors are sufficiently
    distinguishable.

## Installation

The development version of DisCoPals can be installed from
[GitHub](https://github.com/jrotzetter/DisCoPals) with:

``` r
# install.packages("pak")
pak::pak("jrotzetter/DisCoPals")
```

## License

This package is released under the [MIT License](LICENSE.md), which
permits unrestricted use, distribution, and modification, provided the
original copyright notice and license text are included.
