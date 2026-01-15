# DefraUtils
<a><img src="man/figures/DefraUtils-hexsticker.png" align="right" height="200" /></a>

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version/DefraUtils.png)](https://CRAN.R-project.org/package=DefraUtils)
![Development
Status](https://img.shields.io/badge/development-active-brightgreen.png)

- [What is DefraUtils?](#what-is-defrautils)
- [How to install DefraUtils?](#how-to-install-defrautils)
- [How to contribute to DefraUtils?](#how-to-contribute-to-defrautils)
- [What can DefraUtils do?](#what-can-defrautils-do)
  - [Standardising coding approaches](#standardising-coding-approaches)
  - [Standardising outputs](#standardising-outputs)
  - [Helper functions](#helper-functions)
  - [Working on the DASH platform](#working-on-the-dash-platform)

## What is DefraUtils?

`DefraUtils` is a versatile R package that provides a collection of
useful functions developed by analysts in Defra Farming Stats. By making
these tools publicly available, we aim to:

- Share functionality that may be helpful across a range of analytical
  projects and professions.
- Encourage consistent and reproducible coding practices across the
  Defra group.
- Reduce duplication of effort where similar code might otherwise be
  developed independently by multiple analysts.

## How to install DefraUtils?

The `DefraUtils` repository is public, you can install the package
directly from GitHub using:

``` r
devtools::install_github("Defra-Data-Science-Centre-of-Excellence/DefraUtils")
```

However, if you are using Defra DASH, you should install from Package
Manager using:

``` r
install.packages("DefraUtils")
```

## How to contribute to DefraUtils?

Although the package originated within Farming Stats, it is designed as
a resource for all analysts across Defra and is under active
development. We therefore welcome contributions, bug reports, and
suggestions for improvements from all users.

To contribute, please raise an
[issue](https://github.com/Defra-Data-Science-Centre-of-Excellence/DefraUtils/issues)
or a [pull
request](https://github.com/Defra-Data-Science-Centre-of-Excellence/DefraUtils/pulls)
after reading the [contributing
guidance](https://github.com/Defra-Data-Science-Centre-of-Excellence/DefraUtils/blob/main/.github/CONTRIBUTING.md).

If you would like to join the team managing the `DefraUtils` package and
repo, please email the Farming Stats team at
<AUK_stats_update@defra.gov.uk>.

## What can DefraUtils do?

`DefraUtils` is a general-purpose R package designed to make working in
R easier, faster, and more consistent. It provides a broad set of
utility functions to support data handling, platform integration, and
workflow automation. Below is an overview of the main areas the package
covers, along with key features.

More details on all these features is provided in the [package
vignette](add-link-here).

### Standardising coding approaches

- **Analytical project template** – A basic project template for
  analytical work. It appears in RStudio’s “New Project” interface,
  making it easy to start a project with recommended structure. The
  structure is also available as a GitHub repository template.

- **Script templates** – A number of script templates, including a
  simple script header with prompts for key metadata. These help users
  to properly document their code. You can easily configure RStudio so
  all new scripts automatically open with this header.

- **Project README template** – A `.qmd` template that provides prompts
  for key information and suggested sections, helping ensure clear and
  consistent READMEs.

- **Suppression of identifiable data** - The `fix_suppression` function
  finds which values in a table require secondary suppression and
  overwrites their sample size to one - any subsequent suppression code
  will then suppress these figures. This is based on the principal of
  suppressing values with a sample size between 1 and 4
  (i.e. identifiable), as well as any other values which could be used
  to calculate identifiable values.

- **Adjusting for inflation** - The `get_ons_series` reads in quarterly
  ONS price indices and converts to an annual series. Once the series
  has been retrieved, you can convert a current terms value into real
  terms using `value / index * 100`, where index is the price / deflator
  index for the year the value corresponds to.

- **Code logging** - The `tidy_log` tidies up a log created by
  `futile.logger` and outputs to CSV.

### Standardising outputs

- **Visualisations** - The `easy_plot` function uses ggplot and afcharts
  to create accessible static charts. It has a large number of options
  so that the need to modify using ggplot code is minimised. The
  `create_defra_stats_infographic` function uses grid to output an
  infographic into the Plots pane, for sharing on social media. The
  `str_line_wrap` function can be used in axis labels to wrap strings to
  a set number of lines.

- **Tables** - The `get_cell_style` function returns `openxlsx` cell
  styles for stats publications. The `overwrite_num_cols` fixes columns
  where numbers are stored as text (e.g., where there is suppressed
  data). The `round_with_commas` rounds numbers for publishing/sharing
  using `janitor::round_half_up()` (as opposed to the R default
  ‘round-to-even’ method) and adds comma separators.

### Helper functions

- **Connecting RStudio to GitHub** – Two functions streamline connecting
  RStudio and GitHub, using either SSH (recommended for the DASH
  platform) or a Personal Access Token (recommended for local RStudio
  installations).

- **Customising the RStudio IDE** – A function that configures your
  RStudio IDE programmatically, including pane layout, font, theme, and
  other global options. This is especially useful for quickly setting up
  RStudio in transient environments, such as RStudio Server on DASH
  clusters.

- **Customising the R console prompt** – A small helper function that
  replaces the default console prompt with the name of your current Git
  branch.

- **Functions for estimating means of banded data**

- **Commentary functions**

- **Functions for decoding multiple choice answers**

- **Survey modelling functions**

### Working on the DASH platform

- **Reading data** – Functions for reading files from the DASH Unity
  Catalog. These wrap `brickster::db_volume_read()` but include handling
  for common HTTP/2 errors. A generic loading function is provided,
  along with type-specific readers for common file formats.

- **Writing data** – Functions for writing files to the DASH Unity
  Catalog. These wrap `brickster::db_volume_write()` and similarly
  mitigate HTTP/2 errors. A generic write function and format-specific
  helpers are included.

- **Additional `brickster` utilities** – Wrappers around various
  `brickster` functions—such as listing files and creating directories,
  designed to reduce HTTP/2 related failures.

- **Alternative read/write functions** – Additional functions that
  provide an alternative to `brickster` for reading and writing data on
  the DASH platform.
