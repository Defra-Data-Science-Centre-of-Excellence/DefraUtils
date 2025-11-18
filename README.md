# DefraUtils

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version/DefraUtils.png)](https://CRAN.R-project.org/version/DefraUtils)
![Development
Status](https://img.shields.io/badge/development-active-brightgreen.png)

<!-- badges: end -->

## What is DefraUtils?

DefraUtils is a versatile R package providing a collection of useful functions developed by analysts in Defra Farming Stats. By making these functions publicly available, we aim to:

* Share tools that may be useful across projects
* Promote consistent coding practices within the Defra group
* Reduce duplication of effort where the same code is developed my mutliple analysts in isolation.

Although this package was developed by Farming Stats analysts, it is intended to be a resource for all analysts in Defra. Therefore, we welcome any contributions, bug fixes, and suggestions.

## How to install Defrautils?

The `DefraUtils` repository is public, meaning the package can easily be installed using the following code:

``` r
devtools::install_github("Defra-Data-Science-Centre-of-Excellence/DefraUtils")
```

## What can DefraUtils do?

`DefraUtils` is a versatile, general-purpose R package designed to make working in R easier and more efficient. It provides a wide range of utility functions to support data handling, platform integration, and workflow automation. Below, we outline the main areas the package covers and highlight some key features.

### General functionality

* **Simplifying connecting RStudio to GitHub**
* **Customisation of RStudio IDE**
* **Customising R console prompt**

### Standardising coding approaches

* **Script template with key metadata**
* **Script template for functions**
* **Template for project READMEs**
* **Analytical project template**

### Working on the DASH platform

* **Reading data** - various functions to read files from the DASH Unity Catalog. These are wrappers for `brickster::db_volume_read()`, but deal with the frequent http2 errors many users encounter when reading data using this function. There is a generic data load function as well as specific functions for reading common file types.
* **Writing data** - multiple functions for writing files to the DASH Unity Catalog. These are wrappers for `brickster::db_volume_write()`, that deal with the http2 errors. There is a generic write function as well as specific functions for writing several common file types.
* **Additional brickster functionality** - various other `brickster` functions with wrappers to prevent http2 errors, including for listing files and creating directories.
* **Alternative read and write functions** - we provide alternative functions to the `brickster` package for both reading and writing data on the DASH platform.

### FBS function

## How to contribute to DefraUtils?
