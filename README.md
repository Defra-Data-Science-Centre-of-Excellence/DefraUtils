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

## What can DefraUtils do?

As a versatile general purpose package, `DefraUtils` contains a wide range of functions to support working in R. Below we outline the areas the package covers and highlight some of the key functions.

### Customising RStudio IDE

### Standardising coding approaches

### Working on the DASH platform

* Various functions to read files from the DASH Unity Catalog. These are wrappers for `brickster::db_volume_read()`, but deal with the frequent http2 errors many users encounter when reading data using this function. There is a generic data load function as well as specific functions for reading common file types.
* Multiple functions for writing files to the DASH Unity Catalog. These are wrappers for `brickster::db_volume_write()`, that deal with the http2 errors. There is a generic write function as well as specific functions for writing several common file types.
* Various other `brickster` functions with wrappers to prevent http2 errors, including for listing files and creating directories. 

### FBS function

