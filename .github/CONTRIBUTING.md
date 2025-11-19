# Contributing to DefraUtils

This guidance outlines how to propose a change or additional feature in `DefraUtils`. If you would like to join the team of analysts who maintain `DefraUtils`, please email the Farming Stats team at [AUK_stats_update@defra.gov.uk](mailto:AUK_stats_update@defra.gov.uk).

## Suggesting changes to existing files

If you want to suggest a large scale change to a function in DefraUtils, please first file an [issue](https://github.com/Defra-Data-Science-Centre-of-Excellence/DefraUtils/issues), which will bring the request to the attention of the package maintainers.

In the issue, please provide as much details as you can, including:

* The file/function that needs to be modified

* The nature of the change (e.g. fixing a typo, bug fix, improvement etc)

* The importance of the change (e.g. urgent, minor etc)

* Whether you know of a fix for the issue.

* If you encounter a bug with a function, please include a minimal [reproducible example (reprex)](https://tidyverse.org/help/#reprex) to illustrate the bug.

The package maintainers will consider the issue and decide if the change is needed.

If you have suggested you know a fix for the issue, you may be asked to make the change yourself, in which case you should follow the [making changes](#making-changes) guidance below.

## Adding a new function

If you have developed a function or feature that you would like to add to `DefraUtils` , please first file an [issue](https://github.com/Defra-Data-Science-Centre-of-Excellence/DefraUtils/issues), which will bring the request to the attention of the package maintainers.

In your pull request, please include:

* A brief description of the function/feature.

* A brife explanation of why it would be useful to other analysts across Defra group.

Then, please follow the [making changes](#making-changes) guidance below to add you function/feature.

## Making changes

* Clone the `DefraUtils` repository and **create a new branch** for your changes.

* Make your changes then commit them to GitHub. Ensure all changes comply with the formatting guidance detailed below.

* Once complete, **create a pull request** and include the following:

  * Give your pull request a **meaningful title**, which succinctly explains the change made.

  * Include the **issue number** in the body of your pull request. This means your issue will be closed when the pull request is merged into the main branch.

  * Add the package maintainers to the pull request, so they are notified that it is ready for review.

* Please add yourself to the authors list in the `DESCRIPTION` file. Please assign your roles as follows:

  * Use `"aut"` if you have written a function exported in the package.

  * Use `"ctb"` for all other contributions.

## Formatting and code style guidance

Below we include guidance on how to structure, document and test your changes. Any pull request which does not follow this guidance will not be merged.

* Where possible, please format your code following the [tidyverse style](https://style.tidyverse.org/). Please **do not** reformat code that is not related to your changes.

* All functions or changes to functions **must be properly documented** using `roxygen2` and markdown syntax. A template with the correct layout for the documentation of a function is included in the R folder. You can also use the roxygen script addin or function included in this package.

* All functions must include an `@author` tag in the `roxygen2` comments. This must include the authors name and email address, in case of issues.

* All functions must have been appropriately unit tested. It is advised to use `testthat` to unit testing of your changes.
