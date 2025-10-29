# DefraUtils

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![CRAN
status](https://www.r-pkg.org/badges/version/a11ytables.png)](https://CRAN.R-project.org/version/DefraUtils)
![Development
Status](https://img.shields.io/badge/development-active-brightgreen.png)

<!-- badges: end -->

Defra utility functions

## Functions to add

### AUK functions
- [ ] auk_contact_card
- [ ] auk_heading
- [ ] auk_round_number
- [ ] auk_round_percent
- [ ] auk_source

Just listing all available functions - I don't think most of these should go in.

### Functions for DASH - all may become obsolete soon
- [ ] set_databricks_pat
- [ ] create_temp_file
- [ ] uc_volume_get
- [ ] uc_volume_put

### FBS functions
- [x] add_missing_columns
- [x] commentary_functions
- [x] decode_multi_choice
- [x] estimate_band_midpoints - renamed to estimate_band_means
- [x] fbs_model_functions - renamed to survey_model_functions
- [x] fbs_round - renamed to round_with_commas
- [x] fix_suppression & fix_suppression_circular - merged into a single function
- [x] get_cell_style
- [x] get_gdp_deflators - renamed to get-ons-series, added an option to get a snapshot version rather than live
- [x] overwrite_num_cols
- [x] tidy_log


### General functions
- [ ] project_template (not a function - appears in "new project" menu)
- [ ] script_template
- [ ] gitignore_template
- [ ] readme_template
- [ ] connect_github (needs updating to SSH)
- [ ] use_github (needs updating to SSH)
- [ ] change_console_prompt
