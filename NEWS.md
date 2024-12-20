# chartplotter 0.35.1

- Exports `find_matches()` function

# chartplotter 0.35.0

- Adds functionality to plot the target height and target height range

# chartplotter 0.34.0

- Adds support for `dnr` is `NULL` argument in `process_chart()`

# chartplotter 0.33.1

- Updates to roxygen2 7.3.1 package

# chartplotter 0.33.0

- Resolves the incorrect time sequence of the WFH curve when height at a later age is lower. See <https://github.com/growthcharts/james/issues/24> It works for `curve_interpolation` is `FALSE`. Some approximation error remain for `curve_interpolation` is `TRUE`.

# chartplotter 0.32.1

- Set color blue for preterm D-score chart

# chartplotter 0.32.0

- Solves a problem resulted in `Error in eval(predvars, data, env) : object 'hgt_z_0' not found`. The problem occured in `find_matches()` when `hat` contains no brokenstick estimates. This may happen when child observations do not start at birth. The function now removes names of earlier predictors and matches only on covariates.

# chartplotter 0.31.2

- Sets colors to green for new D-score references (despite their WHOpink/blue defaults)

# chartplotter 0.31.1

- Solves a bug in the selection of the preterm reference in `set_curves()`

# chartplotter 0.31.0

- Updates `select()`, `drop_na()` and `rename()` function to `tidyselect 1.2.0` grammar that prohibits names like `.data$x`
- Adds extra sorting operation on age to so that WFH points are always connected in their time sequence

# chartplotter 0.30.0

- Repairs bug introduced by new behavior of `predict()` from `brokenstick 2.3.0`

# chartplotter 0.29.0

- Replaces "tibble with attribute" by list with elements `psn` and `xyz`

# chartplotter 0.28.0

- Replaces `new_data` by `newdata` for `brokenstick 2.0.0` package
- Removes `format = 1` arguments for `read_bds()` in tests
- Update dependency versions to `brokenstick 2.0.0` and `donorloader 0.31.1`

# chartplotter 0.27.1

* Use data from the `jamesdemodata` rather than `bdsreader` package

# chartplotter 0.27.0

* Supports the new `format` arguments in `bdsreader::read_bds()`

# chartplotter 0.26.0

* Solves problem with chart without `ynames`
* Adds some tweaks to GitHub Actions scripts

# chartplotter 0.25.0

* Relocates dependencies to `growthcharts` organisation

# chartplotter 0.24.0

* Relocates back to `stefvanbuuren` account
* Adds GHA `R-CMD-check` and `pkgdown`

# chartplotter 0.23.0

* Relocates dependencies to `growthcharts` organisation
* Improves handling of empty `target` object

# chartplotter 0.22.0

* MAJOR UPDATE
* Replaces `jamestest` by `jamesdemodata` package
* Replaces `minihealth` by `bdsreader` package
* Update all code and test files

# chartplotter 0.21.0

* Updates `ind.rds` to `minihealth 0.90.0`
* Adds a test on testfile 5

# chartplotter 0.20.0

* Main release, now completely based on `centile` package
* Unexport functions for internal use
* Switch to `markdown` documentation
* Solves problem with curve interpolation of WFH
* Limit prediction horizon to 2 years when there is no model
* Sets the maximum prediction horizon equal to last internal knot
* Solves a problem with curve interpolation of the D-score
* Evades prediction on NULL models
* Silences `pivot_longer()` if no ynames are found
* Makes `yname` optional so that it doesn't get stuck on `dsc`
* `process_chart()` returns early if there are no data
* Styles the R sources

# chartplotter 0.19.0

* Simplifies plotting logic
* Solves the problem that did not plot plot T 3254 and T 6021
* Solves bug that did not plot 1-point curve
* Adds data dump for debugging purposes
* Add `safe_approx()` function to deal with zero observations
* Uses `rule = 2` in approx to calculate Z-score outside the reference (note: this may not always be appropriate)

# chartplotter 0.18.0

* Repair typo in `set_curves()` that crashes JAMES
* Make `set_curves()` work with zero observations

# chartplotter 0.17.0

* Replace `yzy` by `centile` package

# chartplotter 0.16.0

* Major overhaul
* Splits calculation and plotting
* Replaces `clopus` dependency by `yzy` and `jamesxzy` imports

# chartplotter 0.15.0

* Accepts new `dnr` arguments: `"0-2"`, `"2-4"` and `"4-18"` as donor data choices
* Repairs the curve interpolation problem for Kevin S

# chartplotter 0.14.0

This version major extension of the chartplotter package.

* Shifts all calculations to the Z-score analysis metric by transform_z()
* Explicit definition of the display metric depending on target individual's sex and ga by transform_y()
* Renames ".z" to "_z" variables
* Improves various plotting functions

# chartplotter 0.13.1

* Repairs prediction line plotting

# chartplotter 0.13.0

* Major update: Consistent use of measurement-analysis-display metrics
* Prediction lines not yet functional

# chartplotter 0.12.0

* Repairs discrepancy in Z-transformation in `find_matches()` that resulted in incorrect Z-scores for preterms in mixed data (#2)

# chartplotter 0.11.0

* Removes `grid.draw()` from `process_chart()`, so now your need to run `grid.draw()` to render the graph

# chartplotter 0.10.1

* Reinstates dependency `brokenstick::get_knots()` because minihealth needs the brokenstick dependency anyway (for predict)

# chartplotter 0.10.0

* Removes dependency on `brokenstick::get_knots()` by making a copy

# chartplotter 0.9.0

* Updates to `R 4.0.0` and more recent `brokenstick`, `curvematching` packages

# chartplotter 0.8.0

* Draws prediction line as continuous between last observation and prediction
* Adds a `NEWS.md` file to track changes to the package.
