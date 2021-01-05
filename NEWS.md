# chartplotter 0.14.0

This version major extension of the chartplotter package.

* Shifts all calculations to the Z-score analysis metric by transform_z()
* Explicit definition of the display metric depending on target individual's sex and ga by transform_y()
* Renames ".z" to "_z" variables
* Improves various plotting functions

# chartplotter 0.13.1

# Repairs prediction line plotting

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
