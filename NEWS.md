# descstat 1.0-0

* descstat is now dependence free

* plots use the tinyplot package

# descstat 0.1-3

* the class is now of class `cls`, with a format method. The
  `pre_print` is almost unnecessary

* bug fixed in regline, the mean of x and not the variance was used to
  compute the slope

# descstat 0.1-2

* much improved documentation
* `marginal` now returns a `freq_table` object
* `cols` argument changed to `f`
* `var_decomp` changed to `anova`
* `cls2val` changed to `as_numeric`
* for consistency reasons, arguments are now called `data` if it is a
  tibble and `x` if it is a series

# descstat 0.1-1

* the `bins_table` function is removed, its functionnalities are now
  merged in the `freq_table` function
* a new `income` data set is added in order to illustrate how
  `freq_table` can be used with a frequency table as an input.
  
