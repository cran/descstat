## ----setup, echo = FALSE-----------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE,
                      fig.width = 7,
                      out.width = '80%', fig.asp = 0.6,
                      fig.align = "center")
options(
    htmltools.dir.version = FALSE, formatR.indent = 2, width = 55, digits = 4,
    tibble.print_max = 5, tibble.print_min = 5)

## ----load_pkgs---------------------------------------
library(descstat)
library(tinyplot)
tinyplot::tinytheme("clean")

## ----------------------------------------------------
padova$price |> range()
bin(padova$price) |> head()

## ----freq_table_continuous---------------------------
bin(padova$price, breaks = c(250, 500, 750)) |> head()
bin(padova$price, breaks = c(30, 250, 500, 750, 1000)) |> head()

## ----------------------------------------------------
head(wages$wage)

## ----------------------------------------------------
wages$wage |> bin(breaks = c(1, 4, 8)) |> head()

## ----------------------------------------------------
wages$wage |> bin(breaks = 8) |> head()

## ----------------------------------------------------
rgp$children |> head(12)
rgp$children |> bin(max = 3) |> head(12)

## ----------------------------------------------------
wages$sector |> head(5)

## ----results = "hide"--------------------------------
wages$sector |> bin()

## ----------------------------------------------------
wages$sector |> bin(levels = c("administration", "business", "services", 
                             "industry", "building")) |> head()


## ----------------------------------------------------
wages$sector |> bin(levels = c(government = "administration", "business", "services", 
                             "industry", construction = "building")) |> head()


## ----------------------------------------------------
wages$sector |> 
    bin(levels = list(pub = "administration", priva = c("industry", "building"),
                      privb = c("business", "services"))) |>  head()

## ----wsize-------------------------------------------
wsize <- head(wages$size)
wsize

## ----as_numeric_base---------------------------------
wsize |> as_numeric()

## ----as_numeric_ex-----------------------------------
wsize |> as_numeric(pos = 1)
wsize |> as_numeric(pos = 0.5)

## ----as_numeric_wlast--------------------------------
wsize |> as_numeric(pos = 0.5, wlast = 2)

## ----as_numeric_xlast, results = 'hide'--------------
wsize |> as_numeric(pos = 0.5, xlast = 400)

## ----as_numeric_xfirst-------------------------------
wsize |> as_numeric(pos = 0.5, xlast = 400, xfirst = 2)

## ----bin_attributes----------------------------------
wsize |> bin(xlast = 400, xfirst = 2) |> as_numeric(pos = 0.5)

## ----rgp---------------------------------------------
rgp |> head(5)

## ----rgp_count---------------------------------------
table(rgp$children) |> as.data.frame() |> setNames(c("children", "n"))

## ----freq_table_base, results = 'hide'---------------
rgp |> freq_table(children)

## ----freq_table_freqs--------------------------------
rgp |> freq_table(children, "nfpNFP")

## ----freq_table_max----------------------------------
rgp |> freq_table(children, max = 3, total = TRUE)

## ----freq_table_max_print----------------------------
rgp |> freq_table(children, max = 3, total = TRUE, print = TRUE)

## ----freq_table_size---------------------------------
wages |> freq_table(size)

## ----freq_table_breaks-------------------------------
wages |> freq_table(size, breaks = c(20, 250))

## ----freq_table_breaks2, results = 'hide'------------
wages |>
    transform(size = bin(size, breaks = c(20, 250))) |>
    freq_table(size)

## ----range_padova, collapse = TRUE-------------------
padova |> freq_table(price)

## ----freq_table_bins---------------------------------
wages |> freq_table(size, "dmM", breaks = c(20, 100, 250))

## ----freq_table_bins_vals----------------------------
wages |> freq_table(size, "p", vals = "xlua", breaks = c(20, 100, 250), wlast = 2)

## ----income------------------------------------------
income |> head(5)

## ----freq_table_freq---------------------------------
income |> freq_table(inc_class, freq = number,
                     breaks = c(50, 100, 500, 1000))

## ----freq_table_mass---------------------------------
income |> freq_table(inc_class, freq = number,
                     mass = tot_inc,
                     breaks = c(50, 100, 500, 1000))

## ----cont_table_weights------------------------------
employment |> freq_table(age, weights = weights, total = TRUE, print = TRUE)

## ----functions, echo = FALSE-------------------------
data.frame(R        = c("mean", "median", "quantile", "var",
                        "sd",    "mad", "", "", "", "", ""),
           descstat = c("mean", "median", "quantile", "variance",
                        "stdev", "madev", "modval", "medial", "gini",
                        "skewness", "kurtosis")) |> 
    knitr::kable(caption = "Table 1: Functions for descriptive statistics",
                 booktabs = TRUE)

## ----central_stat, collapse = TRUE-------------------
z <- wages |> freq_table(wage)
z |> mean()
z |> median()
z |> modval()

## ----disp_stat, collapse = TRUE----------------------
z |> stdev()
z |> variance()
z |> madev()

## ----quantiles, collapse = TRUE----------------------
z |> quantile(probs = c(0.25, 0.5, 0.75))
z |> quantile(y = "mass", probs = c(0.25, 0.5, 0.75))

## ----median_medial, collapse = TRUE------------------
z |> median()
z |> medial()

## ----gini_coef, collapse = TRUE----------------------
z |> gini()

## ----shape_stat, collapse = TRUE---------------------
z |> skewness()
z |> kurtosis()

## ----modval_wages------------------------------------
wages |> freq_table(sector) |> modval()

## ----barplot, fig.cap = "Figure 1:  Bar plot for a frequency table"----
s <- freq_table(wages, sector, f = "f")
tinyplot(f ~ sector, data = s, type = "barplot", flip = TRUE, yaxl = "%", ylab = "Frequencies")

## ----barplotraw, fig.cap = "Bar plot for a raw table", eval = FALSE----
# tinyplot(~ sector, data = wages, type = "barplot", flip = TRUE, ylab = "Frequencies")

## ----barplotint, fig.cap = "Figure 2: Bar plot for integer series"----
i <- freq_table(rgp, children, max = 3, f = "f", print = TRUE)
tinyplot(f ~ children, data = i, type = "barplot", flip = TRUE, yaxl = "%")

## ----pie2, fig.asp = 1, out.width = "50%", fig.cap = "Figure 3: A pie chart"----
tinyplot(~ sector, wages, type = type_pie())

## ----pie2bis, fig.asp = 1, out.width = "50%", eval = FALSE----
# ws <- freq_table(wages, sector)
# tinyplot(n ~ sector, ws, type = type_pie())

## ----pie3, fig.asp = 1, out.width = "50%", fig.cap = "Figure 4: A pie chart with customized initial angle and labels positions"----
tinyplot(~ sector, wages,
         type = type_pie(position = c(1.1, 0.7), init.angle = 10, f = "p",
                         radius = 1.3, pal = "Tableau 10"))

## ----donut, fig.asp = 1, out.width = "50%", fig.cap = "Figure 5: Donut chart"----
tinyplot(~ sector, wages,
         type = type_pie(position = 1.1, init.angle = 10,
                         radius = 1.3, hole = 0.3, pal = "Set3"))

## ----cummulative, fig.cap = "Figure 6: Cummulative distribution"----
tinyplot(~ children, data = rgp, type = type_cumul(expand = 0.1, max = 3))

## ----cummulativebis, eval = FALSE--------------------
# rc <- freq_table(rgp, children, f = "F", max = 3)
# tinyplot(F ~ children, data = rc, type = type_cumul(expand = 0.1))

## ----histogram, fig.cap = "Figure 7: Histogram"------
tinyplot(~ wage, data = wages,
         type = type_histo(breaks = c(10, 20, 30, 40, 50)))

## ----freqpoly, fig.cap = "Figure 8: Frequency polygons"----
tinyplot(~ wage, data = wages,
         type = type_freqpoly(breaks = c(10, 20, 30, 40, 50)))

## ----freqpolybis, eval = FALSE-----------------------
# ww <- freq_table(wages, wage, f = "d",
#                  breaks = c(10, 20, 30, 40, 50))
# tinyplot(d ~ wage, data = ww, type = type_freqpoly())

## ----cumm2, fig.cap = "Figure 9: Cummulative distribution"----
tinyplot(~ wage, data = wages, type = type_cumul(expand = 0.05))

## ----lorenz, fig.cap = "Figure 10: Lorenz curve"-----
tinyplot(~ wage, wages, type = type_lorenz(), col = "lightgrey")

## ----lorenzbis, eval = FALSE-------------------------
# ww <- freq_table(wages, wage, f = "FM")
# tinyplot(M~ F, ww, type = type_lorenz(), col = "lightgrey")

## ----wages2------------------------------------------
wages2 <- wages |>
    transform(size = bin(size, breaks = c(20, 50, 100)),
              wage = bin(wage, breaks = c(10, 30, 50)))

## ----------------------------------------------------
with(wages2, table(wage, size))

## ----------------------------------------------------
with(wages2, table(wage, size)) |> as.data.frame() |> head()

## ----cont_table--------------------------------------
wages2 |> cont_table(wage, size) |> head()

## ----cont_table_wide---------------------------------
wages2 |> cont_table(wage, size) |> wide()

## ----conttableplot, fig.cap = "Figure 11: Contingency table plot"----
tinyplot(wage ~ size, data = wages2,
         type = type_cont.table(size = c(0.5, 5)),
         pch = 16)

## ----distributions-----------------------------------
wht <- wages2 |> cont_table(size, wage)
wht |> joint() |> wide()
wht |> marginal(size)
wht |> conditional(size) |> wide()

## ----distributions_stat, collapse = TRUE-------------
wht |> joint() |> mean()
wht |> joint() |> stdev()
wht |> joint() |> variance()
wht |> joint() |> modval()

## ----marginal_mean, collapse = TRUE------------------
wht |> marginal(size) |> mean()

## ----marginal_mean_freq_table, collapse = TRUE-------
wages2 |> freq_table(size) |> mean()

## ----conditional_stat--------------------------------
wht |> conditional(wage) |> mean()
wht |> conditional(wage) |> variance()

## ----anova_computations------------------------------
cm <- wht |> conditional(wage) |> mean()
cv <- wht |> conditional(wage) |> variance()
md <- wht |> marginal(size)
z <- merge(md, cm) |> merge(cv)
om <- sum(z$f * z$mean)
ev <- sum(z$f * (z$mean - om) ^ 2)
rv <- sum(z$f * z$variance)
tv <- ev + rv
c(om = om, ev = ev, rv = rv, tv = tv)

## ----anova-------------------------------------------
wht_wage <- wht |> anova(wage)
wht_wage

## ----anova_summary-----------------------------------
wht_wage |> summary()

## ----anovaplot, fig.cap = "Figure 12: Anova plot"----
tinyplot(wage ~ size, data = wages2, type = type_anova())

## ----contingent_covariance, collapse = TRUE----------
wht |> joint() |> covariance()
wht |> joint() |> correlation()

## ----regline, collapse = TRUE------------------------
rl <- regline(wage ~ size, wht)
rl

## ----reglineplot, fig.cap = "Figure 13: Regression line"----
tinyplot(wage ~ size, data = wages2, type = type_cont.table(size = c(0.5, 5)),
         legend = FALSE, pch = 16)
a <- cont_table(wages2, wage, size)
cfs <- regline(wage ~ size, data = a)
tinyplot_add(type = type_abline(a = cfs[1], b = cfs[2]), col = "blue")

