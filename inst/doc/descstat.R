## ----setup, echo = FALSE-----------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE,
                      out.width = '70%', fig.asp = 0.5,
                      fig.align = "center")
options(
    htmltools.dir.version = FALSE, formatR.indent = 2, width = 55, digits = 4,
    tibble.print_max = 5, tibble.print_min = 5)
otheme <- ggplot2::theme_set(ggplot2::theme_minimal())

## ----------------------------------------------------
library("ggplot2")
library("descstat")

## ----------------------------------------------------
rgp %>% count(children)

## ----------------------------------------------------
rgp %>% freq_table(children)

## ----------------------------------------------------
rgp %>% freq_table(children, cols = "nfpNFP")

## ----------------------------------------------------
rgp %>% freq_table(children, max = 3, total = TRUE)

## ----------------------------------------------------
rgp %>% freq_table(children, max = 3, total = TRUE) %>% str

## ----------------------------------------------------
descstat:::format.freq_table

## ----------------------------------------------------
rgp %>% freq_table(children, max = 3, total = TRUE) %>%
    pre_print %>% knitr::kable()

## ----------------------------------------------------
cld <- rgp %>% freq_table(children, cols = "nf", max = 3)
cld %>% pre_print %>% ggplot(aes(children, f)) +
    geom_col(fill = "white", color = "black")

## ----------------------------------------------------
cld %>% pre_print %>% pre_plot("f", plot = "banner")

## ----------------------------------------------------
bnp <- cld %>% pre_print %>% pre_plot("f", plot = "banner") %>%
    ggplot(aes(x = 2, y = f, fill = children)) +
    geom_col() +
    geom_text(aes(y = ypos, label = f)) +
    scale_x_continuous(label = NULL) +
    scale_fill_brewer(palette = "Set3")
bnp

## ----------------------------------------------------
bnp + coord_polar(theta = "y") + theme_void()

## ----------------------------------------------------
bnp + scale_x_continuous(limits = c(1, 2.5)) +
    coord_polar(theta = "y") + theme_void()

## ----------------------------------------------------
cld <- rgp %>% freq_table(children, "F", max = 5, total = TRUE)
cld %>% pre_plot(plot = "cumulative") %>% print(n = 5)

## ----------------------------------------------------
cld %>% pre_plot(plot = "cumulative") %>% 
    ggplot() +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend,
                     linetype = pos)) +
    guides(linetype = FALSE) +
    labs(x = "number of children", y = "cumulative frequency")

## ----------------------------------------------------
wages %>% print(n = 3)

## ----------------------------------------------------
wages %>% count(size)
wages %>% freq_table(size)

## ----------------------------------------------------
wages %>% bins_table(size, breaks = c(20, 250))

## ----------------------------------------------------
padova %>% pull(price) %>% range
padova %>% bins_table(price, breaks = c(250, 500, 750))
padova %>% bins_table(price, breaks = c(30, 250, 500, 750, 1000))

## ----------------------------------------------------
wages %>% bins_table(size, cols = "nFp", breaks = c(20, 100, 250))

## ----------------------------------------------------
wages %>% bins_table(size, cols = "dmM", breaks = c(20, 100, 250))

## ----------------------------------------------------
wages %>% bins_table(size, cols = "dMfnFp", breaks = c(20, 100, 250),
                     total = TRUE)

## ----------------------------------------------------
wages %>% bins_table(size, breaks = c(20, 100, 250), xfirst = 10)

## ----results = 'hide'--------------------------------
wages %>% bins_table(size, breaks = c(20, 100, 250), xlast = 400)

## ----------------------------------------------------
wages %>% bins_table(size, breaks = c(20, 100, 250), wlast = 2)

## ----------------------------------------------------
wages %>% bins_table(size, vals = "xlua", cols = "p", breaks = c(20, 100, 250), wlast = 2)

## ----------------------------------------------------
padova %>% pull(price) %>% head
padova %>% pull(price) %>%
    cut(breaks = c(0, 250, 500, 1000), right = FALSE) %>% head

## ----------------------------------------------------
wages %>% pull(wage) %>% levels %>% head
wages2 <- wages %>% mutate(wage = recut(wage, c(10, 20, 50)))
wages2 %>% pull(wage) %>% levels

## ----------------------------------------------------
wages2 %>% select(wage) %>%
    mutate(lb = cls2val(wage, 0),
           ub = cls2val(wage, 1),
           ct = cls2val(wage, 0.5, xfirst = 5, xlast = 100))

## ----------------------------------------------------
padova %>% ggplot(aes(price)) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
    geom_freqpoly(aes(y = ..density..), color = "red") + geom_density(color = "blue")

## ----------------------------------------------------
wages %>% bins_table(wage, "d", breaks = c(10, 20, 30, 40, 50)) %>%
    pre_plot(plot = "histogram") %>%
    ggplot(aes(x, y)) + geom_polygon(fill = "white", color = "black")
wages %>% bins_table(wage, "d", breaks = c(10, 20, 30, 40, 50)) %>%
    pre_plot(plot = "freqpoly") %>%
    ggplot(aes(x, y)) + geom_line()

## ----------------------------------------------------
lzc <- wages %>% bins_table(wage, "MF", breaks = c(10, 20, 30, 40, 50)) %>%
    pre_plot(plot = "lorenz")
lzc

## ----------------------------------------------------
lzc %>% ggplot(aes(F, M)) +
    geom_polygon(fill = "lightyellow", color = "black") +
    geom_point(data = filter(lzc, pts)) +
    geom_line(data = tibble(F = c(0, 1), M = c(0, 1)), color = "blue") +
    geom_line(data = tibble(F = c(0, 1, 1), M = c(0, 0, 1)), color = "red")

## ----echo = FALSE------------------------------------
tribble(~ "base R", ~ descstat,
        "mean", "mean",
        "median", "median",
        "quantile", "quantile",
        "var", "variance",
        "sd", "stdev",
        "mad", "madev",
        "", "modval",
        "", "medial",
        "", "gini") %>%
    knitr::kable()

## ----collapse = TRUE---------------------------------
z <- wages %>% bins_table(wage)
z %>% mean
z %>% median
z %>% modval

## ----collapse = TRUE---------------------------------
z %>% stdev
z %>% variance

## ----collapse = TRUE---------------------------------
z %>% quantile(probs = c(0.25, 0.5, 0.75))
z %>% quantile(y = "mass", probs = c(0.25, 0.5, 0.75))

## ----collapse = TRUE---------------------------------
z %>% median
z %>% medial

## ----------------------------------------------------
z %>% gini

## ----------------------------------------------------
wages2 <- wages %>%
    mutate(size = recut(size, c(20, 50, 100)),
           wage = recut(wage, c(10, 30, 50)))

## ----------------------------------------------------
wages2 %>% count(size, wage)

## ----------------------------------------------------
wages2 %>% count(size, wage) %>%
    tidyr::pivot_wider(values_from = n, names_from = size)

## ----------------------------------------------------
wages2 %>% cont_table(wage, size)

## ----------------------------------------------------
wages2 %>% cont_table(wage, size) %>%
    pre_print %>% knitr::kable()

## ----------------------------------------------------
wages2 %>% cont_table(wage, size, total = TRUE)

## ----------------------------------------------------
employment %>% cont_table(age, sex, weights = weights, total = TRUE)

## ----------------------------------------------------
wages2 %>% cont_table(size, wage) %>% pre_plot %>%
    ggplot() + geom_point(aes(size, wage, size = n))

## ----------------------------------------------------
wht <- wages2 %>% cont_table(size, wage)
wht %>% joint
wht %>% marginal(size)
wht %>% conditional(size)

## ----------------------------------------------------
wht %>% joint %>% mean
wht %>% joint %>% stdev
wht %>% joint %>% variance

## ----------------------------------------------------
wht %>% marginal(size) %>% mean
wht %>% marginal(size) %>% stdev

## ----------------------------------------------------
wages2 %>% bins_table(size) %>% mean

## ----------------------------------------------------
wht %>% conditional(wage) %>% mean
wht %>% conditional(wage) %>% variance

## ----------------------------------------------------
cm <- wht %>% conditional(wage) %>% mean %>% rename(mean = wage)
cv <- wht %>% conditional(wage) %>% variance %>% rename(variance = wage)
md <- wht %>% marginal(size)
md %>% left_join(cm) %>% left_join(cv) %>%
    summarise(om = mean(mean),
              ev = sum(f * (mean - om) ^ 2),
              rv = sum(f * variance),
              tv = ev + rv)                       

## ----------------------------------------------------
wht_wage <- wht %>% var_decomp("wage")
wht_wage

## ----------------------------------------------------
wht_wage %>% summary

## ----------------------------------------------------
wht_wage %>% ggplot(aes(size_val, mean)) + geom_point() +
    geom_line(lty = "dotted") +
    geom_errorbar(aes(ymin = mean - sqrt(var), ymax = mean + sqrt(var))) +
    labs(x = "size", y = "wage")    

## ----------------------------------------------------
wht %>% joint %>% covariance
wht %>% joint %>% correlation

## ----------------------------------------------------
rl <- regline(wage ~ size, wht)
rl

## ----------------------------------------------------
wht %>% pre_plot %>% ggplot() + geom_point(aes(size, wage, size = n)) +
    geom_abline(intercept = rl[1], slope = rl[2])

