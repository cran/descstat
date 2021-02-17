## ----setup, echo = FALSE-----------------------------
knitr::opts_chunk$set(message = FALSE, warning = FALSE,
                      out.width = '70%', fig.asp = 0.5,
                      fig.align = "center")
options(
    htmltools.dir.version = FALSE, formatR.indent = 2, width = 55, digits = 4,
    tibble.print_max = 5, tibble.print_min = 5)
otheme <- ggplot2::theme_set(ggplot2::theme_minimal())

## ----echo = FALSE, eval = FALSE----------------------
#  library("dplyr")
#  library("ggplot2")
#  library("purrr")
#  library("tidyr")
#  library("forcats")
#  library("tibble")
#  #library("descstat")
#  ra <- lapply(system("ls ~/YvesPro2/R_github/descstat/R/*.R", intern = TRUE), source)
#  ra <- system("ls ~/YvesPro2/R_github/descstat/data/*.rda", intern = TRUE)
#  for (i in ra) load(i)

## ----------------------------------------------------
library("descstat")
library("ggplot2")
library("dplyr")

## ----------------------------------------------------
z <- c(1, 5, 10, 12, 4, 9, 8)
bin1 <- cut(z, breaks = c(1, 8, 12), right = FALSE)
bin2 <- cut(z, breaks = c(1, 8, 12), right = TRUE)
bin3 <- cut(z, breaks = c(1, 8, 12, Inf), right = FALSE)
tibble(z, bin1, bin2, bin3)

## ----------------------------------------------------
bin3chr <- as.character(bin3)
bin3chr
factor(bin3chr)
sort(unique(bin3chr))

## ----------------------------------------------------
bin3chr  %>% extract

## ----------------------------------------------------
bin3chr %>% as_bin

## ----------------------------------------------------
bin4 <- c("[1,8)", "[1, 8)", "[8,12", "[12,inf)", "[1,8)",
          "[8,12)", "[8,12)")
bin4 %>% as_bin

## ----------------------------------------------------
bin3 %>% as_numeric

## ----------------------------------------------------
bin3 %>% as_numeric(pos = 1)
bin3 %>% as_numeric(pos = 0.5)

## ----------------------------------------------------
bin3 %>% as_numeric(pos = 0.5, wlast = 4)

## ----results = 'hide'--------------------------------
bin3 %>% as_numeric(pos = 0.5, xlast = 20)

## ----------------------------------------------------
bin3 %>% as_numeric(pos = 0.5, xlast = 20, xfirst = 6)

## ----------------------------------------------------
rgp %>% count(children)

## ----results = 'hide'--------------------------------
rgp %>% freq_table(children)

## ----------------------------------------------------
rgp %>% freq_table(children, "nfpNFP")

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
cld <- rgp %>% freq_table(children, f = "nf", max = 3)
cld %>% pre_print %>% ggplot(aes(children, f)) +
    geom_col(fill = "white", color = "black")

## ----------------------------------------------------
cld %>% pre_print %>% pre_plot("f", plot = "stacked")

## ----------------------------------------------------
bnp <- cld %>% pre_print %>% pre_plot("f", plot = "stacked") %>%
    ggplot(aes(x = 2, y = f, fill = children)) +
    geom_col() +
    geom_text(aes(y = ypos, label = children)) +
    scale_x_continuous(label = NULL) +
    scale_fill_brewer(palette = "Set3") +
    guides(fill = FALSE)
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
cld %>% pre_plot(plot = "cumulative") %>% ggplot() +
    geom_segment(aes(x = x, xend = xend, y = y, yend = yend,
                     linetype = pos)) +
    guides(linetype = FALSE) +
    labs(x = "number of children", y = "cumulative frequency")

## ----------------------------------------------------
wages %>% print(n = 3)

## ----------------------------------------------------
wages %>% freq_table(size) %>% print(n = Inf)

## ----------------------------------------------------
wages %>% freq_table(size, breaks = c(20, 250))

## ----------------------------------------------------
wages %>% freq_table(size, breaks = 50)

## ----------------------------------------------------
padova %>% pull(price) %>% range
padova %>% freq_table(price, breaks = c(250, 500, 750))
padova %>% freq_table(price, breaks = c(30, 250, 500, 750, 1000))

## ----------------------------------------------------
wages %>% freq_table(size, "dmM", breaks = c(20, 100, 250))

## ----------------------------------------------------
wages %>% freq_table(size, "p", vals = "xlua", breaks = c(20, 100, 250), wlast = 2)

## ----------------------------------------------------
padova %>% ggplot(aes(price)) +
    geom_histogram(aes(y = ..density..), color = "black", fill = "white") +
    geom_freqpoly(aes(y = ..density..), color = "red") +
    geom_density(color = "blue")

## ----------------------------------------------------
ftwage <- wages %>% freq_table(wage, "d", breaks = c(10, 20, 30, 40, 50))
ftwage %>% pre_plot(plot = "histogram") %>%
    ggplot(aes(x, y)) + geom_polygon(fill = "white", color = "black")
ftwage %>% pre_plot(plot = "freqpoly") %>%
    ggplot(aes(x, y)) + geom_line()

## ----------------------------------------------------
lzc <- wages %>% freq_table(wage, "MF", breaks = c(10, 20, 30, 40, 50)) %>%
    pre_plot(plot = "lorenz")
lzc

## ----------------------------------------------------
lzc %>% ggplot(aes(F, M)) +
    geom_polygon(fill = "lightyellow", color = "black") +
    geom_point(data = filter(lzc, pts)) +
    geom_line(data = tibble(F = c(0, 1), M = c(0, 1)), color = "blue") +
    geom_line(data = tibble(F = c(0, 1, 1), M = c(0, 0, 1)), color = "red")

## ----------------------------------------------------
income

## ----------------------------------------------------
income %>% freq_table(inc_class, freq = number)

## ----------------------------------------------------
income %>% freq_table(inc_class, freq = number, mass = tot_inc)

## ----echo = FALSE------------------------------------
tribble(~ "R", ~ descstat,
        "mean", "mean",
        "median", "median",
        "quantile", "quantile",
        "var", "variance",
        "sd", "stdev",
        "mad", "madev",
        "", "modval",
        "", "medial",
        "", "gini",
        "", "skewness",
        "", "kurtosis") %>%
    knitr::kable()

## ----collapse = TRUE---------------------------------
z <- wages %>% freq_table(wage)
z %>% mean
z %>% median
z %>% modval

## ----collapse = TRUE---------------------------------
z %>% stdev
z %>% variance
z %>% madev

## ----collapse = TRUE---------------------------------
z %>% quantile(probs = c(0.25, 0.5, 0.75))
z %>% quantile(y = "mass", probs = c(0.25, 0.5, 0.75))

## ----collapse = TRUE---------------------------------
z %>% median
z %>% medial

## ----------------------------------------------------
z %>% gini

## ----------------------------------------------------
z %>% skewness
z %>% kurtosis

## ----------------------------------------------------
wages %>% freq_table(sector) %>% modval

## ----------------------------------------------------
wages2 <- wages %>%
    mutate(size = cut(size, c(20, 50, 100)),
           wage = cut(wage, c(10, 30, 50)))

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
wages2 %>% cont_table(wage, size, total = TRUE) %>% print(row_name = FALSE)

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
wht %>% joint %>% modval

## ----------------------------------------------------
wht %>% marginal(size) %>% mean

## ----------------------------------------------------
wages2 %>% freq_table(size) %>% mean

## ----------------------------------------------------
wht %>% conditional(wage) %>% mean
wht %>% conditional(wage) %>% variance

## ----------------------------------------------------
cm <- wht %>% conditional(wage) %>% mean# %>% rename(mean = wage)
cv <- wht %>% conditional(wage) %>% variance# %>% rename(variance = wage)
md <- wht %>% marginal(size)
md %>% left_join(cm) %>% left_join(cv) %>%
    summarise(om = sum(f * mean),
              ev = sum(f * (mean - om) ^ 2),
              rv = sum(f * variance),
              tv = ev + rv) ->  ra


## ----------------------------------------------------
wht_wage <- wht %>% anova("wage")
wht_wage

## ----------------------------------------------------
wht_wage %>% summary

## ----------------------------------------------------
wht_wage %>% ggplot(aes(x, mean)) + geom_point() +
    geom_line(lty = "dotted") +
    geom_errorbar(aes(ymin = mean - sqrt(variance), ymax = mean + sqrt(variance))) +
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

