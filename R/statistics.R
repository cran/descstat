#' Functions to compute statistics on univariate distributions
#'
#' **descstat** provide functions to compute statistics on an
#' univariate distribution. This includes central tendency,
#' dispersion, shape and concentration.
#'
#' The following functions are provided:
#' 
#' - central tendency: `mean`, `median`, `medial`, `modval` (for the mode),
#' - dispersion: `variance`, `stdev`, `maddev` (for mean absolute
#' deviation) and quantile,
#' - shape: `skewness` and `kurtosis`,
#' - concentration: `gini`.
#'
#' When a generic function exists in base **R** (or in the `stats`
#' package), methods are provided for `freq_table` or `cont_table`,
#' this is a case for `mean`, `median` and `quantile`. When a function
#' exists, but is not generic, we provide a generic and relevant
#' methods using different names (`stdev`, `variance` and `madev`
#' instead respectively of `sd`, `var` and `mad`). Finally some
#' function don't exist in base **R** and recommended packages, we
#' therefore provide a `modval` function to compute the mode, `gini`
#' for the Gini concentration index, `skewness` and `kurtosis` for
#' Fisher's shape statistics and `gmean` for generalized means (which
#' include the geometric, the quadratic and the harmonic means).
#'
#' `madev` has a center argument which indicates whether the
#' deviations should be computed respective to the mean or to the
#' median.
#'
#' `gmean` has a `r` argument: values of -1, 0, 1 and 2 lead
#' respectively to the harmonic, geometric, arithmetic and quadratic
#' means.
#' 
#' @name univariate
#' @param x a series or a `freq_table` or a `cont_table` object,
#' @param center the center value used to compute the mean absolute
#'     deviations, one of `"median"` or `"mean"`,
#' @param w a vector of weights,
#' @param y for the quantile method, one of `"value"` or `"mass"`,
#' @param r the order of the mean for the `gmean` function,
#' @param probs the probabilities for which the quantiles have to be
#'     computed.
#' @param ... further arguments,
#' @return a numeric or a tibble.
#' @importFrom stats weighted.mean quantile median na.omit setNames
#'     reshape anova
#' @export
#' @author Yves Croissant
#' @keywords univar
#' @examples
#' z <- wages |> freq_table(wage)
#' z |> median()
#' # the medial is the 0.5 quantile of the mass of the distribution
#' z |> medial()
#' # the modval function returns the mode, it is a one line tibble
#' z |> modval()
#' z |> quantile(probs = c(0.25, 0.5, 0.75))
#' # quantiles can compute for the frequency (the default) or the mass
#' # of the series
#' z |> quantile(y = "mass", probs = c(0.25, 0.5, 0.75))
variance <- function(x, ...)
    UseMethod("variance")

#' @rdname univariate
#' @export
gmean <- function(x, r = 1, ...)
    UseMethod("gmean")

#' @rdname univariate
#' @export
gini <- function(x, ...)
    UseMethod("gini")

#' @rdname univariate
#' @export
stdev <- function(x, ...)
    UseMethod("stdev")

#' @rdname univariate
#' @export
madev <- function(x, ...)
    UseMethod("madev")

#' @rdname univariate
#' @export
modval <- function(x, ...)
    UseMethod("modval")

#' @rdname univariate
#' @export
medial <- function(x, ...)
    UseMethod("medial")

#' @rdname univariate
#' @export
kurtosis <- function(x, ...)
    UseMethod("kurtosis")

#' @rdname univariate
#' @export
skewness <- function(x, ...)
    UseMethod("skewness")

# Default method

#' @rdname univariate
#' @export
variance.default <- function(x, w = NULL, ...){
    xb <- weighted.mean(x, w)
    weighted.mean((x - xb) ^ 2, w)
}

#' @rdname univariate
#' @export
gmean.default <- function(x, r = 1, ...){
    if (r == 0) exp(mean(log(x)))
    else mean(x ^ r) ^ (1 / r)
}

#' @rdname univariate
#' @export
stdev.default <- function(x, w = NULL, ...)
    sqrt(variance(x = x, w = w, ...))


#' @rdname univariate
#' @export
madev.default <- function(x, w = NULL, center = c("median", "mean"), ...){
    center <- match.arg(center)
    if (is.null(w)) stop("w should be indicated")
    if (center == "median") ctr <-  median(x = x, w = w)
    if (center == "mean") ctr <- weighted.mean(x, w)
    weighted.mean(abs(x - ctr), w)
}

#' @rdname univariate
#' @export
skewness.default <- function(x, ...){
    N <- length(x)
    xb <- mean(x)
    sd <- sd(x) * sqrt((N - 1) / N)
    x3 <- mean((x - xb) ^ 3)
    x3 / sd ^ 3
}

#' @rdname univariate
#' @export
kurtosis.default <- function(x, ...){
    N <- length(x)
    xb <- mean(x)
    sd <- sd(x) * sqrt((N - 1) / N)
    x4 <- mean((x - xb) ^ 4)
    x4 / sd ^ 4 - 3
}

#' @rdname univariate
#' @export
mean.freq_table <- function(x, ...){
    x <- x |> na.omit()
    x <- compute_freq_if_necessary(x)
    weighted.mean(get_numval(x), x$f)
}

#' @rdname univariate
#' @export
gmean.freq_table <- function(x, r = 1, ...){
    x <- x |> na.omit()
    x <- compute_freq_if_necessary(x)
    if (r  == 0) exp(weighted.mean(log(get_numval(x)), x$f))
    else weighted.mean(get_numval(x) ^ r, x$f) ^ (1 / r)
}

#' @rdname univariate
#' @export
variance.freq_table <- function(x, ...){
    x <- x |> na.omit()
    x <- compute_freq_if_necessary(x)
    variance(get_numval(x), x$f)
}

#' @rdname univariate
#' @export
stdev.freq_table <- function(x, ...)
    x |> variance() |> sqrt()

#' @rdname univariate
#' @export
skewness.freq_table <- function(x, ...){
    x <- compute_freq_if_necessary(x)
    xb <- mean(get_numval(x))
    sd <- sd(get_numval(x))
    mu3 <- weighted.mean((get_numval(x) - xb) ^ 3, x$f)
    mu3 /sd ^ 3
}

#' @rdname univariate
#' @export
kurtosis.freq_table <- function(x, ...){
    x <- compute_freq_if_necessary(x)
    xb <- mean(get_numval(x))
    sd <- sd(get_numval(x))
    mu4 <- weighted.mean((get_numval(x) - xb) ^ 4, x$f)
    mu4 /sd ^ 4 - 3
}

#' @rdname univariate
#' @export
madev.freq_table <- function(x, center = c("median", "mean"), ...){
    x <- x |> na.omit()
    center <- match.arg(center)
    if (center == "median") center <- median(x)
    else center <- mean(x)
    x <- compute_freq_if_necessary(x)
    weighted.mean(abs(get_numval(x) - center), x$f)
}

#' @rdname univariate
#' @export
modval.freq_table <- function(x, ...){
    if (! series_type(x[[1]]) %in% c("continuous", "contbin")){
        f <- compute_freq(x)
        pos <- which.max(f)
        f_max <- x[[2]][pos]
        x_max <- x[[1]][pos]
#        result <- tibble(x_max, f_max) |> setNames(c(names(x)))
        result <- data.frame(x_max, f_max) |> setNames(c(names(x)))
    }
    else{
        # the series is a bin, compute the density and get the
        # highest
        d <- compute_dens(x)
        pos <- which.max(d)
        result <- x[pos, , drop =FALSE]
    }
    result
}
        
#' @rdname univariate
#' @export
quantile.freq_table <- function(x, y = c("value", "mass"), probs = c(0.25, 0.5, 0.75), ...){
    x_is_char <- is.character(x[[1]])
    x <- x |> na.omit()
    if (! is.numeric(x[[1]])){
        bds <- extract_bin(x[[1]])[c("first", "last")]
        names(bds) <- c("low", "up")
    }
    y <- match.arg(y)
    x <- compute_freq_if_necessary(x)
    
    if (y == "mass"){
        if (! "m" %in% names(x)){
            x$m <- x$f * get_numval(x)
            x$m <- x$m / sum(x$m)
        }
        y <- x$m
    }
    else y <- x$f

    x <- cbind(x[1:2], y = cumsum(y))
    if (x_is_char) x <- cbind(x, bds)
    aquantile <- function(p){
        if (is.character(x[[1]])){
            I <- which(x$y > p)[1]
            alpha <- (x$y[I] - p) / (x$y[I] - ifelse(I == 1, 0, x$y[I - 1]))
            q <- alpha * x$low[I] + (1 - alpha) * x$up[I]
        }
        else{
            I <- which(x$y > p)[1]
            q <- x[[1]][I]
        }
        q
    }
    sapply(probs, aquantile)
}

#' @rdname univariate
#' @export
median.freq_table <- function(x, ..., y = c("value", "mass")){
    y <- match.arg(y)
    quantile(x, y = y, 0.5)
}

#' @rdname univariate
#' @export
medial.freq_table <- function(x, ...){
    quantile(x, y = "mass", probs = 0.5, ...)
}

#' @rdname univariate
#' @importFrom stats lag
#' @export
gini.freq_table <- function(x, ...){
    x <- x |> na.omit()
    if (! inherits(x, "freq_table")) stop("x should be a freq_table object")
    x_is_char <- is.character(x[[1]])
    if (any(! c("F", "M") %in% names(x))){
        x$f <- compute_freq(x)
        if (! "F" %in% names(x)) x$F <- cumsum(x$f)
        if (! "M" %in% names(x)){
            x$m <- x$f * x$x
            x$m <- x$m / sum(x$m)
            x$M <- cumsum(x$m)
        }
    }
    x[[1]] <- unclass(x[[1]])
    x <- x[c("F", "M")]
    x <- rbind(data.frame(F = 0, M = 0), x)
    N <- nrow(x)
    tz <- (x$F[2:N] - x$F[1:(N - 1)]) * (x$M[2:N] + x$M[1:(N - 1)]) / 2
    g <- 2 * (0.5 -  sum(tz, na.rm = TRUE))
    g
}

#' Functions to compute statistics on bivariate distributions
#'
#' These functions are intended to compute from a `cont_table` objects
#' covariation statistics, ie the covariance, the correlation
#' coefficient, variance decomposition and regression line.
#'
#' @name bivariate
#' @param data,object a `cont_table` object,
#' @param x the series for which the analyse of variance should be
#'     computed,
#' @param formula symbolic description of the model,
#' @param ... further arguments.
#' @return a numeric or a tibble
#' @author Yves Croissant
#' @keywords bivariate
#' @export
#' @examples
#' # the covariance and the linear correlation coefficient are
#' # computed using only the `cont_table`
#' # First reduce the number of bins
#' wages2 <- wages
#' wages2$size <- cut(wages2$size, breaks = c(20, 50, 100))
#' wages2$wage <- cut(wages$wage, breaks = c(10, 30, 50))
#' wages2 |> cont_table(wage, size) |> covariance()
#' wages2 |> cont_table(wage, size) |> correlation()
#' # For the analyse of variance, one of the two series should be
#' # indicated
#' wages2 |> cont_table(wage, size) |> anova(wage)
#' wages2 |> cont_table(wage, size) |> anova(wage) |> summary()
#' # For the regression line, a formula should be provided
#' wages2 |> cont_table(wage, size) |> regline(formula = wage ~ size)
covariance <- function(data, ...)
    UseMethod("covariance")

#' @rdname bivariate
#' @export
correlation <- function(data, ...)
    UseMethod("correlation")

#' @rdname bivariate
#' @export
covariance.cont_table <- function(data, ...){
    limits <- attr(data, "limits")
    data <- total.omit(data)
    x1 <- sort(unique(data[[1]])) |>
        as_numeric(0.5, xfirst  = limits[[1]]$xfirst,
                   xlast = limits[[1]]$xlast,
                   wlast = limits[[1]]$wlast)
    x2 <- sort(unique(data[[2]])) |>
        as_numeric(0.5, xfirst  = limits[[1]]$xfirst,
                   xlast = limits[[1]]$xlast,
                   wlast = limits[[1]]$wlast)
    x1b <- mean(x1)
    x2b <- mean(x2)
    x12 <- outer(x1 - x1b, x2-x2b)
#    data <- as_matrix(data)
    data <- matrix(data[[3]], nrow = nrow(x12))
    sum(x12 * data / sum(data))
}

#' @rdname bivariate
#' @export
correlation.cont_table <- function(data, ...){
    sdevs <- stdev(data)
    covar <- covariance(data)
    covar / sdevs[1, 1, drop = TRUE] / sdevs[1, 2, drop = TRUE]
}

#' @rdname bivariate
#' @export
anova.cont_table <- function(object, x, ...){
    data <- object
    x_name <- paste(substitute(x))
    if (! x_name %in% names(data)){
        if (is.numeric(x)) x_name <- names(data)[x]
    }
    cond <- setdiff(names(data)[1:2], x_name)
    cond_pos <- match(cond, names(data)[1:2])
    m_x <- data |> conditional(x_name, x_is_char = TRUE) |> mean()
    s2_x <- data |> conditional(x_name, x_is_char = TRUE) |> variance()
    f_y <- data |> marginal(cond, x_is_char = TRUE)
    data <- merge(f_y, m_x, by = cond, sort = FALSE)
    data <- merge(data, s2_x, by = cond, sort = FALSE)
    structure(data, class = c("anova.cont_table", class(data)))
}

#' @rdname bivariate
#' @export
summary.anova.cont_table <- function(object, ...){
    .gmean <- sum(object$mean * object$f)
    .inter <- sum( (object$mean - .gmean) ^ 2 * object$f)
    .intra <- sum(object$variance * object$f)
    .total <- .inter + .intra
    .ratio <- .inter / .total
    result <- data.frame(inter = .inter, intra = .intra, total = .total, ratio = .ratio)
    class(result) <- c("tbl_df", "tbl", class(result))
    result
}

#' @rdname bivariate
#' @export
regline <- function(formula, data){
    if (! inherits(data, "cont_table")) stop("regline only suitable for cont_table data")
    formula <- as.list(formula)
    y <- paste(deparse(formula[[2]]))
    x <- paste(deparse(formula[[3]]))
    if (! y %in% names(data)[1:2]) stop(paste(y, "doesn't exist"))
    if (! x %in% names(data)[1:2]) stop(paste(x, "doesn't exist"))
    c_xy <- data |> joint() |> covariance()
    m_x <- data |> marginal(x, x_is_char = TRUE) |> mean()
    m_y <- data |> marginal(y, x_is_char = TRUE) |> mean()
    v_x <- data |> marginal(x, x_is_char = TRUE) |> variance()
    slope <- c_xy / v_x
    intercept <- m_y - slope * m_x
    c(intercept, slope)
}

    
# Cont_table methods    

fun.cont_table <- function(data, fun = weighted.mean, center = "median", ...){
    type_1 <- series_type(data[[1]])
    type_2 <- series_type(data[[2]])
    x_char <- attr(data, "x")
    fun_name <- deparse(substitute(fun))
    if (is.null(x_char)){
        # joint distribution, compute the marginal, the required
        # statistics and put the result in a tibble
        if (! fun_name %in% c("covariance", "correlation")){
            #joint distribution, just get the two marginal distributions,
            #apply the function and return a one line tibble
            marg_1 <- marginal(data, 1)
            marg_2 <- marginal(data, 2)
            cats <- names(data)[1:2][which(c(type_1, type_2) == "cat")]
            if (length(cats) & fun_name != "modval"){
                if (length(cats) == 1)
                    stop(paste("the computation of the", fun_name, "is irrelevant for",
                               cats, "which is a categorial series"))
                else
                    stop(paste("the computation of the", fun_name, "is irrelevant for",
                               paste(cats, collapse = " and "), "which are a categorial series"))
            }
            x1 <- fun(marg_1, center = center)
            x2 <- fun(marg_2, center = center)
            if (fun_name != "modval") data <- data.frame(x1, x2) |> setNames(names(data)[1:2])
            else{
                series <- c(names(x1)[1], names(x2)[1])
                names(x1)[1] <- names(x2)[1] <- "value"
                x1[[1]] <- as.character(x1[[1]])
                x2[[1]] <- as.character(x2[[1]])
                data <- rbind(as.data.frame(x1), as.data.frame(x2))
                data <- cbind(series = series, data)
            }
        }
    }
    else{
        # conditional distribution
        cond_name <- setdiff(names(data)[1:2], x_char)
        levs <- levels(data[[cond_name]])
        data <- split(data, data[[cond_name]])
        data <- lapply(data,
                       function(z){
                           z <- z[- match(cond_name, names(z))]
                           z <- freq_table(data = z, x = x_char, freq = "f", x_is_char = TRUE)
                           fun(z)
                       }
                       ) |>  as.numeric()
#        data <- tibble(levs, data) |> setNames(c(cond_name, fun_name))
        data <- data.frame(levs, data) |> setNames(c(cond_name, fun_name))
    }
    data
}

#' @rdname univariate
#' @export
modval.cont_table <- function(x, ...){
    fun.cont_table(x, fun = modval, ...)
}

#' @rdname univariate
#' @export
gini.cont_table <- function(x, ...){
    fun.cont_table(x, fun = gini, ...)
}

#' @rdname univariate
#' @export
skewness.cont_table <- function(x, ...)
    fun.cont_table(x, fun = skewness, ...)

#' @rdname univariate
#' @export
kurtosis.cont_table <- function(x, ...)
    fun.cont_table(x, fun = kurtosis, ...)

#' @rdname univariate
#' @export
madev.cont_table <- function(x, center = c("median", "mean"), ...){
    center <- match.arg(center)
    fun.cont_table(x, fun = madev, center = center, ...)
}

#' @rdname univariate
#' @export
mean.cont_table <- function(x, ...)
    fun.cont_table(x, fun = mean, ...)

#' @rdname univariate
#' @export
variance.cont_table <- function(x, ...)
    fun.cont_table(x, fun = variance, ...)

#' @rdname univariate
#' @export
stdev.cont_table <- function(x, ...)
    fun.cont_table(x, fun = stdev, ...)


