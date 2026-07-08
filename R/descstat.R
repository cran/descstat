#' descstat: a toolbox for descriptive statistics
#'
#'
#' Descriptive statistics consist on presenting the distribution of
#' series for a sample in tables (frequency table for one series,
#' contingency tables for two series), ploting this distribution and
#' computing some statistics that summarise it. **descstat** provides
#' a complete toolbox to perform this tasks. It has been writen using
#' the tidyverse conventions, especially the selection of series using
#' their unquoted names and the use of the pipe operator.
#'
#' @section The bin class:
#'
#' The `bin` function takes a series as input that can either:
#' - a continouis variable,
#' - a categorical series,
#' - an integer series,
#' - a continuous bin series
#'
#' and returns an object of class `bin`. The `bin` function is
#' particularly usefull for continuous series. It enables:
#'
#' - creating bins from numerical values, which is performed by the
#' `base::cut` function which turns a numerical series to a bin,
#' - coercing bins to numerical values, eg getting from the `[10,20)`
#' bin the lower bound (10), the upper bound (20), the center (15) or
#' whatever other value of the bin,
#' - reducing the number of bins by merging some of them (for example
#' `[0,10)`, `[10, 20)`, `[20,30)`, `[30,Inf)` to `[0,20)`, `[20,Inf)`
#'
#' 
#' The last task is performed using the `as_numeric`
#' function. Coercing bins to their center values is the basis of the
#' computation of descripting statistics for bins.
#'
#'
#' @section Frequency and contingency tables:
#'
#' The `freq_table` and `cont_table` are based on the `dplyr::count`
#' function but offer a much richer interface and performs easily
#' usual operations which are tedious to obtain with `dplyr::count` or
#' `base::table` functions. This includes:
#'
#' - adding a total,
#' - for frequency tables, computing other kind of frequencies than
#' the counts, for example relative frequencies, percentage,
#' cummulative frequencies, etc.,
#' - for contingency tables, computing easily the joint, marginal and
#' conditional distributions,
#' - printing easily the contingency table as a double entry table.
#'
#' @section Plotting the distribution:
#'
#' **descstat** uses the **tinyplot** package to provide classic plots
#' for univariate or bivariate distributions. This includes histogram,
#' frequency plot, pie chart, cummulative plot and Lorenz curve. More
#' precisely, `type_dsc_###` functions are provided and the relevant
#' plot is obtained using `tinyplot(..., type = type_dsc_###)`.
#'
#' @section Descriptive statistics:
#'
#' A full set of statistical functions (of central tendency,
#' dispersion, shape, concentration and covariation) are provided and
#' can be applied directly on objects of class `freq_table` or
#' `cont_table`. Some of them are methods of generics defined by the
#' `base` or `stats` package, some other are defined as methods for
#' generics function provided by the **descstat** function when the
#' corresponding **R** function is not generic. For example,
#'
#' - `mean` is generic, so that we wrote a
#' `mean.freq_table` method to compute directly the mean of a series
#' from a frequency table.
#'
#' - `var` is not generic, so that we provide the `variance` generic
#' and a method for `freq_table` objects.
#' 
#' @name descstat-package
#' @keywords package
"_PACKAGE"


compute_widths <- function(x, xlast = NULL, xfirst = NULL, wlast = NULL){
    xu <- as_numeric(x, 1L, xlast = xlast, xfirst = xfirst, wlast = wlast)
    xl <- as_numeric(x, 0L, xlast = xlast, xfirst = xfirst, wlast = wlast)
    xu - xl
}

compute_freq <- function(x){
    if (! inherits(x, "freq_table")) stop("x should be an freq_table object")
    if (! "f" %in% names(x)){
        if (any(c("f", "p", "n") %in% names(x))){
            col <- na.omit(match(c("f", "p", "n"), names(x)))[1]
            f <- x[[col]]
            f <- f / sum(f)
        }
        else{
            if (any(c("F", "P", "N") %in% names(x))){
                col <- na.omit(match(c("F", "P", "N"), names(x)))[1]
                f <- x[[col]]
                f <- c(f[1], f[-1] - f[- length(f)])
                f <- f / sum(f)
            }
            else stop("the table should contain any of f, p, n, F, P, N")
        }
        f
    }
    else x$f
}

compute_freq_if_necessary <- function(x){
    if (! "f" %in% names(x))
        x$f <- compute_freq(x)
    x
}

compute_dens <- function(x, xlast = NULL, xfirst = NULL, wlast = NULL){
    if (! inherits(x, "freq_table")) stop("x should be an freq_table object")
    if (! "d" %in% names(x)){
        f <- compute_freq(x)
        a <- compute_widths(x[[1]], xlast = xlast, xfirst = xfirst, wlast = NULL)
        d <- f /a
        d
    }
    else x$d
}

total.omit <- function(x) x[ ! (is.na(x[[1]]) | is.na(x[[2]])) &
                             (x[[1]] != "Total") & (x[[2]] != "Total"), ]

# get the numerical series, which is either the first column if it is
# numeric or the one called x
get_numval <- function(x){
    x <- x |> na.omit()
    if (is.numeric(x[[1]])) x[[1]]
    else x$x
}

series_type <- function(x){
    if (is.numeric(x)){
        if (looks_integer(x) %in% c("looks_integer", "is_integer")) type <- "integer"
        else type <- "continuous"
    }
    else{
        if (! all(is.na(extract_bin(x)$left))) type <- "contbin"
        else type <- "categorical"
    }
    type
}

looks_integer <- function(x){
    if (is.null(x)) stop("the series doesn't exist")
    if (! is.numeric(x)) result <- "not_numeric"
    else {
        if (is.integer(x)) result <- "is_integer"
        else{
            rx <- floor(x)
            if (any(abs(x - rx) > sqrt(.Machine$double.eps))) result <- "double"
            else{
#                if (length(unique(x)) > length(x) / 5) result <- "numerous_values"
                if (length(unique(x)) > 50) result <- "numerous_values"
                else result <- "looks_integer"
            }
        }
    }
    result
}
            
series_type <- function(x){
    if (is.numeric(x)){
        if (looks_integer(x) %in% c("looks_integer", "is_integer")) type <- "integer"
        else type <- "continuous"
    }
    else{
        if (! all(is.na(extract_bin(x)$left))) type <- "contbin"
        else type <- "categorical"
    }
    type
}

get_power <- function(x){
    nc <- nchar(as.character(x))
    lc <- as.character(x)[1]
    10 ^ ifelse(lc >= 5, nc, nc - 1)
}

get_breaks <- function(x, q = 0.1){
    pw <- get_power(median(x))
    qi <- floor(quantile(x, q) / pw)
    qf <- ceiling(quantile(x, 1 - q) / pw)
    seq(qi, qf, 1) * pw
}

fctcollapse <- function(x, fct3){
    if (is.factor(x)) x <- as.character(x)
    news <- names(fct3)
    for (i in 1:length(fct3)){
        x[x %in% fct3[[i]]] <- news[i]
    }
    factor(x, levels = news)
 }

.onLoad = function(libname, pkgname){
    opts = c(descstat.locale = "en")
    eval(parse(text = paste0("options(", "descstat.locale", " = ", "'en'", ")")))
    invisible()
}

## pre_print <- function(x, ...)
##     UseMethod("pre_print")

## #' @rdname print_methods
## #' @export
## pre_print.cont_table <- function(x, ..., row_name = TRUE, total_name = "Total"){
##     if (length(x) == 3){
##         if (row_name) names_x <- paste(names(x)[1], "|", names(x)[2], sep = "")
##         else names_x <- " "
##         names(x)[1] <- names_x
##         # the ordering of the columns may be problematic if x[[2]] is
##         # a factor, so get the levels
##         if (is.factor(x[[2]])) levs <- levels(x[[2]]) else levs <- NULL
##         oclass <- class(x)
##         Data <- as.data.frame(x)
##         .levs <- levels(Data[[2]])
##         if (any(is.na(Data[[1]]))){
##             .total <- TRUE
##             if (is.factor(Data[[1]])) levels(Data[[1]]) <- c(levels(Data[[1]]), total_name)
##             Data[[1]][is.na(Data[[1]])] <- total_name
##         } else .total <- FALSE
##         if (any(is.na(Data[[2]]))){
##             if (is.factor(Data[[2]])) levels(Data[[2]]) <- c(levels(Data[[2]]), total_name)
##             Data[[2]][is.na(Data[[2]])] <- total_name
##         }
##         x <- reshape(Data,
##                         idvar = names(Data)[1],
##                         timevar = names(Data)[2],
##                         v.names = names(Data)[3],
##                      direction = "wide")
##         names(x)[2:(length(.levs) + 1)] <- .levs
##         if (.total) names(x)[length(.levs) + 2] <- total_name
##     }
## #    class(x) <- c("cont_table", "tbl_df", "tbl", "data.frame")
##     class(x) <- c("cont_table", "data.frame")
##     x
## }

## format.cont_table <- function(x, ..., n = NULL, width = NULL, n_extra = NULL, row_name = TRUE, total_name = "Total"){
##     x <- pre_print(x, row_name = row_name, total_name = total_name)
##     cat("KE SUIS LA\n")
##     class(x) <- setdiff(class(x), "cont_table")
##     format(x, ..., n = n, width = width, n_extra = n_extra)
## }    


get_name <- function(y, nms = NULL){
#    if (class(y) == "NULL") y <- NULL
#    if (class(y) == "name") y <- deparse(y)
    if (inherits(y, "name")) y <- deparse(y)
    if (inherits(y, "NULL")) y <- NULL
    if (! is.null(y)){
        if (! is.null(nms)){
            if (is.numeric(y)){
                if (y <= length(nms)){
                    y <- nms[y]
                } else {
                    stop("the integer is greater than the length of the names")
                }
            }
            if (is.character(y)){
                if (! y %in% nms){
                    stop("the name doesn't exist")
                }        
            }
        }
    }
    y
}
