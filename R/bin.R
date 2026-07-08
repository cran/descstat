#' Bin series
#'
#' A new class called `bin` is provided ; a `bin` is a series that
#' contain a relatively few distinct values, that can be used to
#' compute a frequency table. The input series can be either :
#' - an `integer` series,
#' - a `contbin` series (a series that contain values as `[20, 40)`,
#' - a categorical series,
#' - a continuous numerical series.
#'
#' The `bin` function takes a series as input and returns a series of
#' class `bin`. It has a `type` attribute that can be either equal to
#' `integer`, `contbin` or `categorical`, and performs the following
#' tasks:
#'
#' - for categorical series, the levels can be defined, renamed and
#' collapsed using the `level` argument, - for integer series, a >= J
#' series can be computed by setting the `max` argument to `J`, so
#' that all the values greater or equal than J are collapsed and the
#' J, J+1, J+2, ... values are replaced by the mean of the values
#' greater or equal than J, - numerical bin series are kept as is, or
#' the number of classes can be reduced using the `breaks` argument, -
#' continuous numerical series are transfomed in numerical bin series,
#' using the `breaks` argument (if `NULL`, the breaks are
#' automatically computed) and the `right` argument can be set to
#' `TRUE` (the intervals are closed on the right, the default), or to
#' `FALSE` (the intervals are closed on the left).
#' @name bin
#' @param x a character or a factor: the first and last characters
#'     should be any of `[`, `(`, `]`, `)` and the other characters
#'     should be interpreted as two numerical values separated by a
#'     comma,
#' @param breaks a numerical vector of breaks which should be a subset
#'     of the initial set of breaks. If only one break is provided,
#'     all the bins with greater values are merged,
#' @param max an integer, to create a >= J bin for integer series
#' @param levels a character or a list to define, rename or collapse
#'     the levels for a categorical series
#' @param right for numerical series, should the interval be closed on
#'     the right ?
#' @param ... further arguments
#' @return a series of class `bin` with a `type attribute
#' @author Yves Croissant
#' @keywords classes
#' @examples
#' # get a few values of the `size` series of the `wages` data set
#' z <- head(wages$size, 10)
#' bin(z)
#' # reduce the number of bins
#' bin(z, breaks = c(20, 50, 100))
#' # set the right value of the last interval to 50
#' bin(z, breaks = 50)
#' # the `children` series of the `rgp` data set
#' z <- rgp$children
#' bin(z) |> head(20)
#' # set a >= 3 cathegory
#' bin(z, max = 3) |> head(20)
#' # the `sector` series of the `wages` data set contains a factor
#' # with levels `industry`, `building`, `business`, `services` and
#' # `administration`
#' z <- head(wages$sector)
#' bin(z)
#' # change the order of the levels:
#' bin(z, levels = c("business", "services", "administration", "building", "industry"))
#' # rename some levels
#' bin(z, levels = c("business", "services", government = "administration",
#'                   construction = "building", "industry"))
#' # collapse some levels
#' bin(z, levels = list(blue = c("building", "industry"),
#'                      white = c("business", "services"),
#'                      government = "administration"))
#' @export
# utilise :
# - series_type pour déduire le type de série en input,
# - get_breaks pour calculer automatiquement les césurse,
# - cut_contbin pour créer de nouvelles classes à partir d'anciennes
# - avec un argument break
bin <- function(x, breaks = NULL, max = NULL, levels = NULL, right = TRUE,
                xlast = NULL, wlast = NULL, xfirst = NULL){
    .type <- series_type(x)
    if (.type == "continuous"){
        if (is.null(breaks)){
            breaks <- get_breaks(x)
        }
        # x is numeric, cut it according to the break and the right
        # argument and then count. right = TRUE is the default value
        # of cut, so keep it at is
        # if the max value of break is lower than the maximum
        # value of x, add Inf to the vectors of breaks
        if (max(breaks) < max(x, na.rm = TRUE)) breaks <- c(breaks, Inf)
             # if the min value of break is greater than the
             # minimum value of x, add either 0 (if min(x) >= 0) or
             # -Inf to the vector of breaks
        if (min(breaks) > min(x, na.rm = TRUE))
            breaks <- c(ifelse(min(x, na.rm = TRUE) < 0, - Inf, 0), breaks)
        x <- cut(x, breaks = breaks, right = right)
    }
    if (.type == "contbin"){
        if (! is.null(breaks)){
            x <- cut_contbin(x, breaks = breaks, right = right)
        }
    }
    if (.type == "integer"){
        if (! is.null(max)){
            x2 <- mean(x[x >= max])
            x[x >= max] <- x2
        }
    }
    if (.type == "categorical"){
        if (! is.null(levels)){
            if (is.list(levels)) x <- fctcollapse(x, levels)
            else {
                if (! is.character(levels))
                    stop("levels should be either a list or a character")
                if (is.factor(x)){
                    old_levels <- levels(x)
                    x <- as.character(x)
                } else {
                    old_levels <- sort(unique(x))
                }
                nms <- names(levels)
                new_levels <- unname(levels)
                nms[nchar(nms) == 0] <- new_levels[nchar(nms) == 0]
                if (is.null(nms)){
                    x <- factor(x, levels = new_levels)
                } else {
                    x <- factor(x, levels = new_levels, labels = nms)
                }
            }
        } else {
            if (! is.factor(x)){
                x <- factor(x)
            }
        }
    }

    if (.type == "continuous") .type <- "contbin"

    # if xlast/wlast/xfirst are provided, save them as an attribute
    if (.type != "contbin"){
        structure(x, class = c("bin", class(x)), type = .type)
    } else {
        structure(x, class = c("bin", class(x)), type = .type,
                  xlast = xlast, wlast = wlast, xfirst = xfirst)
    }
}

#' Coerce a numeric bin in a numerical value
#' 
#' The `as_numeric` function is relevant for numerical bins. It
#' converts a numerical bin (an interval) to a value of the underlying
#' variable defined by its relative position (from 0 lower bound to 1
#' upper bound in the bin). Specific arguments are provided for the
#' first and the last bins
#' @name bin
#' @param pos a numeric between 0 and 1, 0 for the lower bond, 1 for
#'     the upper bond, 0.5 for the center of the class (or any other
#'     value between 0 and 1), which indicates to `as_numeric` how the
#'     bins should be coerced to numerical values,
#' @param xfirst,xlast the center of the first (last) class, if one
#'     wants to specify something different from the average of the
#'     lower and the upper bonds,
#' @param wlast in the case where the upper bond is infinite and
#'     `xlast` is not provided, the width of the last class is set to
#'     the one of the before last class. If `wlast` is provided, it is
#'     set to the width of the before last class times `wlast`,
#' @examples
#' z <- head(wages$size, 10)
#' # coerce to a numeric using the center of the bins
#' as_numeric(z, pos = 0.5)
#' # special values for the center of the first and of the last bin
#' as_numeric(z, pos = 0.5, xfirst = 5, xlast = 400)
#' # same, but indicating that the width of the last class should be
#' # twice the one of the before last
#' as_numeric(z, pos = 0.5, xfirst = 5, wlast = 2)
#' @export
as_numeric <- function(x, pos = 0, xfirst = NULL, xlast = NULL, wlast = NULL){
    # coerce a bin to a numeric. pos indicate the position of the
    # value in the bin, from 0 (lower bound) to 1 (upper bound). The
    # center of the first and the last class can be set using xfirst
    # and xlast. For the last bin, its width can be set using the
    # wlast argument which is a multiple of the width of the just
    # before last bin

    # first check if xfirst/xlast/wlast arguments are provided,
    # otherwise extract the corresponding attributes
    if (is.null(xfirst)) xfirst <- attr(x, "xfirst")
    if (is.null(xlast)) xlast <- attr(x, "xlast")
    if (is.null(wlast)) wlast <- attr(x, "wlast")
    if (! is.null(xlast) & ! is.null(wlast)) stop("only one of last or wlast should be set")
    if (! is.numeric(pos)) stop("pos should be numeric")
    if (is.numeric(pos) & ! (pos >= 0 & pos <= 1)) stop("pos should be between 0 and 1")
    # the series can contain individual data or unique values of
    # bins. The computation is done on the levels
    if (is.factor(x)){
        bins <- extract_bin(levels(x))
    } else {
        bins <- extract_bin(x)
    }
    K <- nrow(bins)

    if (! is.null(xfirst)){
        # xfirst should be inside the range of the first bin
        if (! (xfirst >= bins$first[1] & xfirst <= bins$last[1])) stop("irrelevant value for xfirst")
        # the lower bound is reset so that xfirst is now the center of the bin
        bins$first[1] <- xfirst - (bins$last[1] - xfirst)
    }
    if (! is.null(xlast)){
        if (! (xlast >= bins$first[K] & xlast <= bins$last[K])) stop("irrelevant value for last")
        # xlast should be inside the range of the last bin
        bins$last[K] <- bins$first[K] + 2 * (xlast - bins$first[K])
        # the upper bound is reset so that xlast is now the center of
        # the bin
    }
    else{
        if (is.infinite(bins$last[K])){
            # no xlast argument and the upper bound is infinite. In
            # this case define the width of the last bin by default =
            # the width of the before last bin or wlast x its width
            if (is.null(wlast)) wlast <- 1
            bins$last[K] <- bins$first[K] + wlast * (bins$last[K-1]- bins$first[K - 1])
        }
    }
    # compute the value as a weighted average of the two limits
    xnum <- (1 - pos) * bins$first + pos * bins$last
    # join on tibble that contains the original bins with the one that
    # contains the levels and the corresponding numerical value and
    # return the numerical series
    names(xnum) <- bins$bin
    as.numeric(xnum[as.character(x)])
}

cut_contbin <- function(x, breaks = NULL, ...){
    if (is.null(breaks)) stop("new breaks should be specified")
    # x breaks are provided in order to reduce the number of classes
    # first extract the bin
    new_x <- extract_bin(x)
    # guess the value of right
    left_op <- new_x[2, 2, drop = TRUE]
    if (left_op == "[") right_closed <- FALSE else right_closed <- TRUE
    # get the initial classes and computs the breaks
    initial_breaks <- sort(union(new_x$first, new_x$last))
    if (length(breaks) == 1){
        if (! breaks %in% initial_breaks)
            stop("the break value should be a bound of one of the bins")
        breaks <- initial_breaks[initial_breaks <= breaks]
    }
    # min/max values of the new breaks lower/larger than the
    # min/max values of the initial breaks are not allowed
    if (min(breaks) < min(initial_breaks))
        stop("the minimal value provided is lower than the initial lower bond")
    if (max(breaks) > max(initial_breaks))
        stop("the minimal value provided is lower than the initial lower bond")
    # min/max values of the initial breaks are included in the
    # new breaks if necessary
#    if (! min(initial_breaks) %in% breaks) breaks <- c(min(initial_breaks), breaks)
    if (! min(initial_breaks) %in% breaks) breaks <- c(0, breaks)
    if (! max(initial_breaks) %in% breaks) breaks <- c(breaks, max(initial_breaks))
    # put in form the vector of new breaks and check whether
    # some values are not part of the initial breaks
    breaks <- sort(unique(breaks))
#    na_breaks <- setdiff(breaks, initial_breaks)
    na_breaks <- setdiff(setdiff(breaks, 0), initial_breaks)
    if (length(na_breaks) > 0)
        stop(paste(paste(sort(na_breaks), collapse = ", "),
                   ifelse(length(na_breaks) == 1, "is", "are"),
                   paste("provided in the breaks argument but ",
                         ifelse(length(na_breaks) == 1, "is", "are"),
                         " not part of the  initial set of breaks", sep = "")),
             sep = "")
    new_x$center <- as_numeric(as_bin(new_x$bin), pos = 0.5)
    new_x$center <- cut(new_x$center, breaks = breaks, right = right_closed)
    z <- new_x$center
    names(z) <- new_x$bin
    unname(z[as.character(x)])
}

as_bin <- function(x){
    # coerce a series to a bin, and return NA for uncorrectly formated
    # values
    if (! is_bin(x)){
        ind_data <- length(x) > length(unique(x))
        if (ind_data){
            # for individual data, coerce to factor if necessary and
            # get the resulting levels (bin), storing the old ones in
            # olevels
            if (! is.factor(x)) x <- factor(x)
            olevels <- levels(x)
            bin <- levels(x)
        }
        # otherwise, take bin as the series or its levels if the
        # series is a factor
        else if(is.factor(x)) bin <- levels(x) else bin <- x
        # construct a tibble containing bin, extract the 4 components
        # and sort it according to the lower bond value
        bin_tbl <- extract_bin(bin)
        # get a vector containing the bin or NA if the format is incorrect
        bin_tbl$new_bin <- ifelse(is.na(bin_tbl$left), NA, bin)
        new_bin <- bin_tbl[c("bin", "new_bin")]
        
        # get a vector containing the incorrect levels
        na_levels <- new_bin$bin[is.na(new_bin$new_bin)]
        new_levels <- new_bin$bin[! is.na(new_bin$new_bin)]
        
        if (ind_data){            
            # for individual data, incorrect levels lead to NA values
            # and the uncorrect levels are dropped
            x[x %in% na_levels] <- NA
            x <- x[, drop = TRUE]
        }
        else{
            # for unique values of the bin, just replaced uncorrect
            # values by NAs and coerce to factor
            x[! x %in% new_levels] <- NA
            x <- factor(x)
        }
        # order the levels according to the lower bound
        x <- factor(x, new_levels)
        x <- structure(x, class = c("bin", class(x)))
    }
    x
}

is_bin <- function(x) inherits(x, "bin")

extract_bin <- function(data){
    # take a character series as argument that contains a bin, extract
    # the four components (lower/upper bonds and brackets) in a 4
    # columns tibble sorted according to the lower bound. Note that
    # unique values are considered (usefull when individual data are
    # provided).
    data <- unique(data)
    if (is.factor(data)) data <- levels(data)
    bin_tbl <- data.frame(bin = data)
    a_float <- "[-+]?[0-9]*\\.?[0-9]*[eE]?[-+]?[0-9]+"
    a_pattern <- paste("^", "(\\[|\\()", "(", a_float, "),(", a_float, "|Inf)", "(\\]|\\))", "$", sep = "")
    mtch <- regexec(a_pattern, data)
    nas <- sapply(mtch, function(x) x[1] == -1)
    if (all(nas)){
        n <- data.frame(bin = data, left = NA, first = NA, last = NA, right = NA)
    } else {
        na_bin <- data[nas]
        n <- do.call("rbind", regmatches(x = data, m = mtch))
        n <- as.data.frame(n)
        names(n) <- c("bin", "left", "first", "last", "right")
        if (length(na_bin)){
            # is it necessary (mimic tidyr::extract)
            n <- rbind(n,
                       data.frame(bin = na_bin, left = NA,
                                  first = NA, last = NA, right = NA))
        }
    }
    n$first <- as.numeric(n$first)
    n$last <- as.numeric(n$last)
    n
}

#' @rdname bin
#' @export
cut.bin <- function(x, ..., breaks = NULL){
    if (is.null(breaks)) stop("new breaks should be specified")
    # x breaks are provided in order to reduce the number of classes
    # first extract the bin
    new_x <- extract_bin(x)
    # guess the value of right
    left_op <- new_x[2, 2, drop = TRUE]
    if (left_op == "[") right_closed <- FALSE else right_closed <- TRUE
    # get the initial classes and computs the breaks
    initial_breaks <- sort(union(new_x$first, new_x$last))
    if (length(breaks) == 1){
        if (! breaks %in% initial_breaks) stop("the break value should be a bound of one of the bins")
        breaks <- initial_breaks[initial_breaks <= breaks]
    }
    # min/max values of the new breaks lower/larger than the
    # min/max values of the initial breaks are not allowed
    if (min(breaks) < min(initial_breaks)) stop("the minimal value provided is lower than the initial lower bond")
    if (max(breaks) > max(initial_breaks)) stop("the minimal value provided is lower than the initial lower bond")
    # min/max values of the initial breaks are included in the
    # new breaks if necessary
    if (! min(initial_breaks) %in% breaks) breaks <- c(min(initial_breaks), breaks)
    if (! max(initial_breaks) %in% breaks) breaks <- c(breaks, max(initial_breaks))
    # put in form the vector of new breaks and check whether
    # some values are not part of the initial breaks
    breaks <- sort(unique(breaks))
    na_breaks <- setdiff(breaks, initial_breaks)
    if (length(na_breaks) > 0)
        stop(paste(paste(sort(na_breaks), collapse = ", "),
                   ifelse(length(na_breaks) == 1, "is", "are"),
                   paste("provided in the breaks argument but ",
                         ifelse(length(na_breaks) == 1, "is", "are"),
                         " not part of the  initial set of breaks", sep = "")),
             sep = "")
    new_x$center <- as_numeric(as_bin(new_x$bin), pos = 0.5)
    new_x$center <- cut(new_x$center, breaks = breaks, right = right_closed)
    z <- new_x$center
    names(z) <- new_x$bin
    unname(z[as.character(x)])
}

#' @rdname bin
#' @export
cut.character <- function(x, ..., breaks = NULL){
    x <- as_bin(x)
    cut(x, ..., breaks = breaks)
}

#' @rdname bin
#' @export
cut.factor <- function(x, ..., breaks = NULL){
    x <- as_bin(x)
    cut(x, ..., breaks = breaks)
}
