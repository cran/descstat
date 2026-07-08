#' Frequency table
#'
#' Compute the frequency table of a categorical or a numerical series.
#' 
#' @name freq_table
#' @aliases freq_table
#' @param data a tibble,
#' @param x a categorical or numerical series,
#' @param f a string containing `n` for counts, `f` for relative
#'     frequencies, `p` for percentages and `m` for mass frequencies.
#'     Cumulative series are obtained using the same letters in upper
#'     caps,
#' @param vals a character containing letters indicating the values of
#'     the variable that should be returned; `x` for the center of the
#'     class, `l` and `u` for the lower and upper limit of the class,
#'     `a` for the range,
#' @param breaks a numerical vector of class limits,
#' @param max if the series is a discrete numerical value, this
#'     argument indicates that all the values greater than `max`
#'     should be merged in the same modality,
#' @param levels a character or a list indicating the levels in case
#'     of a categorical input
#' @param right a logical indicating whether the interval should be
#'     closed (`right = TRUE`, the default) or open (`right = FALSE`)
#'     on the right in case of a continuous numerical series,
#' @param weights a series that contain the weights that enable the
#'     sample to mimic the population,
#' @param xfirst,xlast,wlast see [descstat::as_numeric()],
#' @param total a logical indicating whether the total should be
#'     returned,
#' @param freq a series that contains the frequencies (only relevant
#'     if `data` is already a frequency table),
#' @param mass a series that contains the masses of the variable (only
#'     relevant if `data` is already a frequency table),
#' @param center a series that contains the center of the class of the
#'     variable (only relevant if `data` is already a frequency
#'     table),
#' @param numeric a boolean, if true, the numerical value is provided
#'     is the series is an integer
#' @param print a boolean, `TRUE` if a printable version of the table
#'     is required
#' @param x_is_char for internal use only
#' @return a tibble containing the specified values of `vals` and `f`.
#' @export
#' @importFrom stats median quantile
#' @keywords manip
#' @author Yves Croissant
#' @examples
#' # in table padova, price is a numeric variable, a vector of breaks should be provided
#' padova |> freq_table(price,
#'                       breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE)
#' # return relative frequencies and densities, and the center value
#' # of the series and the width of the bin
#' padova |> freq_table(price,
#'                       breaks = c(50, 100, 150, 200, 250, 300, 350, 400),
#'                       right = TRUE, f = "fd", vals = "xa")
#' # in table wages, wage is a factor that represents the classes
#' wages |> freq_table(wage, "d")
#' # a breaks argument is provided to reduce the number of classes
#' wages |> freq_table(wage, breaks = c(10, 20, 30, 40, 50))
#' # a total argument add a total to the frequency table
#' wages |> freq_table(wage, breaks = c(10, 20, 30, 40, 50), total = TRUE)
#' # ìncome is already a frequency table, the freq argument
#' # is mandatory
#' income |> freq_table(inc_class, freq = number)
#' # the mass argument can be indicated if one column contains the
#' # mass of the series in each bin. In this case, the center of the
#' # class are exactly the mean of the series in each bin
#' income |> freq_table(inc_class, freq = number, mass = tot_inc)
#' # reducing the number of classes
#' income |> freq_table(inc_class, freq = number, mass = tot_inc,
#'                      breaks = c(10, 20, 50, 100, 1000))
#' # rgp contains a children series which indicates the number of
#' # children of the households
#' rgp |> freq_table(children)
#' # a max argument can be indicated to merge the unusual high
#' # values of number of childre
#' rgp |> freq_table(children, max = 4)
#' # employment is a non random survey, there is a weights series
#' # that can be used to compute the frequency table according to the
#' # sum of weights and not to counts
#' employment |> freq_table(education)
#' employment |> freq_table(education, weights = weights)
#' @export
freq_table <- function(data, x,
                       f = "n", vals = NULL,
                       breaks = NULL, max = NULL, levels = NULL, right = TRUE,
                       weights = NULL,
                       xfirst = NULL, xlast = NULL, wlast = NULL,
                       total = FALSE,
                       freq = NULL, mass = NULL, center = NULL,
                       numeric = TRUE,
                       print = FALSE,
                       x_is_char = FALSE){
    # 1. get the x series and if required the wgts series

    if (! x_is_char){
        x_char <- get_name(substitute(x), nms = names(data))
    } else {
        x_char <- x
    }
    if (is.null(x_char)) stop("the x argument is mandatory")
    x <- data[[x_char]]

    wgts_char <- get_name(substitute(weights), nms = names(data))
    if (! is.null(wgts_char)) wgts <- data[[wgts_char]] else wgts <- NULL

    freq_char <- get_name(substitute(freq), nms = names(data))
    if (! is.null(freq_char)) freq <- data[[freq_char]] else freq <- NULL

    mass_char <- get_name(substitute(mass), nms = names(data))
    if (! is.null(mass_char)) mass <- data[[mass_char]] else mass <- NULL

    center_char <- get_name(substitute(center), nms = names(data))
    if (! is.null(center_char)) center <- data[[center_char]] else center <- NULL

    # first check if xfirst/xlast/wlast arguments are provided,
    # otherwise extract the corresponding attributes
    if (is.null(xfirst)) xfirst <- attr(x, "xfirst")
    xlast_is_provided <- wlast_is_provided <- FALSE
    if (is.null(xlast)){
        xlast <- attr(x, "xlast")
    } else {
        xlast_is_provided <- TRUE
    }
    if (is.null(wlast)){
        wlast <- attr(x, "wlast")
    } else {
        wlast_is_provided <- TRUE
    }
    if (wlast_is_provided) xlast <- NULL
    if (xlast_is_provided) wlast <- NULL
    if (! is.null(xlast) & ! is.null(wlast)) stop("only one of xlast or wlast should be set")
    
    # check wheter a frequency table is provided; in this case, the
    # freq argument is mandatory and the mass and center arguments can
    # be provided too (only one of them) and x contains unique values
    if (! is.null(center) & ! is.null(mass)) stop("only one of mass and center should be provided")
    freq_data <- ! is.null(freq)
    freq_data_x_provided <- ! is.null(mass) | ! is.null(center)
    if (length(unique(x)) == length(x) & ! freq_data)
        stop("data seems to be a frequency table and the freq argument is mandatory")

    # 2. coerce x to a bin
    if (! freq_data){
        x <- bin(x, breaks = breaks, max = max, levels = levels, right = right)
        .type <- attr(x, "type")
    }
    
    # 3. compute the absolute frequencies, either as a count or as the
    # sum the weights
    if (! freq_data){
        if (is.null(wgts)) z <- table(x) else z <- tapply(wgts, x, sum)
        z <- data.frame(names(z), as.numeric(z))
        names(z) <- c(x_char, "n")
        if (.type == "integer") z[[1]] <- as.numeric(z[[1]])
    } else {
        .type <- series_type(x)
        if (! is.null(breaks)){
            x <- bin(x, breaks = breaks)
            data[[x_char]] <- x
        }
        z <- data[c(x_char, freq_char)]
        names(z)[2] <- "n"
        if (freq_data_x_provided){
            if (! is.null(center)){
                .center <- data[[center_char]]
                if (! is.null(breaks)){
                    .freq <- tapply(z$n, x, sum)
                    .center <- tapply(.center * z$n, x, sum) / .freq                
                    z <- data.frame(names(.center), n = .freq, x = .center)
                    names(z)[1] <- x_char
                } else {
                    z$x <- .center
                }
            } else {
                .mass <- data[[mass_char]]
                if (! is.null(breaks)){
                    .freq <- tapply(z$n, x, sum)
                    .center <- tapply(.mass, x, sum) / .freq
                    z <- data.frame(names(.center), n = unname(.freq), x = unname(.center))
                    names(z)[1] <- x_char
                } else {
                    z$x <- .mass / z$n
                }
            }
        } else {
            if ( ! is.null(breaks)){
                .freq <- tapply(z$n, x, sum)
                z <- data.frame(names(.freq), n = as.numeric(.freq))
                names(z)[1] <- x_char
                z$x <- as_numeric(names(.freq), pos = 0.5)
            }
        }
    }

    # 4. get the vector of the kind of frequencies to compute
    get_letters <- function(x) strsplit(x, "")[[1]]
    # typology of the series
    all_f <- c("n", "f", "p", "d", "m", "N", "F", "P", "M", "d")
    char_f <- c("n", "f", "p")
    f_vec <- get_letters(f)
    # check for incorrect series
    na_f <- setdiff(f_vec, all_f)
    if (length(na_f))
        stop(paste(paste(na_f, collapse = ", "),
                   ifelse(length(na_f) == 1,
                          "is not a valid column",
                          "are not valid columns")))
    
    # 5. get the values to be computed if the vals argument is provided
    if (! is.null(vals)){
        vals_vec <- get_letters(vals)
        vals_na <- setdiff(vals_vec, c("a", "x", "l", "u"))
        if (length(vals_na) > 0)
            stop(paste(paste(sort(vals_na), collapse = ", "),
                       ifelse(length(na_f) == 1,
                              "is not a valid values",
                              "are not valid values")))
    } else vals_vec <- character(0)
    # densities and vals argument are only relevant for continuous
    # variables
    if (.type != "contbin" & "d" %in% f_vec)
        stop("the computation of densities is only relevant for continuous variables")
    if (.type != "contbin" & "m" %in% f_vec)
        stop("the computation of masses is only relevant for continuous variables")
    if (.type != "contbin" & ! is.null(vals))
        stop("the vals argument is only relevant for continuous variables")

    # 6. compute the values (the center of the classes) for contbin
    # series
    if (.type == "contbin" & ! freq_data_x_provided){
        z$x <- as_numeric(z[[x_char]], pos = 0.5, xlast = xlast,
                          xfirst = xfirst, wlast = wlast)
    }
    
    # 7. computation of the required frequencies ; no check of relevance
    # is required at this point as the potential errors were
    # previously checked
    if (any(c("f", "F", "d") %in% f_vec)) z$f <- z$n / sum(z$n)
    if (any(c("p", "P") %in% f_vec)) z$p <- z$n / sum(z$n) * 100
    # compute the masses if required
    if (any(c("m", "M") %in% f_vec))
        z$m = z$n * z$x / sum(z$n * z$x)
    # computation of densities, or computation of l, u and a which
    # are necessary for the computation of densities
    if (! is.null(vals) | "d" %in% f_vec){
        if ("d" %in% f_vec | any(c("a", "l") %in% vals_vec))
            z$l <- as_numeric(z[[x_char]], pos = 0, xlast = xlast,
                              xfirst = xfirst, wlast = wlast)
        if ("d" %in% f_vec | any(c("a", "u") %in% vals_vec))
            z$u <- as_numeric(z[[x_char]], pos = 1, xlast = xlast,
                              xfirst = xfirst, wlast = wlast)
        if ("d" %in% f_vec | "a" %in% vals_vec)
            z$a <- z$u - z$l
    }
    if ("d" %in% f_vec) z$d <- z$f / z$a
    # compute the cummulative distribution if required
    if (any(c("N", "F", "P", "M") %in% f_vec)){
        if ("N" %in% f_vec) z$N <- cumsum(z$n)
        if ("F" %in% f_vec) z$F <- cumsum(z$f)
        if ("P" %in% f_vec) z$P <- cumsum(z$p)
        if ("M" %in% f_vec) z$M <- cumsum(z$m)
    }
    
    # 8. Return the required subset of statistics
    selected_series <- f_vec
    if (.type %in% "contbin"){
        selected_series <- c(selected_series, vals_vec)
        selected_series <- c("x", setdiff(selected_series, "x"))
        attr(z[[x_char]], "extrem_classes") <- c(wlast = wlast, xlast = xlast, xfirst = xfirst)
    }

    # 9. add a total if required; in this case compute the total only
    # for non cummulative frequencies for which a total is irrelevant
    if (total){
        lowcaps <- grep( "^[nfpm]", names(z))
        if (length(lowcaps)){
            a <- lapply(z[, lowcaps, drop = FALSE], sum)
            algn <- vector(mode = "list", length = length(z))
            algn[1:length(algn)] <- NA
            names(algn) <- names(z)
            algn[names(a)] <- a
            z <- rbind(z, algn)
        }
    }
    z <- z[c(x_char, selected_series)]

    # 10. for the print version, replace 3.223 by >=3 and NA by total
    if (print){
        if (.type == "integer"){
            K <- ifelse(total, nrow(z) - 1, nrow(z))
            last_value <- z[[1]][K]
            if (last_value - floor(last_value) > sqrt(.Machine$double.eps)){
                z[[1]][K] <- paste(">=", floor(last_value), sep = "")
                z[[1]] <- factor(z[[1]], levels = z[[1]])
            }
        }
        if (total){
            levels(z[[1]]) <- c(levels(z[[1]]), "Total")
            z[[1]][is.na(z[[1]])] <- "Total"
        }
    }
    class(z[[1]]) <- c("bin", class(z[[1]]))
    class(z) <- c("freq_table", "data.frame")
    z
}

## format.freq_table <- function(x, ..., justify = "none", cut.names = TRUE){
##     a_NA <- which(is.na(x[[1]]))
##     if (length(a_NA)) x[[1]][a_NA] <- "Total"
##     class(x) <- setdiff(class(x), "freq_table")
##     format(x, ..., justify = "none", cut.names = TRUE)
## }    


