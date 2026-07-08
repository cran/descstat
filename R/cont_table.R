#' Contingency table
#'
#' A contingency table returns the counts of all the combinations of
#' the modalities of two series in a table for which every modality of
#' the first series is a row and every modality of the second series
#' is a column. The `joint`, `marginal` and `conditional` functions
#' compute these three distributions from the contingency table (by
#' indicating one series for the last two).
#'
#' `cont_table` actually returns a tibble in "long format", as the
#' `dplyr::count` table does. As the returned object is of class
#' `cont_table`, this is the `format` and `print` methods that turns
#' the tibble in a wide format before printing.
#'
#' The `conditional` and `joint` functions return a `cont_table`
#' object, as the `marginal` function returns a `freq_table` object.
#' 
#' 
#' @name cont_table
#' @aliases cont_table
#' @param data a tibble,
#' @param x the series for which the marginal or the conditional
#'     distribution should be computed, or a `cont_table` object for
#'     the `wide` function
#' @param x1,x2 the two series used the construct the contingency
#'     table, the distinct values of the first and the second will
#'     respectively be the rows and the columns of the contingency
#'     table,
#' @param weights a series containing the weights that should be used
#'     to mimic the population,
#' @param freq the frequencies (in the case where data is already
#'     contingency table),
#' @param total if `TRUE`, a total is added to the table,
#' 
#' @param xfirst1,xfirst2,xlast1,xlast2,wlast1,wlast2 see [descstat::as_numeric()],
#' @param breaks1,breaks2,max1,max2,levels1,levels2,right1,right2 see [descstat::bin()]
#' @param f,vals see [freq_table()],
#' @param x_is_char for internal use only
#' @return a tibble
#' @export
#' @importFrom stats var
#' @keywords manip
#' @author Yves Croissant
#' @examples
#' # get a contingency table containing education and sex
#' cont_table(employment, education, sex)
#' # instead of counts, sum the weights
#' cont_table(employment, education, sex, weights = weights)
#' # get the joint distribution and the conditional and marginal
#' # distribution of sex
#' cont_table(employment, education, sex) |> joint()
#' cont_table(employment, education, sex) |> marginal(sex)
#' cont_table(employment, education, sex) |> conditional(sex)
#' cont_table(employment, education, sex) |> joint() |> wide()
#' @export
cont_table <- function(data, x1, x2, weights = NULL, freq = NULL,
                       total = FALSE,
                       xfirst1 = NULL, xlast1 = NULL, wlast1 = NULL,
                       xfirst2 = NULL, xlast2 = NULL, wlast2 = NULL,
                       breaks1 = NULL, breaks2 = NULL,
                       max1 = NULL, max2 = NULL,
                       right1 = NULL, right2 = NULL,
                       levels1 = NULL, levels2 = NULL){
    
    x1_char <- get_name(substitute(x1), names(data))
    x2_char <- get_name(substitute(x2), names(data))
    freq_char <- get_name(substitute(freq), names(data))
    wgts_char <- get_name(substitute(weights), names(data))

    x1 <- bin(data[[x1_char]], breaks = breaks1, max = max1,
              levels = levels1, right = right1)
    x2 <- bin(data[[x2_char]], breaks = breaks2, max = max2,
              levels = levels2, right = right2)
    the_series <- data.frame(x1, x2)
    if (nrow(unique(the_series)) == nrow(the_series) & is.null(freq_char))
        stop("data seems to be a frequency table and the freq argument is mandatory")
    
    if (! is.null(freq_char)){
        ct <- data[c(x1_char, x2_char, freq_char)]
        names(ct)[3] <- "n"
    }
    else{
        if (is.null(wgts_char)){
            ct <- as.data.frame(table(x1, x2))
            names(ct) <- c(x1_char, x2_char, "n")
        } else {
            ct <- as.data.frame(as.table(
                tapply(data[[wgts_char]],
                       list(data[[x1_char]], data[[x2_char]]),
                       sum)))
            names(ct) <- c(x1_char, x2_char, "n")
        }
    }
    if (total){
        mg_1 <- tapply(ct$n, ct[[x1_char]], sum)
        mg_2 <- tapply(ct$n, ct[[x2_char]], sum)
        .tot <- sum(mg_1)
        mg_1 <- data.frame(names(mg_1), NA, as.numeric(mg_1))
        mg_2 <- data.frame(NA, names(mg_2), as.numeric(mg_2))
        mg_T <- data.frame(NA, NA, .tot)
        names(mg_1) <- names(mg_2) <- names(mg_T) <- c(x1_char, x2_char, "n")
        ct <- rbind(ct, mg_1, mg_2, mg_T)

    }
    limits <- list(list(xfirst = xfirst1, xlast = xlast1, wlast = wlast1),
                   list(xfirst = xfirst2, xlast = xlast2, wlast = wlast2))
    names(limits) <- c(x1_char, x2_char)
    structure(ct,
              class = c("cont_table", class(ct)),
              total = total,
              limits = limits)
}

#' @rdname cont_table
#' @export
joint <- function(data){
    data <- total.omit(data)
    data$n <- data$n / sum(data$n)
    names(data)[match("n", names(data))] <- "f"
    data
}

#' @rdname cont_table
#' @export
conditional <- function(data, x = NULL, x_is_char = FALSE){
    limits <- attr(data, "limits")
    if (! x_is_char){
        x_char <- get_name(substitute(x), names(data))
    } else {
        x_char <- x
    }
    cond_name <- setdiff(names(data)[1:2], x_char)
    data <- as.data.frame(total.omit(data))
    data <- split(data, data[[cond_name]])
    data <- lapply(data, function(x){
        x$n <- x$n / sum(x$n)
        x})
    data <- Reduce(rbind, data)
    names(data)[match("n", names(data))] <- "f"
    structure(data, class = c("cont_table", class(data)), x = x_char, limits = limits)
}

#' @rdname cont_table
#' @export
marginal <- function(data, x = NULL, f = "f", vals = NULL, x_is_char = FALSE){
    if (! x_is_char){
        x_char <- get_name(substitute(x), names(data))
    } else {
        x_char <- x
    }
    limits <- attr(data, "limits")
    has_total <- any(is.na(data[[x_char]]))
    f_name <- names(data)[3]
    limits <- limits[[x_char]]
    data <- tapply(data[[f_name]], data[[x_char]], sum)
    data <- data.frame(names(data), as.numeric(data))
    names(data) <- c(x_char, "f")
    data$f <- data$f / sum(data$f)
    if (series_type(data[[x_char]]) != "categorical"){
        data <- freq_table(data, x_char, f = f, vals = vals, freq = "f",
                           xfirst = limits$xfirst,
                           xlast = limits$xlast,
                           wlast = limits$wlast, x_is_char = TRUE)
        data[[1]] <- unclass(data[[1]])
    }
    class(data) <- c("freq_table", class(data))
    data
}

#' @rdname cont_table
#' @importFrom stats reshape
#' @export
wide <- function(x){
    x[[1]] <- as.character(x[[1]])
    x[[2]] <- as.character(x[[2]])
    x[[1]][is.na(x[[1]])] <- "Total"
    x[[2]][is.na(x[[2]])] <- "Total"
    names_row <- names(x)[1]
    names_col <- names(x)[2]
    name_col1 <- paste(names_row, "|", names_col, sep = "")
    x <- reshape(x, timevar = names(x)[2], idvar = names(x)[1], direction = "wide")
    names(x)[-1] <- substr(names(x)[-1], 3L, 1000L)
    names(x)[1] <- name_col1
    x
}
    

