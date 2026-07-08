#' tinyplot types
#'
#' tinyplot types for frequency tables
#' 
#' @name tinyplot
#' @aliases tinyplot
#' @param position the position of the labels for `type_pie`
#' @param edges the number of edges for `type_pie`
#' @param hole the size of the hole of a donut chart for `type_pie`
#' @param radius the size of the foreground for `type_pie`
#' @param init.angle the initital angle for `type_pie`
#' @param size the sizes of the smallest and of the largest point for
#'     `type_cont.table`
#' @param level the confidence interval for `type_anova`
#' @param expand for `type_cumul` a numeric indicating the fraction of
#'     the range of the series and set the width of the segments for
#'     which the cummulative distribution is 0 or 1, for `type_anova`,
#'     a numeric of length 2 indicating the horizontal and vertical
#'     margins added to the range of the series
#' @param f the frequency that is printed, one of `"none"` (the
#'     default), `"n"` for the absolute frequency, `"f"` for the
#'     relative frequency and `"p"` for the percentages
#' @param max,breaks see `descstat::freq_table`
#' @param digits the number of printed digits of the frequencies
#' @param pal a palette of colors
#' @param geom for `type_cumul` the geometry that is ploted ; by
#'     default, `"s"` for integer series and `"b"` for continuous bin
#'     series
#' @param length the length of the error bars for `type_anova`
#' @param wlast comment
#' @importFrom graphics lines polygon text points arrows
#' @return invisible, used for its side effects
#' @author Yves Croissant
#' @importFrom stats qnorm
#' @importFrom grDevices palette
#' @export
type_pie <- function(position = NULL,
                     edges = 200,
                     hole = 0,
                     radius = 1,
                     init.angle = 0,
                     f = c("none", "n", "f", "p"),
                     digits = 2,
                     pal = "Set2"){
    f <- match.arg(f)
    if (f == "none" & is.null(position)) position <- 0.5
    if (f != "none" & is.null(position)) position <- c(1.05, 0.5)

    data_pie <- function(edges, hole, radius, init.angle){
        fun <- function(settings, ...){
            if (is.null(settings$ylab) || settings$ylab == "x"){ 
                y <- settings$datapoints$y
                freqs <- table(y)
                labs <- factor(names(freqs))
            } else {
                freqs <- settings$datapoints$y
                labs <- settings$datapoints$x
            }
            freqs <- unname(freqs)
            props <- freqs / sum(freqs)
            agls <- cumsum(props)
            if (f == "f") freqs <- round(props, digits)
            if (f == "p") freqs <- paste(round(props * 100, digits), "%", sep = "")
            if (f == "none") freqs <- rep("", length(freqs))

            freqs <- paste("p", as.character(freqs), sep = "")
            agls <- c(0, agls)
            settings$ylab <- settings$xlab <- NULL
            settings$axes <- settings$legend <- FALSE
            N <- length(agls) - 1
            datapoints <- lapply(1:N,
                                 function(i){
                                     xi <- agls[i]
                                     xip1 <- agls[i + 1]
                                     n <- max(2, floor(edges * (xip1 - xi)))
                                     x_seq <- seq.int(xi, xip1, length.out = n)
                                     x <- cos(x_seq * 2 * pi + init.angle * pi / 180)
                                     y <- sin(x_seq * 2 * pi + init.angle * pi / 180)
                                     xmax <- x[floor(n / 2)]
                                     ymax <- y[floor(n / 2)]
                                     if (hole == 0){
                                         d <- data.frame(x = c(x, 0),
                                                         y = c(y, 0),
                                                         xmax = xmax,
                                                         ymax = ymax,
                                                         xmin = as.character(labs[i]),
                                                         ymin = freqs[i])
                                     } else {
                                         xh <- cos(x_seq * 2 * pi + init.angle * pi / 180) * hole
                                         yh <- sin(x_seq * 2 * pi + init.angle * pi / 180) * hole                 
                                         d <- data.frame(x = c(x, rev(xh)),
                                                         y = c(y, rev(yh)),
                                                         xmax = xmax,
                                                         ymax = ymax,
                                                         xmin = labs[i],
                                                         ymin = freqs[i])
                                     }
                                 }
                                 ) |> Reduce(f = "rbind")
            settings$datapoints <- datapoints
            settings$xlim <- settings$ylim <- c(- radius, radius)
        }
        return(fun)
    }
    
    draw_pie <- function(position){
        fun <- function(ix, iy, ixmax, iymin, iymax, ixmin, ...){
            iymin <- substr(iymin, 2, 100)
            print_freq <- ! all(nchar(iymin[! is.na(iymin)]) == 0)
            # .levs is a character containing the labels
            .levs <- unique(ixmin)
            # call twice palette to get the relevant colors
            cols <- palette(pal)[2:(length(.levs) + 1)]
            cols <- palette(pal)[2:(length(.levs) + 1)]
            names(cols) <- .levs
            for (alevel in .levs){
                sel <- ixmin == alevel
                # .xmax / .ymax are the coordinates of the center of
                # the portion of the circle for each portion / used to
                # position the labels
                .xmax <- ixmax[sel][1]
                .ymax <- iymax[sel][1]
                afreq <- iymin[sel][1]
                polygon(ix[sel], iy[sel], col = cols[alevel])
                if (position[1] >= 1){
                    lines(c(1, 1.05) * .xmax, c(1, 1.05) * .ymax)
                }
                .pos <- ifelse(.xmax > 0, 4, 2)
                if (position[1] < 1) .pos <- NULL
                if (abs(.xmax) < 0.3) .pos <- ifelse(.ymax > 0, 3, 1)
                text(.xmax * position[1],
                     .ymax * position[1],
                     alevel,
                     pos = .pos,
                     cex = 1)
                text(.xmax * position[2],
                     .ymax * position[2],
                     afreq,
                     cex = 1)
            }
        }
        return(fun)
    }

    out = list(
        data = data_pie(edges = edges, hole = hole, radius = radius, init.angle = init.angle),
        draw = draw_pie(position = position),
        name = "pie")
    class(out) = "tinyplot_type"
    return(out)
}

#' @rdname tinyplot
#' @export
type_lorenz <- function(){
    data_lorenz <- function(){
        fun <- function(settings, ...){
            data <- settings$datapoints
            if (is.null(settings$ylab) || settings$ylab == "x"){
                dat <- data.frame(y = data$y)
                dat <- freq_table(dat, "y", "FM")
                settings$datapoints <- data.frame(y = dat$M, x = dat$F, ymax = dat$F)
            }
            if (settings$ylab == "x"){
                settings$ylab <- "cummulative mass"
                settings$xlab <- "cummulative density"
            }
            return(fun)
        }
    }

    draw_lorenz <- function() {
        fun <- function(ix, iy, icol, ibg, ipch, ilwd, icex, ...) {
            polygon(c(0, ix), c(0, iy), col = icol)
            lines(ix, iy, type = "h")
            points(ix, iy, type = "p", pch = ipch, cex = icex)
            lines(c(0, 1), c(0, 0))
            lines(c(0, 1), c(0, 1))
        }
        return(fun)
    }

    out = list(
        data = data_lorenz(),
        draw = draw_lorenz(),
        name = "p" # fallback behaviour same as "p" type (e.g., legend defaults)
    )
    class(out) <- "tinyplot_type"
    return(out)   
    
}

#' @rdname tinyplot
#' @export
type_freqpoly <- function(breaks = NULL) {
    # data tranformation function
    data_freqpoly <- function() {
        fun <- function(settings, ...) {
            # extract raw datapoints from settings environment
            if (is.null(settings$ylab) || settings$ylab == "x"){
                dat <- data.frame(y = settings$datapoints$y)
                dat <- freq_table(dat, "y", "d", breaks = breaks)
                data <- data.frame(x = dat[[1]], y = dat$d)
                if (settings$ylab == "x"){
                    settings$ylab <- "density"
                }
            } else {
                data <- settings$datapoints[c("x", "y")]
            }
            data[[1]] <- as.character(data[[1]])
            K <- nrow(data)
            xu <- as_numeric(data[[1]], 1)
            xl <- as_numeric(data[[1]], 0)
            x <- as_numeric(data[[1]], 0.5)
            xo <- xl[1] - (x[1] - xl[1])
            xs <- xu[K] + (xu[K] - x[K])
            data$x <- x
            data <- rbind(list(x = xo, y = 0),
                          data,
                          list(x = xs, y = 0)
                          )
            # re-assign modified datapoints back to settings
            settings$datapoints = data
            settings$type <- "b"
        }
        return(fun)
    }

    out <- list(
        data = data_freqpoly(),
        draw = NULL,
        name = "b" # fallback behaviour same as "p" type (e.g., legend defaults)
    )
    class(out) <- "tinyplot_type"
    return(out)   
}


#' @rdname tinyplot
#' @export
type_histo <- function(wlast = 2, breaks = NULL) {
    # data tranformation function
    data_histo <- function() {
        fun <- function(settings, ...) {
            # extract raw datapoints from settings environment
            if (is.null(settings$ylab) || settings$ylab == "x"){
                dat <- data.frame(y = settings$datapoints$y)
                dat <- freq_table(dat, "y", "d", breaks = breaks)
                data <- data.frame(x = dat[[1]], y = dat$d)
                if (settings$ylab == "x"){
                    settings$ylab <- "density"
                }
            } else {
                data <- settings$datapoints[c("x", "y")]
            }
#            data = settings$datapoints
            data[[1]] <- as.character(data[[1]])
            xu <- as_numeric(data[[1]], 1, wlast = wlast)
            xl <- as_numeric(data[[1]], 0, wlast = wlast)
            xm <- as_numeric(data[[1]], 0.5, wlast = wlast)
            K <- nrow(data)
            xu[K] <- xl[K] + 2 * (xm[K] - xl[K])
            xl[1] <- xu[1] - 2 * (xu[1] - xm[1])
            data <- data.frame(x = c(0, xl, xu[K]),
                               y = c(0, data$y, data$y[K]))
            # re-assign modified datapoints back to settings
            settings$datapoints = data
        }
        return(fun)
    }
    draw_histo <- function() {
        fun <- function(ix, iy, ...){
            ix <- ix[- 1]
            iy <- iy[- 1]
            lines(ix, iy, type = "s")
            lines(ix, iy, type = "h")
        }
        return(fun)
    }
    out <- list(
        data = data_histo(),
        draw = draw_histo(),
        name = "s" # fallback behaviour same as "p" type (e.g., legend defaults)
    )
    class(out) <- "tinyplot_type"
    return(out)   
}

#' @rdname tinyplot
#' @export
type_cumul <- function(expand = 0.1, geom = NULL, breaks = NULL, max = NULL) {
    # data tranformation function
    data_cumul <- function() {
        fun <- function(settings, ...) {
            # extract raw datapoints from settings environment
            if (settings$ylab == "x" || settings$xlab == "Index"){
                dat <- data.frame(y = settings$datapoints$y)
                dat <- freq_table(dat, "y", "F", breaks = breaks, max = max)
                if (settings$xlab == "Index" || settings$ylab == "x"){
                    settings$xlab <- settings$ylab
                    settings$ylab <- "cummulative frequency"
                }

                data <- data.frame(x = dat[[1]], y = dat$F)
            } else {
                data <- settings$datapoints
            }
            is_cont_bin <- ! is.numeric(data$x)
            if (is_cont_bin){
                xsup <- as_numeric(as.character(data$x),
                                   pos = 1)[nrow(data)]
                x <- as_numeric(as.character(data$x))
                data <- data.frame(x = c(x, xsup),
                                   y = c(0, data$y))
            }
            xmin <- min(data$x) - diff(range(data$x)) * expand
            xmax <- max(data$x) + diff(range(data$x)) * expand
            y <- c(0, data$y, 1)
            x <- c(xmin, data$x, xmax)
            attr(x, "geom") <- ifelse(is_cont_bin, "b", "s")
            data <- data.frame(x = x, y = y)
           settings$datapoints <- data
        }
        return(fun)
    }
    draw_cumul <- function() {
        fun <- function(ix, iy, ...){
            if (is.null(geom))
                geom <- attr(ix, "geom")
            lines(ix, iy, type = geom)            
        }
        return(fun)
    }
    out <- list(
        data = data_cumul(),
        draw = draw_cumul(),
        name = "cumul" # fallback behaviour same as "p" type (e.g., legend defaults)
    )
    class(out) <- "tinyplot_type"
    return(out)   
}

#' @rdname tinyplot
#' @export
type_cont.table <- function(size = c(1, 2)) {
    # data tranformation function
    data_cont.table <- function() {
        ## fun <- function(settings, ...) {
        ##     # extract raw datapoints from settings environment
        ##     data = settings$datapoints
        ##     data$x <- as_numeric(data$x)
        ##     data$y <- as_numeric(data$y)
        ##     mins <- min(data$by)
        ##     maxs <- max(data$by)
        ##     data$xmin <- size[1] + (data$by - mins) / (maxs - mins) * (size[2] - size[1])
        ##     # re-assign modified datapoints back to settings
        ##     settings$legend <- FALSE
        ##     settings$datapoints <- data
        ## }
        fun <- function(settings, ...){
            data <- settings$datapoints
            z <- data.frame(y1 = data$x, y2 = data$y)
            z <- cont_table(z, "y1", "y2")
            .size <- size[1] + (z$n - min(z$n)) / (max(z$n) - min(z$n)) * (size[2] - size[1])
            settings$datapoints <- data.frame(x = as_numeric(z$y1, 0.5), y = as_numeric(z$y2, 0.5), xmin = .size)
            settings$legend <- FALSE
        }
        return(fun)
    }
    draw_cont.table <- function() {
        fun <- function(ix, iy, ixmin, ipch, ...){
            points(ix, iy, cex = ixmin, pch = ipch)
        }
        return(fun)
    }
    out <- list(
        data = data_cont.table(),
        draw = draw_cont.table(),
        name = "p" # fallback behaviour same as "p" type (e.g., legend defaults)
    )
    class(out) <- "tinyplot_type"
    return(out)   
}

#' @rdname tinyplot
#' @export
type_anova <- function(level = 0.95, length = 0.1, expand = 0.05) {
    if (length(expand) == 1) expand <- c(expand, expand)
    ## data_anova <- function() {
    ##     fun <- function(settings, ...) {
    ##         # extract raw datapoints from settings environment
    ##         data = settings$datapoints
    ##         std <- sqrt(data$ymax)
    ##         cv <- qnorm( (1 - level) / 2, lower.tail = FALSE)
    ##         data$ymin <- data$y + cv * std
    ##         data$ymax <- data$y - cv * std
    ##         settings$datapoints = data
    ##     }
    ##     return(fun)
    ## }
    data_anova <- function() {
        fun <- function(settings, ...){
            data <- settings$datapoints
            z <- data.frame(y1 = data$y, y2 = data$x)
            xmax <- max(as_numeric(z$y2, 0.5))
            xmin <- min(as_numeric(z$y2, 0.5))
            xmin <- xmin - expand[1] * (xmax - xmin)
            xmax <- xmax + expand[1] * (xmax - xmin)
            
            z <- cont_table(z, "y1", "y2")
            z <- anova(z, "y1")
            std <- sqrt(z$variance)
            cv <- qnorm( (1 - level) / 2, lower.tail = FALSE)
            data <- data.frame(x = z$x,
                               y = z$mean,
                               ymin = z$mean - cv * std,
                               ymax = z$mean + cv * std)
            range_y <- max(data$ymax) - min(data$ymin)
            settings$ylim = c(min(data$ymin) - range_y * expand[2],
                              max(data$ymax) + range_y * expand[2])
            settings$datapoints <- data
            settings$xlim <- c(xmin, xmax)
        }
        return(fun)
    }
    draw_anova <- function() {
        fun <- function(ix, iy, iymax, iymin, ipch, icex, ilty, ilwd, ...){
            arrows(x0 = ix, y0 = iymin, x1 = ix, y1 = iymax,
                   length = length, angle = 90, code = 3, lwd = ilwd)
            lines(ix, iy, lty = ilty)
            points(x = ix, y = iy, pch = ipch, cex = icex)
        }
        return(fun)
    }
    out <- list(
        data = data_anova(),
        draw = draw_anova(),
        name = "b" # fallback behaviour same as "p" type (e.g., legend defaults)
    )
    class(out) <- "tinyplot_type"
    return(out)   
}


  ## alpha, axes, bg, bubble, by, by_dep, call, cex, cex_dep, col, datapoints, dodge, dots, draw, facet, facet_attr, facet_by, facet_dep, facet.args, file, fill, flip, frame.plot, height, legend, legend_args, log, lty, lwd, null_by, null_facet, null_palette, null_xlim, null_ylim, palette, pch, ribbon.alpha, type, type_data, type_draw, type_info, type_name, was_area_type, width, x, x_by, x_dep, xaxb, xaxl, xaxs, xaxt, xlab, xlabs, xlim, xmax, xmax_dep, xmin, xmin_dep, y, y_dep, yaxb, yaxl, yaxs, yaxt, ygroup, ylab, ylabs, ylim, ymax, ymax_dep, ymin, ymin_dep
