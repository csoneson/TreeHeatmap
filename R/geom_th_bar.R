
#' add barplots
#' @param name a name to select heatmap
#' @param th_data a data frame. It should include one column named as
#'   \code{rowLab} to store the row name of the heatmap when \code{side} is
#'   \strong{left} or \strong{right}; otherwise, it should include one column
#'   named as \code{colLab} to store the column name of the heatmap when
#'   \code{side} is \strong{top} or \strong{bottom}.
#' @param side a character to specify where to put the barplot. It is selected
#'   from \strong{left}, \strong{right}, \strong{top} or \strong{bottom}.
#' @param gap a numeric value to specify the gap between the selected heatmap
#'   and the barplot
#' @param rel_width a numeric value decide the relative width of the barplot
#'   compared to the selected heatmap
#' @param stat Please see \code{stat} of \code{\link[ggplot2]{geom_bar}}.
#' @param ... More arguements accepted by \code{\link[ggplot2]{geom_bar}}.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @import ggplot2
#' @export
#' @return geom layer
#' @author Ruizhu Huang

geom_th_bar <- function(name = NULL,
                        th_data = NULL,
                        mapping = NULL,
                        side = "right",
                        gap = 1,
                        rel_width = 1,
                        rel_height = 1,
                        stat = "identity",
                        ...) {

    if (side %in% c("left", "right")) {
        position <- position_nudge(x = gap)
        orientation <- "y"
    } else {
        position <- position_nudge(y = gap)
        orientation <- "x"
    }

    if (stat != "identity") {
        stop("Currently, only (stat = 'identity') is supported.")
    }
    new_layer <- layer(
        stat = stat, data = NULL, mapping = mapping,
        geom = GeomBar, position = position,
        params = list(orientation = orientation, ...)
    )

    th_params <- list(gap = gap, name = name, side = side,
                      th_data = th_data, rel_width = rel_width,
                      rel_height = rel_height)
    ggproto("ggTHbar", new_layer, th_params = th_params)
}


#' @method ggplot_add ggTHbar
#' @import ggplot2
#' @importFrom methods is
#' @importFrom rlang .data as_label ':='
#' @importFrom utils modifyList
#' @importFrom dplyr '%>%' left_join select
#' @export

ggplot_add.ggTHbar <- function(object, plot, object_name) {

    # the active layer of ggheat
    current <- .current_heatmap(plot = plot, object = object)

    # side: left / right; top/bottom
    side <- object$th_params$side
    th_data <- object$th_params$th_data
    rel_width <- object$th_params$rel_width
    rel_height <- object$th_params$rel_height
    gap <- object$th_params$gap

    # width & height of the heatmap
    ra <- .row_anchor(plot, current)
    ww <- max(ra$maxX - ra$minX, na.rm = TRUE)
    ca <- .col_anchor(plot, current)
    hh <- max(ca$maxY - ca$minY, na.rm = TRUE)

    # default mapping & data
    if (side %in% c("left", "right")) {
        # mapping
        ylab <- as_label(object$mapping$y)
        if (!ylab %in% colnames(th_data)) {
            stop("y in 'aes()' should be rowLab")
        }
        # fix y = "y" to align the barplot to rows
        fix_y_mapping <- aes_string(y = "y")
        object$mapping <- modifyList(object$mapping, fix_y_mapping)

        # get the x label
        xlab <- as_label(object$mapping$x)
        object$data <- .row_anchor(plot, current) %>%
            select(.data$rowLab, .data$y) %>%
            left_join(th_data, by = "rowLab") %>%
            mutate(mw = diff(range(.data[[xlab]], na.rm = TRUE)),
                   !!xlab := .data[[xlab]] * ww/.data$mw * rel_width)

        # position: right
        if (side == "right") {
            minX <- min(.row_anchor(plot, current)$minX)
            barX <- max(object$data[[xlab]], na.rm = TRUE)
            object$position$x <- minX - gap - barX
        } else {
            # position: right
            maxX <- max(.row_anchor(plot, current)$maxX)
            object$position$x <- object$position$x + maxX
        }

    } else {
        # mapping
        xlab <- as_label(object$mapping$x)
        if (!xlab %in% colnames(th_data)) {
            stop("x in 'aes()' should be colLab")
        }
        fix_x_mapping <- aes_string(x = "x")
        object$mapping <- modifyList(object$mapping, fix_x_mapping)

        # get the y label
        ylab <- as_label(object$mapping$y)
        object$data <- .col_anchor(plot, current) %>%
            select(.data$colLab, .data$x, .data$w) %>%
            left_join(th_data, by = "colLab") %>%
            mutate(height = diff(range(.data[[ylab]], na.rm = TRUE)),
                   old = .data[[ylab]],
                   !!ylab := .data[[ylab]] * hh/.data$height * rel_height)

        # position: bottom
        if (side == "bottom") {
            minY <- min(.col_anchor(plot, current)$minY)
            barY <- max(object$data[[ylab]], na.rm = TRUE)
            object$position$y <- minY - gap - barY

            # df_axis
            od <- object$data
            ht <- unique(od$height)
            df <- data.frame(axisV = pretty(od$old, n = 2) ,
                             axisX = min(od$x, na.rm = TRUE)- 0.55*unique(od$w),
                             axisXX = max(od$x, na.rm = TRUE)+ 0.55*unique(od$w)) %>%
                mutate(axisY = .data$axisV * hh/ht* rel_height +
                           object$position$y)
            plot$heatmap[[current]]$col_tmp$df_axis <- df

            } else {
            # position: top
            maxY <- max(.col_anchor(plot, current)$maxY)
            object$position$y <- object$position$y + maxY
        }


    }

    NextMethod()
}


