#' add the boundary line of a heatmap
#'
#' add a line to separate heatmaps
#'
#' @param name the name of the heatmap to add row or column labels.
#' @param side a character value selected from \strong{left}, \strong{right},
#'   \strong{top} or \strong{bottom}. \strong{left} and \strong{right} to add
#'   the vertical line; \strong{top} or \strong{bottom} to add the horizontal
#'   line.
#' @param nudge_x a value to shift the border line horizontally.
#' @param nudge_y a value to shift the border line vertically.
#' @param extend_x a numeric vector with two elements. For example, \code{c(1,
#'   2)} would extend the left end of the line by 1 and the right end by 2.
#' @param extend_y a numeric vector with two elements. For example, \code{c(1,
#'   2)} would extend the bottom end of the line by 1 and the top end by 2.
#' @inheritParams ggplot2::geom_segment
#' @import ggplot2
#' @importFrom dplyr left_join
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_borderline <- function(mapping = NULL,
                           name = NULL,
                           side = "left",
                           nudge_x = 0,
                           nudge_y = 0,
                           extend_x = c(0, 0),
                           extend_y = c(0, 0),
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {

    .annotate_layer(mapping = mapping, th_data = NULL,
                    data = NULL, name = name,
                    subset = NULL, side = side,
                    nudge_x = nudge_x, nudge_y = nudge_y,
                    extend_x = extend_x, extend_y = extend_y,
                    na.rm = na.rm, show.legend = show.legend,
                    inherit.aes = inherit.aes,
                    geom = "segment", stat = StatTH,
                    new_class = "ggTHborder",
                    ...)

}



#' @method ggplot_add ggTHborder
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @export

ggplot_add.ggTHborder <- function(object, plot, object_name) {


    if (!length(plot$row_anchor)) {
        stop("row anchor data is missing ...")
    }
    if (!length(plot$col_anchor)) {
        stop("column anchor data is missing ...")
    }
    # the active layer of ggheat
    current <- object$stat_params$name
    if (is.null(current)) {
        current <- length(plot$row_anchor)
    } else {
        if (!current %in% names(plot$row_anchor)) {
            stop(current, " can't be found")
        }
    }

    # side: left / right; top/bottom
    side <- object$stat_params$side
    extend_x <- object$stat_params$extend_x
    extend_y <- object$stat_params$extend_y

    # default mapping & data
    if (side %in% c("left", "right")) {
        df <- plot$row_anchor[[current]]
        object$data <- data.frame(
            minX = unique(df$minX),
            maxX = unique(df$maxX),
            minY = min(df$y - 0.5*df$h, na.rm = TRUE) - extend_y[1],
            maxY = max(df$y + 0.5*df$h, na.rm = TRUE) + extend_y[2])

        if (side == "left") {
            self_mapping <- aes_string(x = "minX", y = "minY",
                                       xend = "minX", yend = "maxY")
        } else {
            self_mapping <- aes_string(x = "maxX", y = "minY",
                                       xend = "maxX", yend = "maxY")
        }


    } else {
        df <- plot$col_anchor[[current]]
        object$data <- data.frame(
            minX = min(df$x - 0.5*df$w) - extend_x[1],
            maxX = max(df$x + 0.5*df$w) + extend_x[2],
            minY = unique(df$minY),
            maxY = unique(df$maxY))
        if (side == "top") {
            self_mapping <- aes_string(x = "minX", y = "maxY",
                                       xend = "maxX", yend = "maxY")
        } else {
            self_mapping <- aes_string(x = "minX", y = "minY",
                                       xend = "maxX", yend = "minY")
        }
    }

    if (is.null(object$mapping)) {
        object$mapping <- self_mapping
    } else {
        object$mapping <- modifyList(self_mapping, object$mapping)
    }

    NextMethod()
}

#' Label the border line
#'
#' Label the border line
#'
#' @param name the name of the heatmap to add row or column labels.
#' @param side a character value selected from \strong{left}, \strong{right},
#'   \strong{top} or \strong{bottom}. \strong{left} and \strong{right} to add
#'   the vertical line; \strong{top} or \strong{bottom} to add the horizontal
#'   line.
#' @param nudge_x a value to shift the border line horizontally.
#' @param nudge_y a value to shift the border line vertically.
#' @param extend_x a numeric vector with two elements. For example, \code{c(1,
#'   2)} would extend the left end of the line by 1 and the right end by 2.
#' @param extend_y a numeric vector with two elements. For example, \code{c(1,
#'   2)} would extend the bottom end of the line by 1 and the top end by 2.
#' @inheritParams ggplot2::geom_text
#' @import ggplot2
#' @importFrom dplyr left_join
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_bordertext <- function(mapping = NULL,
                               name = NULL,
                               side = "left",
                               nudge_x = 0,
                               nudge_y = 0,
                               extend_x = c(0, 0),
                               extend_y = c(0, 0),
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               ...) {

    .annotate_layer(mapping = mapping, th_data = NULL,
                    data = NULL, name = name,
                    subset = NULL, side = side,
                    nudge_x = nudge_x, nudge_y = nudge_y,
                    extend_x = extend_x, extend_y = extend_y,
                    na.rm = na.rm, show.legend = show.legend,
                    inherit.aes = inherit.aes,
                    geom = "text", stat = StatTH,
                    new_class = "ggTHbordertext",
                    ...)

}



#' @method ggplot_add ggTHbordertext
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @export

ggplot_add.ggTHbordertext <- function(object, plot, object_name) {


    if (!length(plot$row_anchor)) {
        stop("row anchor data is missing ...")
    }
    if (!length(plot$col_anchor)) {
        stop("column anchor data is missing ...")
    }
    # the active layer of ggheat
    current <- object$stat_params$name
    if (is.null(current)) {
        current <- length(plot$row_anchor)
    } else {
        if (!current %in% names(plot$row_anchor)) {
            stop(current, " can't be found")
        }
    }

    # side: left / right; top/bottom
    side <- object$stat_params$side
    extend_x <- object$stat_params$extend_x
    extend_y <- object$stat_params$extend_y

    # default mapping & data
    if (side %in% c("left", "right")) {
        df <- plot$row_anchor[[current]]
        object$data <- data.frame(
            minX = unique(df$minX),
            maxX = unique(df$maxX),
            minY = min(df$y - 0.5*df$h, na.rm = TRUE) - extend_y[1],
            maxY = max(df$y + 0.5*df$h, na.rm = TRUE) + extend_y[2])

        if (side == "left") {
            self_mapping <- aes_string(x = "minX", y = "maxY")
        } else {
            self_mapping <- aes_string(x = "maxX", y = "maxY")
        }


    } else {
        df <- plot$col_anchor[[current]]
        object$data <- data.frame(
            minX = min(df$x - 0.5*df$w) - extend_x[1],
            maxX = max(df$x + 0.5*df$w) + extend_x[2],
            minY = unique(df$minY),
            maxY = unique(df$maxY))
        if (side == "top") {
            self_mapping <- aes_string(x = "maxX", y = "maxY")
        } else {
            self_mapping <- aes_string(x = "maxX", y = "minY")
        }
    }

    if (is.null(object$mapping)) {
        object$mapping <- self_mapping
    } else {
        object$mapping <- modifyList(self_mapping, object$mapping)
    }

    NextMethod()
}

#' add the border line of a heatmap
#'
#' add a line to separate heatmaps
#'
#' @param name the name of the heatmap to add row or column labels.
#' @param side a character value selected from \strong{left}, \strong{right},
#'   \strong{top} or \strong{bottom}. \strong{left} and \strong{right} to add
#'   the vertical line; \strong{top} or \strong{bottom} to add the horizontal
#'   line.
#' @param nudge_x a value to shift the border line horizontally.
#' @param nudge_y a value to shift the border line vertically.
#' @param extend_x a numeric vector with two elements. For example, \code{c(1,
#'   2)} would extend the left end of the line by 1 and the right end by 2.
#' @param extend_y a numeric vector with two elements. For example, \code{c(1,
#'   2)} would extend the bottom end of the line by 1 and the top end by 2.
#' @param label a character to label the border line
#' @param label_size the font size of the label.
#' @param label_color the color of the label.
#' @param label_offset_x a value to shift the label horizontally.
#' @param label_offset_y a value to shift the label vertically.
#' @inheritParams ggplot2::geom_segment
#' @seealso \code{\link{geom_th_borderline}} \code{\link{geom_th_bordertext}}
#' @import ggplot2
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_border <- function(name, side,
                           nudge_x = 0, nudge_y = 0,
                           extend_y = c(0, 0),
                           extend_x = c(0, 0),
                           label = NULL,
                           label_color = "black",
                           label_size = 4,
                           label_offset_x = 0,
                           label_offset_y =0,
                           ...){
    list(
        geom_th_borderline(name = name, side = side,
                           nudge_x = nudge_x,
                           nudge_y = nudge_y,
                           extend_x = extend_x,
                           extend_y = extend_y,
                           ...),
        geom_th_bordertext(name = name, side = side, color = label_color,
                           nudge_x = nudge_x + label_offset_x,
                           nudge_y = nudge_y + label_offset_y,
                           extend_x = extend_x,
                           extend_y = extend_y,
                           label = label,
                           size = label_size)
    )
}
