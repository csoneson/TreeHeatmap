#' add the boundary line of a heatmap
#'
#' add a line to separate heatmaps
#'
#' @param name the name of the heatmap to add row or column labels.
#' @param side a character value selected from \strong{left}, \strong{right},
#'   \strong{top} or \strong{bottom}. \strong{left} and \strong{right} to add
#'   the vertical line; \strong{top} or \strong{bottom} to add the horizontal
#'   line.
#' @param gap the gap between the border line and the heatmap
#' @param extend_x a numeric vector with two elements. For example, \code{c(1,
#'   2)} would extend the left end of the line by 1 and the right end by 2.
#' @param extend_y a numeric vector with two elements. For example, \code{c(1,
#'   2)} would extend the bottom end of the line by 1 and the top end by 2.
#' @inheritParams ggplot2::geom_segment
#' @import ggplot2
#' @importFrom dplyr left_join
#' @keywords internal
#' @return geom layer
#' @author Ruizhu Huang
geom_th_borderline <- function(mapping = NULL,
                           name = NULL,
                           side = "left",
                           gap = 0.5,
                           extend_x = c(0, 0),
                           extend_y = c(0, 0),
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {

    side <- match.arg(side, c("left", "right", "top", "bottom"))

    if (side %in% c("left", "right")) {
        gap_x <- ifelse(side == "left", -gap, gap)
        position <- position_nudge(x = gap_x)
    } else {
        gap_y <- ifelse(side == "bottom", -gap, gap)
        position <- position_nudge(y = gap_y)
    }
    StatTH <- allow_subset_stat("StatTH", Stat)
    new_layer <- layer(
        mapping = mapping, data = NULL, geom = "segment",
        stat = StatTH, position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, subset = subset, ...)
    )

    th_params <- list(name = name, side = side,
                     extend_x = extend_x,
                     extend_y = extend_y)

    ggproto("ggTHborder", new_layer, th_params = th_params)

}



#' @method ggplot_add ggTHborder
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @export

ggplot_add.ggTHborder <- function(object, plot, object_name) {

    # the active layer of ggheat
    current <- .current_heatmap(plot = plot, object = object)

    # side: left / right; top/bottom
    side <- object$th_params$side
    extend_x <- object$th_params$extend_x
    extend_y <- object$th_params$extend_y

    # default mapping & data
    if (side %in% c("left", "right")) {
        df <- .row_anchor(plot, current)
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
        df <- .col_anchor(plot, current)
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
#' @param name the name of the heatmap
#' @param side a character value selected from \strong{left}, \strong{right},
#'   \strong{top} or \strong{bottom}. \strong{left} and \strong{right} to add
#'   the vertical line; \strong{top} or \strong{bottom} to add the horizontal
#'   line.
#' @param nudge_x a value to shift the text horizontally.
#' @param nudge_y a value to shift the text vertically.
#' @inheritParams ggplot2::geom_text
#' @import ggplot2
#' @importFrom dplyr left_join
#' @keywords internal
#' @return geom layer
#' @author Ruizhu Huang
geom_th_bordertext <- function(mapping = NULL,
                               name = NULL,
                               side = "left",
                               gap = 0.5,
                               extend_x = c(0, 0),
                               extend_y = c(0, 0),
                               na.rm = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               label_offset_x = 0,
                               label_offset_y = 0,
                               ...) {

    side <- match.arg(side, c("left", "right", "top", "bottom"))

    if (side %in% c("left", "right")) {
        gap_x <- ifelse(side == "left", label_offset_x - gap,
                        label_offset_x + gap)
        position <- position_nudge(x = gap_x)
    } else {
        gap_y <- ifelse(side == "bottom", label_offset_y - gap,
                        label_offset_y - gap)
        position <- position_nudge(y = gap_y)
    }

    StatTH <- allow_subset_stat("StatTH", Stat)
    new_layer <- layer(
        mapping = mapping, data = NULL, geom = "text",
        stat = StatTH, position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, subset = subset, ...)
    )

    th_params <- list(name = name, side = side,
                      extend_x = extend_x,
                      extend_y = extend_y)

    ggproto("ggTHbordertext", new_layer, th_params = th_params)


}



#' @method ggplot_add ggTHbordertext
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @export

ggplot_add.ggTHbordertext <- function(object, plot, object_name) {

    # the active layer of ggheat
    current <- .current_heatmap(plot = plot, object = object)

    # side: left / right; top/bottom
    side <- object$th_params$side
    extend_x <- object$th_params$extend_x
    extend_y <- object$th_params$extend_y

    # default mapping & data
    if (side %in% c("left", "right")) {
        df <- .row_anchor(plot, current)
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
        df <- .col_anchor(plot, current)
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

#' add the border line
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
                           gap = 0.5,
                           extend_y = c(0, 0),
                           extend_x = c(0, 0),
                           label_color = "black",
                           label_size = 4,
                           label_offset_x = 0,
                           label_offset_y = 0,
                           label = "",
                           ...){

    list(
        geom_th_borderline(name = name, side = side,
                           gap = gap,
                           extend_x = extend_x,
                           extend_y = extend_y,
                           ...),
        geom_th_bordertext(name = name, side = side,
                           color = label_color,
                           gap = gap,
                           extend_x = extend_x,
                           extend_y = extend_y,
                           label = label,
                           size = label_size,
                           label_offset_x = label_offset_x,
                           label_offset_y = label_offset_y)
    )
}
