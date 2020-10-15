
#' The row or column title
#'
#' add the row or column title
#'
#' @param name the name of the heatmap to add row or column labels.
#' @param side a character value selected from \strong{left}, \strong{right},
#'   \strong{top} or \strong{bottom}. \strong{left} and \strong{right} to
#'   add the row title; \strong{top} or \strong{bottom} to add the column
#'   title.
#' @param nudge_x a value to shift the title horizontally.
#' @param nudge_y a value to shift the title vertically.
#' @inheritParams ggplot2::geom_text
#' @import ggplot2
#' @importFrom dplyr left_join
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_title <- function(mapping = NULL,
                           name = NULL,
                           side = "left",
                           nudge_x = 0,
                           nudge_y = 0,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           ...) {


    .annotate_layer(mapping = mapping, th_data = NULL,
                    data = NULL, name = name,
                    subset = NULL, side = side,
                    nudge_x = nudge_x, nudge_y = nudge_y,
                    extend_x = c(0, 0), extend_y = c(0, 0),
                    na.rm = na.rm, show.legend = show.legend,
                    inherit.aes = inherit.aes,
                    geom = "text", stat = StatTH,
                    new_class = "ggTHtitle",
                    ...)

}

#' @method ggplot_add ggTHtitle
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @export

ggplot_add.ggTHtitle <- function(object, plot, object_name) {


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

    # default mapping & data
    if (side %in% c("left", "right")) {
        df <- plot$row_anchor[[current]]
        object$data <- data.frame(
            minX = unique(df$minX) - 1,
            y = mean(range(df$y, na.rm = TRUE)),
            maxX = unique(df$maxX) + 1)

        if (side == "left") {
            self_mapping <- aes_string(x = "minX", y = "y")
        } else {
            self_mapping <- aes_string(x = "maxX", y = "y")
        }


    } else {
        df <- plot$col_anchor[[current]]
        object$data <- data.frame(
            x = mean(range(df$x, na.rm = TRUE)),
            minY = unique(df$minY - 1, na.rm = TRUE),
            maxY = max(df$maxY + 1, na.rm = TRUE))
        if (side == "top") {
            self_mapping <- aes_string(x = "x", y = "maxY")
        } else {
            self_mapping <- aes_string(x = "x", y = "minY")
        }
    }

    if (is.null(object$mapping)) {
        object$mapping <- self_mapping
    } else {
        object$mapping <- modifyList(self_mapping, object$mapping)
    }

    NextMethod()
}


