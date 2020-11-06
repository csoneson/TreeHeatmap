
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
                           inherit.aes = FALSE,
                           ...) {

    side <- match.arg(side, c("left", "right", "top", "bottom"))

    position <- position_nudge(nudge_x, nudge_y)

    StatTH <- allow_subset_stat("StatTH", Stat)
    new_layer <- layer(
        mapping = mapping, data = NULL, geom = "text",
        stat = StatTH, position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
    th_params <- list(name = name, side = side,
                      nudge_x = nudge_x, nudge_y = nudge_y)

    ggproto("ggTHtitle", new_layer, th_params = th_params)
}

#' @method ggplot_add ggTHtitle
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @export

ggplot_add.ggTHtitle <- function(object, plot, object_name) {

    # the active layer of ggheat
    current <- .current_heatmap(plot = plot, object = object)

    # side: left / right; top/bottom
    side <- object$th_params$side

    # default mapping & data
    if (side %in% c("left", "right")) {
        df <- .row_anchor(plot, current)
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
        df <- .col_anchor(plot, current)
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


