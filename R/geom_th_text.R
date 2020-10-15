
#' add row or column labels
#' @param th_data a data frame. It should include at least one column
#'   \code{label} that stores the row/column names of the heatmap.
#' @param name the name of the heatmap to add row or column labels.
#' @param side a character value selected from \strong{left}, \strong{right},
#'   \strong{top} or \strong{bottom}. \strong{left} and \strong{right} to
#'   annotate row names; \strong{top} or \strong{bottom} to annotate column
#'   names.
#' @param subset a logical vector to specify rows or columns to add labels
#' @param nudge_x a value to shift the text horizontally.
#' @param nudge_y a value to shift the text vertically.
#' @inheritParams ggplot2::geom_text
#' @import ggplot2
#' @importFrom dplyr left_join
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_text <- function(mapping = NULL,
                         th_data = NULL,
                         data = NULL,
                         name = NULL,
                         subset = NULL,
                         side = "left",
                         nudge_x = 0,
                         nudge_y = 0,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE,
                         ...) {

    .annotate_layer(mapping = mapping, th_data = th_data,
                    data = data, name = name,
                    subset = subset, side = side,
                    nudge_x = nudge_x, nudge_y = nudge_y,
                    extend_x = c(0, 0), extend_y = c(0, 0),
                    na.rm = na.rm, show.legend = show.legend,
                    inherit.aes = inherit.aes,
                    geom = "text", stat = StatTH,
                    new_class = "ggTHtext",
                    ...)

}

#' @method ggplot_add ggTHtext
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @importFrom dplyr '%>%' distinct select
#' @export

ggplot_add.ggTHtext <- function(object, plot, object_name) {


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
    th_data <- object$stat_params$th_data

    # default mapping & data
    if (side %in% c("left", "right")) {
        if (!is.null(th_data)) {
            object$data <- plot$row_anchor[[current]] %>%
                left_join(th_data, by = "label")
        } else {
            object$data <- plot$row_anchor[[current]]
        }


        if (side == "left") {
            self_mapping <- aes_string(x = "minX", y = "y", label = "label")
        } else {
            self_mapping <- aes_string(x = "maxX", y = "y", label = "label")
        }
    } else {
        if (!is.null(th_data)) {
        object$data <- plot$col_anchor[[current]] %>%
            left_join(th_data, by = "label")
        } else {
            object$data <- plot$col_anchor[[current]]
        }

        if (side == "top") {
            self_mapping <- aes_string(x = "x", y = "maxY", label = "label")
        } else {
            self_mapping <- aes_string(x = "x", y = "minY", label = "label")
        }
    }

    if (is.null(object$mapping)) {
        object$mapping <- self_mapping
    } else {
        object$mapping <- modifyList(self_mapping, object$mapping)
    }

    NextMethod()
}


