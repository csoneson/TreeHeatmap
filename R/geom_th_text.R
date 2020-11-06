
#' add row or column labels
#' @param th_data a data frame. It should include one column named as
#'   \code{rowLab} to store the row name of the heatmap when \code{side} is
#'   \strong{left} or \strong{right}; otherwise, it should include one column
#'   named as \code{colLab} to store the column name of the heatmap when
#'   \code{side} is \strong{top} or \strong{bottom}.
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
                         name = NULL,
                         th_data = NULL,
                         subset = NULL,
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
        params = list(na.rm = na.rm, subset = subset, ...)
    )
    th_params <- list(name = name, th_data = th_data,
                      side = side)

    ggproto("ggTHtext", new_layer, th_params = th_params)
}
#' @method ggplot_add ggTHtext
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @importFrom dplyr '%>%' distinct select
#' @export

ggplot_add.ggTHtext <- function(object, plot, object_name) {

    # the active layer of ggheat
    current <- .current_heatmap(plot = plot, object = object)

    # side: left / right; top/bottom
    side <- object$th_params$side
    th_data <- object$th_params$th_data

    # default mapping & data
    if (side %in% c("left", "right")) {
        if (!is.null(th_data)) {
            object$data <- .row_anchor(plot, current) %>%
                left_join(th_data, by = "rowLab")
        } else {
            object$data <- .row_anchor(plot, current)
        }


        if (side == "left") {
            self_mapping <- aes_string(x = "minX", y = "y", label = "rowLab")
        } else {
            self_mapping <- aes_string(x = "maxX", y = "y", label = "rowLab")
        }
    } else {
        if (!is.null(th_data)) {
            object$data <- .col_anchor(plot, current) %>%
                left_join(th_data, by = "colLab")
        } else {
            object$data <- .col_anchor(plot, current)
        }

        if (side == "top") {
            self_mapping <- aes_string(x = "x", y = "maxY", label = "colLab")
        } else {
            self_mapping <- aes_string(x = "x", y = "minY", label = "colLab")
        }
    }

    if (is.null(object$mapping)) {
        object$mapping <- self_mapping
    } else {
        object$mapping <- modifyList(self_mapping, object$mapping)
    }

    NextMethod()
}
