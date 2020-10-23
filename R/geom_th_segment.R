

#' add row or column labels
#' @param th_data a data frame. It should include at least one column
#'   \code{label} that stores the row/column names of the heatmap.
#' @param name the name of the heatmap to add row or column labels.
#' @param side a character value selected from \strong{left}, \strong{right},
#'   \strong{top} or \strong{bottom}. \strong{left} and \strong{right} to
#'   annotate row names; \strong{top} or \strong{bottom} to annotate column
#'   names.
#' @param subset a logical vector to specify rows or columns to add labels
#' @param nudge_x a value to shift the segment line horizontally.
#' @param nudge_y a value to shift the segment line vertically.
#' @inheritParams ggplot2::geom_segment
#' @import ggplot2
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_segment <- function(mapping = NULL,
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

    side <- match.arg(side, c("left", "right", "top", "bottom"))

    position <- position_nudge(nudge_x, nudge_y)
    StatTH <- allow_subset_stat("StatTH", Stat)
    new_layer <- layer(
        mapping = mapping, data = NULL, geom = "segment",
        stat = StatTH, position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, subset = subset, ...)
    )

    th_params <- list(name = name, side = side,
                     th_data = th_data,
                     nudge_x = nudge_x, nudge_y = nudge_y)

    ggproto("ggTHsegment", new_layer, th_params = th_params)

    # .annotate_layer(mapping = mapping, th_data = th_data,
    #                 data = data, name = name,
    #                 subset = subset, side = side,
    #                 nudge_x = nudge_x, nudge_y = nudge_y,
    #                 extend_x = c(0, 0), extend_y = c(0, 0),
    #                 na.rm = na.rm, show.legend = show.legend,
    #                 inherit.aes = inherit.aes,
    #                 geom = "segment", stat = StatTH,
    #                 new_class = "ggTHsegment",
    #                 ...)
}


#' @method ggplot_add ggTHsegment
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @importFrom dplyr '%>%' distinct select left_join mutate
#' @importFrom rlang .data
#' @export

ggplot_add.ggTHsegment <- function(object, plot, object_name) {

    # the active layer of ggheat
    current <- .current_heatmap(plot = plot, object = object)

    # side: left / right; top/bottom
    side <- object$th_params$side
    th_data <- object$th_params$th_data

    # default mapping & data
    if (side %in% c("left", "right")) {
        if (!is.null(th_data)) {
            df <- .row_anchor(plot, current) %>%
                left_join(th_data, by = "label")
        } else {
            df <- .row_anchor(plot, current)
        }


        if (side == "left") {
            object$data <- df %>%
                mutate(minY = .data$y - 0.5*.data$h,
                       maxY = .data$y + 0.5*.data$h)
            self_mapping <- aes_string(x = "minX", y = "minY",
                                       xend = "minX", yend = "maxY")
        } else {
            object$data <- df %>%
                mutate(minY = .data$y - 0.5*.data$h,
                       maxY = .data$y + 0.5*.data$h)
            self_mapping <- aes_string(x = "maxX", y = "minY",
                                       xend = "maxX", yend = "maxY")
        }
    } else {
        if (!is.null(th_data)) {
            df <- .col_anchor(plot, current) %>%
                left_join(th_data, by = "label")
        } else {
            df <- .col_anchor(plot, current)
        }

        if (side == "top") {
            object$data <- df %>%
                mutate(minX = .data$x - 0.5*.data$w,
                       maxX = .data$x + 0.5*.data$w)
            self_mapping <- aes_string(x = "minX", y = "maxY",
                                       xend = "maxX", yend = "maxY")
        } else {
            object$data <- df %>%
                mutate(minX = .data$x - 0.5*.data$w,
                       maxX = .data$x + 0.5*.data$w)
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



