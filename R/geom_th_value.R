
#' show values in the heatmap
#' @param th_data NULL or a data frame. Default is NULL. If a data frame is
#'   provided, then it should include at least one column \code{rowLab} or
#'   \code{colLab} that stores the row/column names of the heatmap.
#' @param name the name of the heatmap to add row or column labels.
#' @param subset a logical vector to subset rows or columns to add values
#' @param nudge_x a value to shift the text horizontally.
#' @param nudge_y a value to shift the text vertically.
#' @inheritParams ggplot2::geom_text
#' @import ggplot2
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_addvalue <- function(mapping = NULL,
                             th_data = NULL,
                             name = NULL,
                             subset = NULL,
                             nudge_x = 0,
                             nudge_y = 0,
                             na.rm = FALSE,
                             show.legend = NA,
                             inherit.aes = FALSE,
                             ...) {

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
                     nudge_x = nudge_x, nudge_y = nudge_y)

    ggproto("ggTHvalue", new_layer, th_params = th_params)
}

#' @method ggplot_add ggTHvalue
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @importFrom dplyr '%>%' left_join
#' @export

ggplot_add.ggTHvalue <- function(object, plot, object_name) {

    # the active layer of ggheat
    current <- .current_heatmap(plot = plot, object = object)

    # the input data
    th_data <- object$th_params$th_data
    if (!is.null(th_data)) {
        th_cname <- colnames(th_data)
        if (!any(th_cname %in% c("rowLab", "colLab"))) {
            stop("Don't know how to join data.
                 'rowLab' or/and 'colLab' is/are required.")
        }
        object$data <- .hm_data(plot, current) %>%
            left_join(th_data)
    } else {
        object$data <- .hm_data(plot, current)
    }

    self_mapping <- aes_string(x = "x", y = "y", label = "value")

    if (is.null(object$mapping)) {
        object$mapping <- self_mapping
    } else {
        object$mapping <- modifyList(self_mapping, object$mapping)
    }

    NextMethod()
}
