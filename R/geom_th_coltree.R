#' add the column tree
#' @param name the name of the heatmap
#' @param gap a numeric value to specify the gap between the column tree and the
#'   heatmap
#' @param rel_height a numeric value decide the relative height of the column
#'   tree compared to the height of the heatmap
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_segment
#' @import ggplot2
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_coltree <- function(mapping = NULL,
                            data = NULL,
                            gap = 0.5,
                            name = NULL,
                            rel_height = 0.1,
                            ...) {

    position <- position_nudge(y = gap)
    new_layer <- layer(
        stat = "identity", data = data, mapping = mapping, geom = "segment",
        position = position, show.legend = FALSE, inherit.aes = FALSE,
        params = list(...)
    )
    th_params <- list(gap = gap, name = name, rel_height = rel_height)
    ggproto("ggTHtree", new_layer, th_params = th_params)
}

#' @method ggplot_add ggTHtree
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @importFrom rlang .data
#' @importFrom dplyr '%>%' mutate
#' @export

ggplot_add.ggTHtree <- function(object, plot, object_name) {

    # the active layer of ggheat
    current <- .current_heatmap(plot = plot, object = object)

    # extract the tree data
    df_tree <- .col_tree(plot, current)
    if (is.null(df_tree)) {
        stop("the column tree can't be found.
             Try to set 'cluster_column = TRUE' in the geom_th_heatmp.")}

    # adapt the height of the tree
    rel_height <- object$th_params$rel_height
    max_yy <- unique(.col_anchor(plot, current)$maxY, na.rm = TRUE)

    object$data <- df_tree %>%
        mutate(y = rel_height * .data$y + max_yy,
               yend = rel_height * .data$yend + max_yy)

    # mapping
    self_mapping <- aes_string(x = "x", y = "y", xend = "xend", yend = "yend")
    if (is.null(object$mapping)) {
        object$mapping <- self_mapping
    } else {
        object$mapping <- modifyList(self_mapping, object$mapping)
    }

    NextMethod()
}


