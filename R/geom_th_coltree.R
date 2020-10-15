#' StatTHtree
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
StatTHtree<- ggproto("StatTHtree", Stat,
                      compute_group = function(data, th_data, scales,
                                               gap, name, rel_height) {
                          data$y <- data$y + gap
                          data$yend <- data$yend + gap
                          data
                      }
)

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
                            gap = 1,
                            name = NULL,
                            rel_height = 0.1,
                            ...) {
    new_layer <- layer(
        stat = StatTHtree, data = data, mapping = mapping, geom = "segment",
        position = "identity", show.legend = FALSE, inherit.aes = FALSE,
        params = list(gap = gap, name = name, rel_height = rel_height, ...)
    )
    class(new_layer) <- c("ggTHtree", class(new_layer))
    new_layer
}

#' @method ggplot_add ggTHtree
#' @import ggplot2
#' @importFrom methods is
#' @importFrom utils modifyList
#' @importFrom rlang .data
#' @importFrom dplyr '%>%' mutate
#' @export

ggplot_add.ggTHtree <- function(object, plot, object_name) {


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

    # extract the tree data
    df_tree <- plot$col_tree[[current]]
    if (is.null(df_tree)) {
        stop("the column tree can't be found.
             Try to set 'cluster_column = TRUE' in the geom_th_heatmp.")}

    # adapt the height of the tree
    rel_height <- object$stat_params$rel_height
    max_yy <- unique(plot$col_anchor[[current]]$maxY, na.rm = TRUE)

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


