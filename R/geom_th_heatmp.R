#' StatHeatmp
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
StatHeatmp <- ggproto("StatHeatmp", Stat,
                      compute_group = function(data, th_data, scales,
                                               gap, name, rel_width,
                                               cluster_column,
                                               hclust_method,
                                               dist_method) {
                          data$x <- data$x + gap
                          data
                      },
                      required_aes = c("x", "y")
)

#' Create heatmaps
#' @param name the name of the current heatmap
#' @param th_data a matrix
#' @param cluster_column a logical value, TRUE or FALSE. If TRUE, columns of the
#'   heatmap are rearranged according to their similarities.
#' @param hclust_method the method used to do clustering. See \code{method} for
#'   \code{\link[stats]{hclust}}.
#' @param dist_method the method used to do clustering. See \code{method} for
#'   \code{\link[stats]{dist}}.
#' @param gap a numeric value to specify the gap between the current and the
#'   previous heatmap
#' @param rel_width a numeric value decide the relative width of the heatmap
#'   compared to the ggtree plot
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_tile
#' @import ggplot2
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_heatmp <- function(mapping = NULL,
                           th_data = NULL,
                           cluster_column = FALSE,
                           hclust_method = "complete",
                           dist_method = "euclidean",
                           data = NULL,
                           position = "identity",
                           gap = 1,
                           name = NULL,
                           rel_width = 1,
                           ...,
                           linejoin = "mitre",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE) {
    new_layer <- layer(
        stat = StatHeatmp, data = data, mapping = mapping, geom = "tile",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gap = gap, name = name,
                      th_data = th_data, rel_width = rel_width,
                      cluster_column = cluster_column,
                      hclust_method = hclust_method,
                      dist_method = dist_method, ...)
    )
    class(new_layer) <- c("ggHeatmp", class(new_layer))
    new_layer
}

#' @method ggplot_add ggHeatmp
#' @importFrom dplyr mutate select distinct '%>%'
#' @importFrom methods is
#' @importFrom rlang .data
#' @importFrom utils modifyList
#' @importFrom stats dist hclust
#' @import ggplot2
#' @export
ggplot_add.ggHeatmp <- function(object, plot, object_name) {
    if (length(plot$col_anchor) != length(plot$row_anchor)) {
        stop("The anchor data has different lengths...")
    }

    # the global data
    main_data <- plot$data

    if(is(main_data, "waiver")) {
        stop("ggtree can't be found.")
    }

    # the previous & current treeheatmap
    previous <- length(plot$col_anchor)
    current <- object$stat_params$name
    if (is.null(current)) {
        current <- previous + 1
    }

    # if the anchor data doesn't exist, start slots to store the anchor data
    # tmpX: the upper x of the previous 'geom_th_heatmp'
    if (!previous) {
        plot$col_anchor <- plot$row_anchor <- list()
        tmpX <- max(main_data$x, na.rm = TRUE)
    } else {
        tmpX <- unique(plot$row_anchor[[previous]]$maxX)
    }

    # if the data of 'geom_th_heatmp' not available, generate it from th_data
    if (is(object$data, "waiver")) {
        # to cluster columns according to similarities
        cluster_column <- object$stat_params$cluster_column
        hclust_method <- object$stat_params$hclust_method
        dist_method <- object$stat_params$dist_method
        th_data <- object$stat_params$th_data

        if (cluster_column) {
            dd <- dist(t(th_data))
            xx <- hclust(dd, hclust_method)
            th_data <- th_data[, colnames(th_data)[xx$order]]
        }
        object$data <- th_data %>%
            .align_to_ggtree(data_tree = main_data,
                             rel_width = object$stat_params$rel_width)

        # the layer data starts x from tmpX
        object$data$x <- object$data$x + tmpX
    }

    # the gap to the previous 'geom_th_heatmp'
    gap <- object$stat_params$gap
    plot$row_anchor[[current]] <- object$data %>%
        mutate(minX = min(.data$x - 0.5*.data$w, na.rm = TRUE) + gap,
               maxX = max(.data$x + 0.5*.data$w, na.rm = TRUE) + gap,
               label = .data$rowLab) %>%
        select(.data$label, .data$y, .data$minX, .data$maxX, .data$h) %>%
        distinct()

    plot$col_anchor[[current]] <- object$data %>%
        mutate(minY = min(.data$y - 0.5*.data$h, na.rm = TRUE),
               maxY = max(.data$y + 0.5*.data$h, na.rm = TRUE),
               label = .data$colLab,
               x = .data$x + gap) %>%
        select(.data$label, .data$x, .data$minY, .data$maxY, .data$w) %>%
        distinct()

    # mapping
    self_mapping <- aes_string(x = "x", y = "y", width = "w",
                               height = "h", fill = "value")
    if (is.null(object$mapping)) {
        object$mapping <- self_mapping
    } else {
        object$mapping <- modifyList(self_mapping, object$mapping)
    }

    NextMethod()
}

