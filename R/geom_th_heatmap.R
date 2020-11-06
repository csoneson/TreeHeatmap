#' StatTHtile
#' @format NULL
#' @usage NULL
#' @import ggplot2
#' @export
StatTHtile <- ggproto("StatTHtile", Stat,
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
#' @param hclust_method the method used to do clustering. This should be
#'   "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median"
#'   or "centroid". See \code{method} for \code{\link[stats]{hclust}}.
#' @param dist_method the method used to do clustering. This must be one of
#'   "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#'   See \code{method} for \code{\link[stats]{dist}}.
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
geom_th_heatmap0 <- function(th_data = NULL,
                           cluster_column = FALSE,
                           hclust_method = "complete",
                           dist_method = "euclidean",
                           gap = 1,
                           name = NULL,
                           rel_width = 1,
                           ...,
                           linejoin = "mitre",
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = FALSE) {
    new_layer <- layer(
        stat = StatTHtile, data = NULL, mapping = NULL, geom = "tile",
        position = "identity", show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gap = gap, name = name,
                      th_data = th_data, rel_width = rel_width,
                      cluster_column = cluster_column,
                      hclust_method = hclust_method,
                      dist_method = dist_method, ...)
    )
    class(new_layer) <- c("ggTHtile", class(new_layer))
    new_layer
}

#' @method ggplot_add ggTHtile
#' @importFrom dplyr mutate select distinct '%>%'
#' @importFrom methods is
#' @importFrom rlang .data
#' @importFrom utils modifyList
#' @importFrom stats dist hclust
#' @importFrom ggdendro dendro_data
#' @import ggplot2
#' @export
ggplot_add.ggTHtile <- function(object, plot, object_name) {
    if (length(plot$col_anchor) != length(plot$row_anchor)) {
        stop("The anchor data has different lengths...")
    }

    # the global data
    main_data <- plot$data
    if(is(main_data, "waiver")) {
        stop("ggtree can't be found.")
    }

    # the previous & current treeheatmap
    previous <- length(plot$heatmap)
    current <- object$stat_params$name
    if (is.null(current)) {
        current <- previous + 1
    }

    # if the anchor data doesn't exist, start slots to store the anchor data
    # tmpX: the upper x of the previous 'geom_th_heatmap'
    if (!previous) {
        plot$heatmap <- list()
        # plot$heatmap$col_anchor <- plot$heatmap$row_anchor  <- list()
        tmpX <- max(main_data$x, na.rm = TRUE)
    } else {
        tmpX <- unique(plot$heatmap[[previous]]$row_anchor$maxX)
        #tmpX <- unique(plot$row_anchor[[previous]]$maxX)
    }

    # if the data of 'geom_th_heatmp' not available, generate it from th_data
    if (is(object$data, "waiver")) {
        # to cluster columns according to similarities
        cluster_column <- object$stat_params$cluster_column
        hclust_method <- object$stat_params$hclust_method
        dist_method <- object$stat_params$dist_method
        th_data <- object$stat_params$th_data
        rel_width <- object$stat_params$rel_width
        if (cluster_column) {
            dd <- dist(t(th_data), method = dist_method)
            tree_col <- hclust(dd, method = hclust_method)
            dendr <- dendro_data(tree_col, type="rectangle")
            th_data <- th_data[, colnames(th_data)[tree_col$order]]
        }
        object$data <- th_data %>%
            .align_to_ggtree(data_tree = main_data,
                             rel_width = rel_width)
        # the layer data starts x from tmpX
        object$data$x <- object$data$x + tmpX
    }

    # the gap to the previous 'geom_th_heatmp'
    gap <- object$stat_params$gap

    # store the heatmap data
    plot$heatmap[[current]]$data <- object$data %>%
        mutate(x = .data$x + gap)

    ## store the anchor data
    # plot$row_anchor[[current]] <- object$data %>%
    plot$heatmap[[current]]$row_anchor <- object$data %>%
        mutate(minX = min(.data$x - 0.5*.data$w, na.rm = TRUE) + gap,
               maxX = max(.data$x + 0.5*.data$w, na.rm = TRUE) + gap,
               rowLab = .data$rowLab) %>%
        select(.data$rowLab, .data$y, .data$minX, .data$maxX, .data$h) %>%
        distinct()

    # plot$col_anchor[[current]] <- object$data %>%
    plot$heatmap[[current]]$col_anchor <- object$data %>%
        mutate(minY = min(.data$y - 0.5*.data$h, na.rm = TRUE),
               maxY = max(.data$y + 0.5*.data$h, na.rm = TRUE),
               colLab = .data$colLab,
               x = .data$x + gap) %>%
        select(.data$colLab, .data$x, .data$minY, .data$maxY, .data$w) %>%
        distinct()

    ## store the column tree data
    if (cluster_column) {
        # adapt the tree position
        ww <- unique(object$data$w, na.rm = TRUE)
        min_xx <- min(object$data$x, na.rm = TRUE)
        df_tree <- dendr$segments %>%
            left_join(dendr$labels,
                      by = c("xend" = "x", "yend" = "y")) %>%
            mutate(x = ww * (.data$x-1) + min_xx + gap,
                   xend = ww * (.data$xend-1) + min_xx + gap)
    } else {
         df_tree <- NULL
     }
    # plot$col_tree[[current]] <- df_tree
    plot$heatmap[[current]]$col_tree <- df_tree

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


#' Create heatmaps
#' @param name the name of the current heatmap
#' @param th_data a matrix
#' @param cluster_column a logical value, TRUE or FALSE. If TRUE, columns of the
#'   heatmap are rearranged according to their similarities.
#' @param hclust_method the method used to do clustering. This should be
#'   "ward.D", "ward.D2", "single", "complete", "average", "mcquitty", "median"
#'   or "centroid". See \code{method} for \code{\link[stats]{hclust}}.
#' @param dist_method the method used to do clustering. This must be one of
#'   "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#'   See \code{method} for \code{\link[stats]{dist}}.
#' @param gap a numeric value to specify the gap between the current and the
#'   previous heatmap
#' @param rel_width a numeric value decide the relative width of the heatmap
#'   compared to the ggtree plot
#' @param show_coltree TRUE or FALSE. If TRUE, the column tree is shown when
#'   \code{cluster_column = TRUE}
#' @param gap_coltree a numeric value to decide the gap between the heatmap and
#'   the column tree
#' @param rel_height_coltree the relative height of the column tree compared to
#'   the height of row tree.
#' @param color_coltree the color of the column tree.
#' @param size_coltree the line size of the column tree
#' @param ... Other arguments passed on to \code{\link[ggplot2]{geom_tile}} to
#'   customize the heatmap.
#' @import ggplot2
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_th_heatmap <- function(name = NULL,
                            th_data = NULL,
                            cluster_column = FALSE,
                            hclust_method = "complete",
                            dist_method = "euclidean",
                            gap = 1,
                            rel_width = 1,
                            show_coltree = TRUE,
                            gap_coltree = 0.5,
                            rel_height_coltree = 0.1,
                            color_coltree = "grey",
                            size_coltree = 1,
                            ...) {

    out <- list(
        geom_th_heatmap0(name = name,
                         th_data = th_data,
                         cluster_column = cluster_column,
                         hclust_method = hclust_method,
                         dist_method = dist_method,
                         gap = gap,
                         rel_width = rel_width,
                         ...)
    )

    if (cluster_column & show_coltree) {
        out <- c(out,
                 list(geom_th_coltree(name = name,
                                      gap = gap_coltree,
                                      rel_height = rel_height_coltree,
                                      color = color_coltree,
                                      size = size_coltree)))
    }
    out
}
