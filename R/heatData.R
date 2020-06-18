# hm_data <- dd
# ggtree_data <- fig_row$data
# tree <- rowT
# split_column <- setNames(rep(LETTERS[1:2], each = 4), colnames(dd))
# split_gap <- 1
# order_column <- NULL
# cluster_column <- FALSE

# heatData <- function(ggtree_data, hm_data,
#                      rel_width = 0.5,
#                      split_column = NULL,
#                      split_gap = 0.5,
#                      order_column = NULL,
#                      cluster_column = FALSE,
#                      w = 1, h = 1,
#                      hclust_method = "ave",
#                      dist_method = "euclidean",
#                      fig_coltree = NULL){
#
#     # a tree
#     tree <- as.phylo(ggtree_data)
#
#     # heatmap: data (in the wide form)
#     wide_data <- hm_data <- data.frame(hm_data, check.names = FALSE)
#     hm_rn <- rownames(hm_data)
#     hm_cn <- colnames(hm_data)
#
#     if (is.null(hm_rn) | is.null(hm_cn)) {
#         stop("row/column names are required ...")
#     }
#
#     # rows <-> nodes
#     hm_node <- transNode(tree = tree, node = hm_rn)
#     wide_data$node <- hm_node
#     wide_data$rowLab <- hm_rn
#
#     # heatmap: y
#     desd_row_tbl <- offspring(ggtree_data, hm_node,
#                              tiponly = FALSE, self_include = TRUE)
#     desd_row <- lapply(desd_row_tbl, function(x){x$node})
#
#     y_row <- lapply(desd_row, FUN = function(x){
#         xx <- match(x, ggtree_data$node)
#         y <- ggtree_data$y[xx]
#         # the middle point
#         mean(range(y, na.rm = TRUE))
#     })
#     wide_data$y <- unlist(y_row)
#
#     # heatmap: height of a row
#     h_row <- lapply(desd_row, FUN = function(x){
#         xx <- match(x, ggtree_data$node)
#         y <- ggtree_data$y[xx]
#
#         cy <- colnames(ggtree_data)
#         if ("scale" %in% cy) {
#             dt <- unique(ggtree_data$scale[xx])
#             if (length(dt) > 1) {
#                 dt <- max(setdiff(dt, 1), 1)
#             }
#         } else {
#             dt <- 1
#         }
#         # the distance
#         diff(range(y, na.rm = TRUE)) + dt
#     })
#     wide_data$h <- unlist(h_row)
#
#     # heatmap: data (in the long form)
#     data_long <- gather(wide_data, key = "colLab", value = "value",
#                         - c(rowLab, y, h, node))
#
#     # heatmap: column order
#     if (!is.null(split_column)) {
#         # 1. split_column is given, ignore column order. The order within the
#         #    same slice is determined by the order of split_column
#         # 2. split_column is given and column_cluster = TRUE, the order within
#         #    the same slice is determined by the column similarity
#
#         split_column <- factor(split_column, levels = unique(split_column))
#
#         if (cluster_column) {
#             # order column by the similarity within the same slice
#             ind_split <- lapply(levels(split_column), FUN = function(x){
#                 names(split_column)[split_column == x]
#             })
#             similar <- lapply(ind_split, FUN = function(x) {
#                 if (length(x) > 2) {
#                     st <- hm_data[, x, drop = FALSE]
#                     xx <- hclust(dist(t(st), method = dist_method),
#                                  method = hclust_method)
#                     colnames(st)[xx$order]
#                 } else { x }
#             })
#             similar_order <- unlist(similar)
#             split_column <- split_column[similar_order]
#         }
#         split_level <- sort(split_column)
#
#         if (!is.null(order_column)) {
#             warnings("order_column is ignored when split_column is given")}
#         order_column <- names(split_level)
#     } else {
#         ## split_column isn't given
#         # 1. order_column is given, use order_column and ignore column
#         #    similarity.
#         # 2. order_column isn't given but allow cluster columns, order columns
#         #    by similarity
#         # 3. order_column isn't given and cluter columns is not allowed, use the
#         #    original column order.
#
#         split_level <- setNames(rep(0, ncol(hm_data)), colnames(hm_data))
#         split_level <- factor(split_level)
#         if (is.null(order_column) & !cluster_column) {
#             order_column <- colnames(hm_data)}
#         if (is.null(order_column) & cluster_column) {
#             hc <- hclust(dist(t(hm_data), method = dist_method),
#                          method = hclust_method)
#             order_column <- colnames(hm_data)[hc$order]}
#         if (!is.null(order_column) & cluster_column) {
#             warnings("cluster_column is ignored because order_column is given")
#         }
#     }
#
#
#     # heatmap: x
#     hm_w <- rel_width * (ggtree_data$x %>%
#                                  range(na.rm = TRUE) %>%
#                                  diff)/ncol(hm_data)
#     data_long <- data_long %>%
#         mutate(colLab = factor(colLab, levels = order_column),
#                order_column = as.numeric(colLab) -1,
#                split_level = split_level[colLab],
#                split_level = as.numeric(split_level) - 1,
#                x = hm_w/2 +
#                    order_column  * hm_w +
#                    split_level * split_gap,
#                w = hm_w) %>%
#         select(node, x, y, rowLab, colLab,
#                h, w, value,
#                order_column, split_level)
#
#     return(data_long)
#
#
# }

heatData <- function(ggtree_data, hm_data,
                     rel_width = 1,
                     split_column = NULL,
                     split_gap = 0.5,
                     order_column = NULL,
                     cluster_column = FALSE,
                     hclust_method = "ave",
                     dist_method = "euclidean",
                     fig_coltree = NULL){

    # a tree
    row_tree <- as.phylo(ggtree_data)

    # heatmap: data (in the wide form)
    wide_data <- hm_data <- data.frame(hm_data, check.names = FALSE)
    hm_rn <- rownames(hm_data)
    hm_cn <- colnames(hm_data)

    if (is.null(hm_rn) | is.null(hm_cn)) {
        stop("row/column names are required ...")
    }

    # heatmap rows <-> the figure of the row tree
    node_rn <- transNode(tree = row_tree, node = hm_rn)
    wide_data$node <- node_rn
    wide_data$rowLab <- hm_rn
    wide_data <- map_to_ggtree(ggtree_data = ggtree_data, hm_data = wide_data,
                               node = node_rn, on_row = TRUE)

    # heatmap: data (in the long form)
    data_long <- gather(wide_data, key = "colLab", value = "value",
                        - c(rowLab, y, h, node))

    # heatmap columns <-> the figure of the column tree
    if (!is.null(fig_coltree)) {
        # cols <-> nodes
        col_tree <- ape::as.phylo(fig_coltree)
        node_cn <- transNode(tree = col_tree, node = data_long$colLab)
        data_long <- map_to_ggtree(ggtree_data = fig_coltree$data,
                                   hm_data = data_long,
                                   node = node_cn, on_row = FALSE)
        return(data_long)
    }


    # heatmap: column order
    if (!is.null(split_column)) {
        # 1. split_column is given, ignore column order. The order within the
        #    same slice is determined by the order of split_column
        # 2. split_column is given and column_cluster = TRUE, the order within
        #    the same slice is determined by the column similarity

        split_column <- factor(split_column, levels = unique(split_column))

        if (cluster_column) {
            # order column by the similarity within the same slice
            ind_split <- lapply(levels(split_column), FUN = function(x){
                names(split_column)[split_column == x]
            })
            similar <- lapply(ind_split, FUN = function(x) {
                if (length(x) > 2) {
                    st <- hm_data[, x, drop = FALSE]
                    xx <- hclust(dist(t(st), method = dist_method),
                                 method = hclust_method)
                    colnames(st)[xx$order]
                } else { x }
            })
            similar_order <- unlist(similar)
            split_column <- split_column[similar_order]
        }
        split_level <- sort(split_column)

        if (!is.null(order_column)) {
            warnings("order_column is ignored when split_column is given")}
        order_column <- names(split_level)
    } else {
        ## split_column isn't given
        # 1. order_column is given, use order_column and ignore column
        #    similarity.
        # 2. order_column isn't given but allow cluster columns, order columns
        #    by similarity
        # 3. order_column isn't given and cluter columns is not allowed, use the
        #    original column order.

        split_level <- setNames(rep(0, ncol(hm_data)), colnames(hm_data))
        split_level <- factor(split_level)
        if (is.null(order_column) & !cluster_column) {
            order_column <- colnames(hm_data)}
        if (is.null(order_column) & cluster_column) {
            hc <- hclust(dist(t(hm_data), method = dist_method),
                         method = hclust_method)
            order_column <- colnames(hm_data)[hc$order]}
        if (!is.null(order_column) & cluster_column) {
            warnings("cluster_column is ignored because order_column is given")
        }
    }


    # heatmap: x
    hm_w <- rel_width * (ggtree_data$x %>%
                             range(na.rm = TRUE) %>%
                             diff)/ncol(hm_data)
    data_long <- data_long %>%
        mutate(colLab = factor(colLab, levels = order_column),
               order_column = as.numeric(colLab) -1,
               split_level = split_level[colLab],
               split_level = as.numeric(split_level) - 1,
               x = hm_w/2 +
                   order_column  * hm_w +
                   split_level * split_gap,
               w = hm_w) %>%
        select(node, x, y, rowLab, colLab,
               h, w, value,
               order_column, split_level)

    data_long


}
map_to_ggtree <- function(ggtree_data, hm_data, node, on_row = FALSE) {

    # y
    desd_tbl <- tidytree::offspring(ggtree_data, node,
                              tiponly = FALSE, self_include = TRUE)
    desd <- lapply(desd_tbl, function(x){x$node})

    xy_v <- lapply(desd, FUN = function(x){
        xx <- match(x, ggtree_data$node)
        y <- ggtree_data$y[xx]
        # the middle point
        mean(range(y, na.rm = TRUE))
    })

    # decide rows'y or cols'x
    if (on_row) {
        hm_data$y <- unlist(xy_v)
    } else {
        hm_data$x <- unlist(xy_v)
    }


    # heatmap: height (or width) of a row (column)
    hw <- lapply(desd, FUN = function(x){
        xx <- match(x, ggtree_data$node)
        y <- ggtree_data$y[xx]

        cy <- colnames(ggtree_data)
        if ("scale" %in% cy) {
            dt <- unique(ggtree_data$scale[xx])
            if (length(dt) > 1) {
                dt <- max(setdiff(dt, 1), 1)
            }
        } else {
            dt <- 1
        }
        # the distance
        diff(range(y, na.rm = TRUE)) + dt
    })

    if (on_row) {
        hm_data$h <- unlist(hw)
    } else {
        hm_data$w <- unlist(hw)
    }
    return(hm_data)
}
