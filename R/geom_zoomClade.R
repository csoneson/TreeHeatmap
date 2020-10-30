#' Scale clades
#'
#' This is to zoom in specific clades.
#'
#' @param node a vector of node
#' @param scale_x A numeric vector to scale clades horizontally. It should have
#'   length equal to 1 or the same length as \code{node}.
#' @param scale_y A numeric vector to scale clades vertically. It should have
#'   length equal to 1 or the same length as \code{node}.
#' @import ggplot2
#' @import ggtree
#' @export
#' @return a layer
#' @author Ruizhu Huang
#' @examples
#'
#' library(ggtree)
#' set.seed(1)
#' rt <- ape::rtree(200)
#'
#' ggtree(rt, branch.length = "none") +
#'     geom_th_zoomclade(node = c(280, 320), scale_y = 8,
#'     scale_x = c(2, 1.75)) +
#'     geom_hilight(node = 280, fill = "red", alpha = 0.5) +
#'     geom_hilight(node = 320, fill = "blue", alpha = 0.5)


geom_th_zoomclade <- function(node = NULL, scale_x = 1,
                              scale_y = 1) {

    th_params <- list(node = node, scale_x = scale_x,
                      scale_y = scale_y)

    ggproto("ggTH", Geom, th_params = th_params)
}

#' @method ggplot_add ggTH
#' @import ggplot2
#' @export
ggplot_add.ggTH <- function(object, plot, object_name) {

   node <- object$th_params$node
   scale_x <- object$th_params$scale_x
   scale_y <- object$th_params$scale_y

   if (length(scale_x) != 1 & length(node) != length(scale_x)) {
       stop("node and scale_x have different lengths.")
   }
   if (length(scale_y) != 1 & length(node) != length(scale_y)) {
       stop("node and scale_y have different lengths.")
   }

   # keep node & scale_* having the same length
   if (length(scale_x) == 1) {
       scale_x <- rep(scale_x, length(node))
   }

   if (length(scale_y) == 1) {
       scale_y <- rep(scale_y, length(node))
   }

   out_y <- .prepare_node(data = plot$data, node = node, scale = scale_y)
   out_x <- .prepare_node(data = plot$data, node = node, scale = scale_x)

   node <- out_y$node
   scale_y <- out_y$scale
   scale_x <- out_x$scale

   # zoom node one by one
   for (i in seq_along(node)) {
       # i <- 1
       # i <- i + 1
       # i
       # node[i]
       plot$data <- .zoomScale(data = plot$data, node = node[i],
                              scale_y = scale_y[i],
                              scale_x = scale_x[i])
       plot
   }

   plot
}



#' @importFrom tidytree offspring
#' @details \code{.zoomScale} is created by Ruizhu Huang based on
#'   \code{\link[ggtree]{scaleClade}} created by GuangchuangYu. The original
#'   code is
#'   \href{https://github.com/YuLab-SMU/ggtree/blob/master/R/clade-functions.R}{here}
#'
.zoomScale <- function(data, node, scale_y = 1, scale_x =1){

    if (!"scale" %in% colnames(data)) {data$scale <- 1}

    # the descendant data
    old_data_os <- data_os <- offspring(data, node)

    # the number of descendant leaves
    # n_os <- sum(data_os$isTip, na.rm = TRUE)

    is_same <- scale_y == 1 & scale_x == 1
    #skip_scale <- n_os == 1
    if (is_same) {return(data)}


    os <- data_os$node
    data_os$y <- data$y[node] + (data_os$y - data$y[node]) * scale_y
    data_os$x <- data$x[node] + (data_os$x - data$x[node]) * scale_x


    new_gap <- diff(sort(data_os$y[data_os$isTip]))
    new_gap.lw <- new_gap[1]
    new_gap.up <- new_gap[length(new_gap)]

    old_gap <- diff(sort(old_data_os$y[old_data_os$isTip]))
    old_gap.lw <- old_gap[1]
    old_gap.up <- old_gap[length(old_gap)]

    scale_diff.up <- max(data_os$y+0.5*new_gap.up) - max(old_data_os$y+0.5*old_gap.up)
    scale_diff.lw <- min(data_os$y-0.5*new_gap.lw) - min(old_data_os$y - 0.5*old_gap.lw)
    ii <- data$y > max(old_data_os$y)
    if (sum(ii) > 0) {
        data[ii, "y"] <- data$y[ii] + scale_diff.up
    }
    jj <- data$y < min(old_data_os$y)
    if (sum(jj) > 0) {
        data[jj, "y"] <- data$y[jj] + scale_diff.lw
    }
    data[os, ] <- data_os

    data[os, "scale"] <- data[os, "scale"] * scale_y
    data <- ggtree:::reassign_y_from_node_to_root(data, node)
    data <- ggtree:::calculate_branch_mid(data)
    dff <- ggtree:::calculate_angle(data)
    return(dff)
}

#' @importFrom  tidytree sibling
.prepare_node <- function(data, node, scale) {

    leaf <- data$node[data$isTip %in% TRUE]

    ind <- which(node %in% leaf)
    if (length(ind)){
        warning("leaves in 'node' can't be zoomed in and are ignored")
        node <- node[-ind]
        scale <- scale[-ind]
    }

    df_os <- offspring(data, node)

    # single-leaf clade: not ignored only when its sibling node exist in 'node'
    n_leaf <- lapply(df_os, FUN = function(x){sum(x$isTip, na.rm = TRUE)})
    ind_single <- which(unlist(n_leaf) == 1)

    if (length(ind_single)) {
        node_s <- node[ind_single]
        scale_s <- scale[ind_single]

        ## if the sibling exists in 'node', use the parent node
        ## The scale will follow the scale for the sibling node
        df_sib <- lapply(node_s, FUN = function(x) {sibling(data, x)})
        node_sib <- unlist(lapply(df_sib, FUN = function(x) {x$node}))
        node_par <- unlist(lapply(df_sib, FUN = function(x) {x$parent}))
        is_in_sib <- node_sib %in% node
        use_node_par <- unique(node_par[is_in_sib])
        use_scale_par <- unique(scale[match(node_sib[is_in_sib], node)])

        if (length(use_node_par) != length(use_scale_par)) {
            stop("Don't know how to zoom in/out clades with single leaf")
        }
        ind_sib <- which(node %in% node_sib)

        node <- c(node[-c(ind_single, ind_sib)], use_node_par)
        scale <- c(scale[-c(ind_single, ind_sib)], use_scale_par)

    }

    out <- list(node = node, scale = scale)
    return(out)


}
