#' The tree visualization
#'
#' This
#'
#' @inheritParams ggtree::ggtree
#' @param node Nodes to specify clades for zoom-in
#' @param scale_x A numeric vector to scale clades horizontally. It should have
#'   length equal to 1 or the same length as \code{node}.
#' @param scale_y A numeric vector to scale clades vertically. It should have
#'   length equal to 1 or the same length as \code{node}.
#'
#' @importFrom ggtree ggtree
#' @export
#' @return ggtree
#' @author Ruizhu Huang
#'
#' @examples
#' library(ggtree)
#' set.seed(1)
#' rt <- ape::rtree(200)
#'
#' ggtree_th(rt, branch.length = "none", node = c(280, 320),
#'                   scale_y = 8, scale_x = c(2, 1.75)) +
#'     geom_hilight(node = 280, fill = "red", alpha = 0.5) +
#'     geom_hilight(node = 320, fill = "blue", alpha = 0.5)

ggtree_th <- function(tr,
                      mapping = NULL,
                      layout = "rectangular",
                      open.angle = 0,
                      mrsd = NULL,
                      as.Date = FALSE,
                      yscale = "none",
                      yscale_mapping = NULL,
                      ladderize = TRUE,
                      right = FALSE,
                      branch.length = "branch.length",
                      root.position = 0,
                      node = NULL,
                      scale_x = 1,
                      scale_y = 1,
                      ...){
    # list(
    #     ggtree(tr = tr,
    #            mapping = mapping,
    #            layout = layout,
    #            open.angle = open.angle,
    #            mrsd = mrsd,
    #            as.Date = as.Date,
    #            yscale = yscale,
    #            yscale_mapping = yscale_mapping,
    #            ladderize = ladderize,
    #            right = right,
    #            branch.length = branch.length,
    #            root.position = root.position,
    #            ...),
    #     geom_th_zoomclade(node = node,
    #                       scale_x = scale_x,
    #                       scale_y = scale_y)
    #
    # )
    #
    ggtree(tr = tr,
               mapping = mapping,
               layout = layout,
               open.angle = open.angle,
               mrsd = mrsd,
               as.Date = as.Date,
               yscale = yscale,
               yscale_mapping = yscale_mapping,
               ladderize = ladderize,
               right = right,
               branch.length = branch.length,
               root.position = root.position,
               ...) +
        geom_th_zoomclade(node = node,
                          scale_x = scale_x,
                          scale_y = scale_y)



}
