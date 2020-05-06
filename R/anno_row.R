#' add annotation on rows of heatmap
#'
#' \code{anno_row} annotates rows of heatmap, either on the left or right side.
#' @param plot the main heatmap
#' @param anno_plot A annotation plot
#' @param side "left" or "right"
#' @param width 0.1
#' @param name NULL
#'
#' @export
#' @importFrom rlang quo_text

rowAnnotate <- function(plot, anno_plot, side = "left",
                        width = 0.1, name = NULL) {

    plot <- as.ggheat(plot = plot)
    if (is.null(name)) {
        name <- paste0("row_", length(plot$plotlist))
    }

    # use anchor to update data of anno_plot
    anchor <- plot$anchor$row_anchor
    anchor_y <- anchor$y
    names(anchor_y) <- anchor$rowLab

    # update data: layers & global
    nlayer <- length(anno_plot$layers)
    if (!inherits(anno_plot, "ggtree")) {
       # update layer data
       for (i in seq_len(nlayer)) {
           yvar <- quo_text(anno_plot$layers[[i]]$mapping[["y"]])
           df <- anno_plot$layers[[i]]$data
           if (!is(df, "waiver")) {
               df[[yvar]] <- anchor_y[df[[yvar]]]
               anno_plot$layers[[i]]$data <- df
           }
       }

        # update main data
        is_waiver <- lapply(anno_plot$layers, FUN = function(x){ is(x$data, "waiver")})
        ind_waiver <- max(c(0, which(unlist(is_waiver))))
        if (ind_waiver) {
            yvar <- quo_text(anno_plot$layers[[ind_waiver]]$mapping[["y"]])
            anno_plot$data[[yvar]] <- anchor_y[anno_plot$data[[yvar]]]
        }

    }

    # add anno_plot
    plot$plotlist[[name]] <- anno_plot

    plot$n <- plot$n + 1
    new_col <- matrix(nrow=nrow(plot$layout), ncol=1)
    new_col[plot$main_row] <- plot$n

    if (side == "left") {
        plot$width <- c(width, plot$width)
        plot$layout <- cbind(new_col, plot$layout)
        plot$main_col <- plot$main_col + 1
    } else {
        plot$width <- c(plot$width, width)
        plot$layout <- cbind(plot$layout, new_col)
    }

    plot
    }

# plot$plotlist[[name]]
#
#
# library(tidyr)
# library(dplyr)
#
# set.seed(2020-05-04)
# dd <- matrix(rnorm(20), ncol=5)
# rownames(dd) <- paste0('r', 1:4)
# colnames(dd) <- paste0('c', 1:5)
# main <- ggheat(data = dd)
# main
#
# dr <- data.frame(gene = paste0('r', 1:4),
#                  size = sample(1:10, 4))
# anno_plot <- ggplot(dr) +
#     geom_point(aes(y = gene, x = 1, size = size),
#                color = "red", alpha = 0.5) +
#     theme_void()
# anno_plot
# cp <- main %>% rowAnnotate(anno_plot = anno_plot, width = 0.1) %>%
#     rowAnnotate(anno_plot = anno_plot, width = 0.5)
# cp$width
#
#
#
# cp$plotlist
# pp & theme_minimal()
# pp + plot_layout(byrow=F, ncol=ncol(cp$layout),
#                  widths = cp$width,
#                  heights= cp$height,
#                  guides = 'collect') & coord_polar(start = 0, theta = "y")

# xx <- x$plotlist[[2]] # y axis is  numeric
#
#
# # main %>% rowAnnotate(anno_plot = anno_plot, width = 0.1, side = "right")
#
# xx
# lim <- ylim2(x)
# ggplot_add.axisAlign(object = lim, plot = xx)
#
# xx
#
#
# anno_plot <- ggplot() +
#     geom_point(data = dr %>%
#                    filter(gene %in% c("r1", "r2")),
#                aes(y = gene, x = 1, size = size),
#                color = "red", alpha = 0.5)
# anno_plot
#
# library(patchwork)
# ma <- anno_plot + main +
#     plot_layout(widths = c(0.1, 1), ncol = 2,
#                                 guides = "collect")
# class(ma)
