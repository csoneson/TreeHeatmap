#' @import ggplot2
#' @export
StatHeatText <- ggproto("StatHeatText", Stat,
                       setup_data = function(data, params) {
                           if (is.null(data$subset)) {
                               return(data)
                           }
                           data[which(data$subset), , drop = FALSE]
                       },
                       compute_group = function(data, scales,
                                                name, side,
                                                subset = NULL) {
                           data
                       },
                       required_aes = c("x", "y")
)

#' add row or column labels
#' @param name the name of the current heatmap
#' @param geom "label" or "text"
#' @param subset a logical vector to specify rows or columns to add labels
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @import ggplot2
#' @export
#' @return geom layer
#' @author Ruizhu Huang
geom_heattext <- function(mapping = NULL,
                         data = NULL,
                         geom = GeomText,
                         name = NULL,
                         subset = NULL,
                         side = "left",
                         ...,
                         nudge_x = 0,
                         nudge_y = 0,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
    side <- match.arg(side, c("left", "right", "top", "bottom"))

    position <- position_nudge(nudge_x, nudge_y)

    new_layer <- layer(
        mapping = mapping, data = data,  geom = geom,
        stat = StatHeatText, position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, name = name,
                      subset = subset, side = side,
                      ...)
    )
    class(new_layer) <- c("heatText", class(new_layer))
    new_layer
}

##' @method ggplot_add heatText
##' @import ggplot2
##' @importFrom methods is
##' @importFrom utils modifyList
##' @importFrom dplyr '%>%' distinct select
##' @export
ggplot_add.heatText <- function(object, plot, object_name) {


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

    # side: left / right; top/bottom
    side <- object$stat_params$side
    if (side %in% c("left", "right")) {
        df <- plot$row_anchor[[current]]
        if (side == "left") {
            df$x <- df$minX - min(0.5*df$w)
        } else {
            df$x <- df$maxX + min(0.5*df$w)
        }
    } else {
        df <- plot$col_anchor[[current]]
        if (side == "top") {
            df$y <- df$maxY + min(0.5*df$h)
        } else {
            df$y <- df$minY - min(0.5*df$h)
        }
    }

    # layer data
    object$data <- df %>%
        select(x, y, label) %>%
        distinct()

    self_mapping <- aes(x = x, y = y, label = label)
    if (is.null(object$mapping)) {
        object$mapping <- self_mapping
    } else {
        object$mapping <- modifyList(self_mapping, object$mapping)
    }

    NextMethod()
}



# set.seed(2020-05-11)
# library(dplyr)
# # matrix
# set.seed(2020-05-04)
# dd <- matrix(rnorm(20), ncol=5)
# rownames(dd) <- paste0('r', 1:4)
# colnames(dd) <- paste0('c', 1:5)
#
#
# main <- ggplot() +
#     geom_heat(data = dd %>% heatdf(),
#               aes(x = x , y = y, width = w, height = h, fill = value),
#               name = "hm1", gap = 5) +
#     geom_heat(data = dd %>% heatdf(),
#               aes(x = x , y = y, width = w, height = h, fill = value),
#               gap = 2, name = "hm2") +
#     geom_heat(data = dd %>% heatdf(),
#               aes(x = x , y = y, width = w, height = h, fill = value),
#               gap = 2, name = "hm3")
# main +
#     geom_heattext(aes(color = label),
#                   geom = "label", name = "hm2", nudge_x = -0.5,
#                   size = 4, side = "left",
#                  label.r = unit(0.5, "lines")) +
#     geom_heattext(aes(subset = (label %in% c("c1", "c2"))),
#                       geom = "text", name = "hm3",
#                  color = "red", size = 4, side = "top") +
#     theme_void()


