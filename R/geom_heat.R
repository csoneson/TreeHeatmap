#' @rdname ggplot2-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatHeat <- ggproto("StatHeat", Stat,
                     compute_group = function(data, scales, gap, name) {
                         data$x <- data$x + gap
                         data
                     },
                     required_aes = c("x", "y")
)

#' @param name the name of the current heatmap
#' @param gap a numeric value to specify the gap between the current and the
#'   previous heatmap
#' @inheritParams layer
#' @inheritParams geom_tile
#' @export
#' @rdname geom_heat
#' @return geom layer
#' @author Ruizhu Huang
geom_heat <- function(mapping = NULL,
                       data = NULL,
                       position = "identity",
                       gap = 1,
                       name = NULL,
                       ...,
                       linejoin = "mitre",
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
    new_layer <- layer(
        stat = StatHeat, data = data, mapping = mapping, geom = "tile",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gap = gap, name = name, ...)
    )
    class(new_layer) <- c("ggHeat", class(new_layer))
    new_layer
}

#' @method ggplot_add ggHeat
#' @importFrom ggplot2 ggplot_add
#' @importFrom dplyr mutate select distinct '%>%'
#' @export
ggplot_add.ggHeat <- function(object, plot, object_name) {

    if (length(plot$col_anchor) != length(plot$row_anchor)) {
        stop("The anchor data has different lengths...")
    }
    # about ggheat layer
    previous <- length(plot$col_anchor)
    current <- object$stat_params$name
    if (is.null(current)) {
        current <- previous + 1
    }


    # if not exists, create anchor data
    if (!previous) {
        plot$col_anchor <- plot$row_anchor <- list()
        plot$row_anchor[[current]] <- object$data %>%
            mutate(minX = min(x, na.rm = TRUE),
                   maxX = max(x, na.rm = TRUE),
                   label = rowLab) %>%
            select(label, y, minX, maxX, w) %>%
            distinct()

        plot$col_anchor[[current]] <- object$data %>%
            mutate(minY = min(y, na.rm = TRUE),
                   maxY = max(y, na.rm = TRUE),
                   label = colLab,
                   x = x + object$stat_params$gap) %>%
            select(label, x, minY, maxY, h) %>%
            distinct()
        } else {
            # otherwise, update anchor data
            object$data$x <- object$data$x + max(plot$col_anchor[[previous]]$x)
            plot$row_anchor[[current]] <- plot$row_anchor[[previous]]
            plot$col_anchor[[current]] <- object$data %>%
                mutate(minY = min(y, na.rm = TRUE),
                       maxY = max(y, na.rm = TRUE),
                       label = colLab,
                       x = x + object$stat_params$gap) %>%
                select(label, x, minY, maxY, h) %>%
                distinct()


        }
    plot$row_anchor[[current]]$maxX <- max(object$data$x) + object$stat_params$gap
    plot$row_anchor[[current]]$minX <- min(object$data$x) + object$stat_params$gap

    NextMethod()
}



main <- ggplot() +
    geom_heat(data = dd %>% heatdf(),
              aes(x = x , y = y, width = w, height = h, fill = value),
              name = "hm1", gap = 5) +
    geom_heat(data = dd %>% heatdf(),
              aes(x = x , y = y, width = w, height = h, fill = value),
              gap = 2, name = "hm2") +
    geom_heat(data = dd %>% heatdf(),
              aes(x = x , y = y, width = w, height = h, fill = value),
              gap = 2, name = "hm3")

main
main$col_anchor
main$row_anchor


# main
# main$col_anchor
# main$row_anchor
# plot <- main
# object <- geom_heat(data = dd %>% heatdf(),
#                     aes(x = x , y = y, width = w, height = h, fill = value),
#                     name = "hm2", gap = 2)
# main2 <- main +
#     geom_heat(data = dd %>% heatdf(),
#               aes(x = x , y = y, width = w, height = h, fill = value),
#               name = "hm2", gap = 2)
#
# main2$col_anchor
# main2$row_anchor
#
# plot <- ggplot()
# object <- geom_heat(data = dd %>% heatdf(),
#               aes(x = x , y = y, width = w, height = h, fill = value),
#               name = "hm1")
#
#
# plot <- ggplot() + geom_heat(data = dd %>% heatdf(),
#                     aes(x = x , y = y, width = w, height = h, fill = value),
#                     name = "hm1")
#
# object <- geom_heat(data = dd %>% heatdf(),
#                     aes(x = x , y = y, width = w, height = h, fill = value),
#                     name = "hm2")
#
# ghh <- gh +
#     geom_heat(data = dd %>% heatdf(),
#               aes(x = x , y = y, width = w, height = h, fill = value),
#               gap = 1) +
#     geom_heat(data = dd %>% heatdf(),
#               aes(x = x , y = y, width = w, height = h, fill = value),
#               gap = 1)
# ghh
# ghh$col_anchor
# ghh$row_anchor
# ggplot_build(ghh)$data
