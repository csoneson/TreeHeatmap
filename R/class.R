# as.ggheat <- function(plot) {
#     if (inherits(plot, "ggheat"))
#         return(plot)
#
#     if (!inherits(plot, 'gg')) {
#         stop("input should be a 'gg' object.")
#     }
#
#     # anchor data
#     row_df <- plot$data %>%
#         select(rowLab, y, h) %>%
#         distinct()
#     col_df <- plot$data %>%
#         select(colLab, x, w) %>%
#         distinct()
#
#     structure(list(plotlist = list(main = plot),
#                    anchor = list(row_anchor = row_df,
#                                  col_anchor = col_df)),
#               class = "ggheat")
#
# }

ag <- as.ggheat(plot = gh)
xg  <- geom_point(data = mpg, aes(cyt, hwy))
class(xg)


# library(ggplot2)
#
# custom_geom_point <- function(...) {
#     layer <- geom_point(...)
#     structure(list(layer = layer), class = "custom_layer")
# }
#
# ggplot_add.custom_layer <- function(object, plot, object_name) {
#     x_mapping <- plot$mapping$x
#     message("Ha! I know that the global aesthetic mapping for the `x` variable is ", x_mapping)
#     plot + object$layer
# }
#
# gp <- ggplot(mpg, aes(x = cty, y = hwy)) +
#     custom_geom_point()
#
# gp
# class(gp)

StatHtile <- ggproto("StatHtile", Stat,
                     compute_group = function(data, scales, gap, name) {
                         data$x <- data$x + gap
                         data
                     },
                     required_aes = c("x", "y")
)

geom_htile <- function(mapping = NULL,
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
        stat = StatHtile, data = data, mapping = mapping, geom = "tile",
        position = position, show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, gap = gap, name = name, ...)
    )
    class(new_layer) <- c("htile", class(new_layer))
    new_layer
}

ggplot_add.htile <- function(object, plot, object_name) {
    object$data$x <- object$data$x + max(plot$col_anchor$maxX)
    plot$col_anchor$maxX <- max(object$data$x) + object$stat_params$gap
    plot$col_anchor$minX <- min(object$data$x) + object$stat_params$gap
    NextMethod()
}

xh <- gh +
    geom_htile(data = gh$data %>%
                         mutate(value = 2* value),
               aes(x = x , y = y, width = 0.1, height = h, fill = value),
               gap = 2)
xxh <- xh +
    geom_htile(data = gh$data ,
               aes(x = x, y = y, width = w, height = h, fill = value),
               gap = 1)
xxh

# add row names
add_rowtext <- function(mapping = NULL,
                        data = NULL,
                        stat = "identity",
                        position = "identity",
                        ...,
                        parse = FALSE,
                        nudge_x = 0,
                        nudge_y = 0,
                        check_overlap = FALSE,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = TRUE) {
    new_layer <- layer(
        stat = stat, data = data, mapping = mapping, geom = "text",
        position = position, show.legend = FALSE, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, nudge_x = nudge_x, ...)
    )
    class(new_layer) <- c("rowtext", class(new_layer))
    new_layer
}

add_rowtext <- function(...) {
    new_layer <- geom_text(...)
    class(new_layer) <- c("rowtext", class(new_layer))
    new_layer
}
ggplot_add.rowtext <- function(object, plot, object_name) {
    if (is(object$data, "waiver")) {
        object$data <- plot$row_anchor
        object$data$label <- object$data$rowLab
        object$data$x <- min(plot$col_anchor$minX - 0.6 * plot$col_anchor$w)
    }
    NextMethod()
}

df_rtext <- xh$row_anchor %>%
    mutate(label = rowLab,
           x = min(xh$col_anchor$minX - 0.6 * xh$col_anchor$w))
xh +
    add_rowtext(data = df_rtext,
                aes(x = x, y = y, label = label), nudge_x = 15)
xh +
    geom_text(data = df_rtext,
                aes(x = x, y = y, label = label), nudge_x = 15)



ar <- add_rowtext(aes(x = x, y = y, label = label))
xxh <- xh +
    geom_htile(data = gh$data ,
               aes(x = x, y = y, width = w, height = h, fill = value),
               gap = 1)

xh$layers[[2]]$data
lapply(xh$layers, class)
ggplot_build(xh)$data
xg <- geom_htile(data = gh$data ,
                 aes(x = x +1, y = y, width = w, height = h, fill = value),
                 gap = 2)
xg$aes_params
xg$geom_params
xg$stat_params
