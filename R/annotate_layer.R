#' @import ggplot2
#' @keywords internal
#' @author Ruizhu Huang
.annotate_layer <- function(mapping = NULL,
                            th_data = NULL,
                            data = NULL,
                            name = NULL,
                            subset = NULL,
                            side = "left",
                            nudge_x = 0,
                            nudge_y = 0,
                            extend_x = c(0, 0),
                            extend_y = c(0, 0),
                            na.rm = FALSE,
                            show.legend = NA,
                            inherit.aes = TRUE,
                            geom = NULL,
                            stat = NULL,
                            new_class = new_class,
                            ...) {
    side <- match.arg(side, c("left", "right", "top", "bottom"))

    position <- position_nudge(nudge_x, nudge_y)

    new_layer <- layer(
        mapping = mapping, data = data, geom = geom,
        stat = stat, position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, name = name,
                      subset = subset, side = side,
                      th_data = th_data, extend_x = extend_x,
                      extend_y = extend_y, ...)
    )
    class(new_layer) <- c(new_class, class(new_layer))
    new_layer
}
