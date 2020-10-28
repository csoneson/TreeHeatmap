#' Annotation barplot
#'
#' This adds a barplot to annotate rows or columns of a heatmap with showing
#' the scale of the barplot.
#'
#' @param name a name to select heatmap
#' @param th_data a data frame. It should include one column named as
#'   \code{rowLab} to store the row name of the heatmap when \code{side} is
#'   \strong{left} or \strong{right}; otherwise, it should include one column
#'   named as \code{colLab} to store the column name of the heatmap when
#'   \code{side} is \strong{top} or \strong{bottom}.
#' @param side a character to specify where to put the barplot. It is selected
#'   from \strong{left}, \strong{right} to annotate rows of the heatmap, or
#'   \strong{top}, \strong{bottom} to annotate columns of the heatmap.
#' @param gap a numeric value to specify the gap between the selected heatmap
#'   and the barplot
#' @param width Bar width. By default, set to 90% of the resolution of the data.
#' @param rel_width a numeric value decide the relative width of the barplot
#'   compared to the selected heatmap
#' @param rel_height a numeric value decide the relative height of the barplot
#'   compared to the selected heatmap
#' @param n a number to decide the number of ticks. See \link[base]{pretty}.
#' @param axis_ticks_length a number to decide the lenght of ticks.
#' @param axis_color the color of the axis
#' @param axis_label_color the color of axis labels
#' @param axis_label_size the size of axis labels
#' @param axis_label_nudge_x adjust the location of labels along x-axis
#' @param axis_label_nudge_y adjust the location of labels along y-axis
#' @param axis_location NULL. If \code{side} is "top" or "bottom", it should be
#'   "left" or "right"; otherwise, it should be "top" or "bottom".
#' @param border_color the color of the rectangular border
#' @param border_size the size of the border line
#' @param ... More arguments accepted by \code{\link[ggplot2]{geom_bar}}.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @import ggplot2
#' @export
#' @return a list of geom layers
#' @author Ruizhu Huang
geom_th_bar <- function(name = NULL,
                        th_data = NULL,
                        mapping = NULL,
                        side = c("top", "bottom", "right", "left"),
                        gap = 1,
                        width = 0.3,
                        rel_width = 1,
                        rel_height = 1,
                        n = 2,
                        ...,
                        axis_ticks_length = NULL,
                        axis_color = "black",
                        axis_label_color = "black",
                        axis_label_size = 3,
                        axis_label_nudge_x = 0,
                        axis_label_nudge_y = 0,
                        axis_location = NULL,
                        border_color = "grey",
                        border_size = 0.5) {

    # where to put the annotation figure
    side <- match.arg(side)

    # where to put the axis on the annotation figure
    if (side %in% c("top", "bottom")) {
        if (is.null(axis_location)) {
            axis_location <- "left"
        }
        axis_direction <- "v"

    } else {
        if (is.null(axis_location)) {
            axis_location <- "bottom"
        }
        axis_direction <- "h"
    }

    c(
      list(geom_th_bar0(name = name,
                        th_data = th_data,
                        mapping = mapping,
                        side = side,
                        gap = gap,
                        width = width,
                        rel_width =  rel_width,
                        rel_height = rel_height,
                        n = n, ...)),
      geom_th_axis(name = name,
                   th_data = NULL,
                   axis_ticks_length = axis_ticks_length,
                   axis_color = axis_color,
                   axis_label_color = axis_label_color,
                   axis_label_size = axis_label_size,
                   axis_label_nudge_x = axis_label_nudge_x,
                   axis_label_nudge_y = axis_label_nudge_y,
                   axis_direction = axis_direction,
                   axis_location = axis_location,
                   border_color = border_color,
                   border_size = border_size)
      )

}

#' Annotation barplot
#'
#' This adds a barplot to annotate rows or columns of a heatmap without showing
#' the scale of the barplot.
#'
#' @param name a name to select heatmap
#' @param th_data a data frame. It should include one column named as
#'   \code{rowLab} to store the row name of the heatmap when \code{side} is
#'   \strong{left} or \strong{right}; otherwise, it should include one column
#'   named as \code{colLab} to store the column name of the heatmap when
#'   \code{side} is \strong{top} or \strong{bottom}.
#' @param side a character to specify where to put the barplot. It is selected
#'   from \strong{left}, \strong{right} to annotate rows of the heatmap, or
#'   \strong{top}, \strong{bottom} to annotate columns of the heatmap.
#' @param gap a numeric value to specify the gap between the selected heatmap
#'   and the barplot
#' @param rel_width a numeric value decide the relative width of the barplot
#'   compared to the selected heatmap
#' @param rel_height a numeric value decide the relative height of the barplot
#'   compared to the selected heatmap
#' @param n a number to decide the number of ticks. See \link[base]{pretty}.
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_bar
#' @import ggplot2
#' @return a geom layer
#' @author Ruizhu Huang
geom_th_bar0 <- function(name = NULL,
                        th_data = NULL,
                        mapping = NULL,
                        side = "right",
                        gap = 1,
                        rel_width = 1,
                        rel_height = 1,
                        stat = "identity",
                        n = 2,
                        ...,
                        inherit.aes = FALSE) {

    if (side %in% c("left", "right")) {
        position <- position_nudge(x = gap)
        orientation <- "y"
    } else {
        position <- position_nudge(y = gap)
        orientation <- "x"
    }

    if (stat != "identity") {
        stop("Currently, only (stat = 'identity') is supported.")
    }
    new_layer <- layer(
        stat = stat, data = NULL, mapping = mapping,
        geom = GeomBar, position = position,
        inherit.aes = inherit.aes,
        params = list(orientation = orientation, ...)
    )

    th_params <- list(gap = gap, name = name, side = side,
                      th_data = th_data, rel_width = rel_width,
                      rel_height = rel_height,
                      n = n)
    ggproto("ggTHbar", new_layer, th_params = th_params)
}


#' @method ggplot_add ggTHbar
#' @import ggplot2
#' @importFrom methods is
#' @importFrom rlang .data as_label ':='
#' @importFrom utils modifyList
#' @importFrom dplyr '%>%' left_join select
#' @export

ggplot_add.ggTHbar <- function(object, plot, object_name) {

    # the active layer of ggheat
    current <- .current_heatmap(plot = plot, object = object)

    # side: left / right; top/bottom
    side <- object$th_params$side
    th_data <- object$th_params$th_data
    rel_width <- object$th_params$rel_width
    rel_height <- object$th_params$rel_height
    gap <- object$th_params$gap
    n <- object$th_params$n

    # width & height of the heatmap
    ra <- .row_anchor(plot, current)
    ww <- max(ra$maxX - ra$minX, na.rm = TRUE)
    ca <- .col_anchor(plot, current)
    hh <- max(ca$maxY - ca$minY, na.rm = TRUE)

    # default mapping & data
    if (side %in% c("left", "right")) {
        # mapping
        ylab <- as_label(object$mapping$y)
        # if (!ylab %in% colnames(th_data)) {
        #     stop("y in 'aes()' should be rowLab")
        # }
        # fix y = "y" to align the barplot to rows
        fix_y_mapping <- aes_string(y = "y")
        object$mapping <- modifyList(object$mapping, fix_y_mapping)

        # get the x label
        xlab <- as_label(object$mapping$x)
        object$data <- .row_anchor(plot, current) %>%
            select(.data$rowLab, .data$y, .data$h) %>%
            left_join(th_data, by = "rowLab") %>%
            mutate(width = diff(range(.data[[xlab]], na.rm = TRUE)),
                   old = .data[[xlab]],
                   !!xlab := .data[[xlab]] * ww/.data$width * rel_width)

        # position: right
        if (side == "left") {
            minX <- min(.row_anchor(plot, current)$minX)
            barX <- max(object$data[[xlab]], na.rm = TRUE)
            object$position$x <- minX - gap - barX
        } else {
            # position: right
            maxX <- max(.row_anchor(plot, current)$maxX)
            object$position$x <- object$position$x + maxX
        }

        # store data to add axis
        od <- object$data
        wh <- unique(od$width)

        df <- data.frame(
            label = pretty(c(0, od$old), n = n) ,
            axis_minY = min(od$y - 0.5*od$h, na.rm = TRUE),
            axis_maxY = max(od$y + 0.5*od$h, na.rm = TRUE)) %>%
            mutate(axisX = .data$label * ww/wh* rel_width +
                       object$position$x)
        plot$heatmap[[current]]$row_tmp$df_axis <- df

    } else {
        # mapping
        xlab <- as_label(object$mapping$x)
        # if (!xlab %in% colnames(th_data)) {
        #     stop("x in 'aes()' should be colLab")
        # }
        fix_x_mapping <- aes_string(x = "x")
        object$mapping <- modifyList(object$mapping, fix_x_mapping)

        # get the y label
        ylab <- as_label(object$mapping$y)
        object$data <- .col_anchor(plot, current) %>%
            select(.data$colLab, .data$x, .data$w) %>%
            left_join(th_data, by = "colLab") %>%
            mutate(height = diff(range(.data[[ylab]], na.rm = TRUE)),
                   old = .data[[ylab]],
                   !!ylab := .data[[ylab]] * hh/.data$height * rel_height)


        # position: bottom
        if (side == "bottom") {
            minY <- min(.col_anchor(plot, current)$minY)
            barY <- max(object$data[[ylab]], na.rm = TRUE)
            object$position$y <- minY - gap - barY

            } else {
            # position: top
            maxY <- max(.col_anchor(plot, current)$maxY)
            object$position$y <- object$position$y + maxY
        }

        # store data to add axis
        od <- object$data
        ht <- unique(od$height)
        df <- data.frame(
          label = pretty(c(0, od$old), n = n) ,
          axis_minX = min(od$x - 0.5*od$w, na.rm = TRUE),
          axis_maxX = max(od$x + 0.5*od$w, na.rm = TRUE)) %>%
          mutate(axisY = .data$label * hh/ht* rel_height +
                   object$position$y)
        plot$heatmap[[current]]$col_tmp$df_axis <- df

    }

    NextMethod()
}


