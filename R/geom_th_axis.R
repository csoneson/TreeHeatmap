#' add the axis and the border
#' add the axis and the border of an annotation plot
#'
#' @param name NULL or a name of heatmap (\link{geom_th_heatmap}).
#' @param th_data a data frame. To draw a vertical axis, it requires columns
#'   \code{axis_minX}, \code{axis_maxX}, \code{axisY} and \code{label}, where
#'   \code{axis_minX} and \code{axis_maxX} decide the border limit on the x-dim.
#'   The axis is either on \code{axis_minX} for \code{axis_location = "left"} or
#'   \code{axis_maxX} for \code{axis_location = "right"}. The axis ticks are
#'   decided by the \code{axisY} and the tick labels are given in \code{label}.
#'   Similarly, to draw a horizontal axis, it requires columns \code{axisX},
#'   \code{axis_minY}, \code{axis_maxY}, and \code{label}.
#' @param axis_ticks_length a number to decide the lenght of ticks.
#' @param axis_color the color of the axis
#' @param axis_label_color the color of axis labels
#' @param axis_label_size the size of axis labels
#' @param axis_label_nudge_x adjust the location of labels along x-axis
#' @param axis_label_nudge_y adjust the location of labels along y-axis
#' @param axis_direction either "h" for horizontal or "v" for vertical
#' @param border_color the color of the rectangular border
#' @param border_size the size of the border line
#' @importFrom rlang .data
#' @export
#' @author Ruizhu Huang
#' @examples
#' library(ggplot2)
#' df_v <- data.frame(axis_minX = rep(1, 10),
#'                    axis_maxX = rep(10, 10),
#'                    axisY = 1:10,
#'                    label = LETTERS[1:10])
#' ggplot() +
#'     geom_th_axis(th_data = df_v,
#'     axis_direction = "v",
#'     axis_location = "right")
#'
#'
#' df_h <- data.frame(axisX = 1:10,
#'                    axis_minY = rep(1, 10),
#'                    axis_maxY = rep(10, 10),
#'                    label = LETTERS[1:10])
#'
#'
#' ggplot() +
#'     geom_th_axis(th_data = df_h, axis_direction = "h")
geom_th_axis <- function(name = NULL,
                         th_data = NULL,
                         axis_ticks_length = NULL,
                         axis_color = "black",
                         axis_label_color = "black",
                         axis_label_size = 3,
                         axis_label_nudge_x = 0,
                         axis_label_nudge_y = 0,
                         axis_direction = "h",
                         axis_location = NULL,
                         border_color = "grey",
                         border_size = 0.5
){
    if (axis_direction == "h") {
        if (is.null(axis_location)) {axis_location <- "top"}
        false_loc <- !axis_location %in% c("top", "bottom")
        if (false_loc) {
            stop("axis_location should be 'top'/'bottom' for horizontal axis.")
        }
        h_just <- 0.5
        v_just <- ifelse(axis_location == "top", 0, 1)
    }
    if (axis_direction == "v") {
        if (is.null(axis_location)) {axis_location <- "right"}
        false_loc <- !axis_location %in% c("right", "left")
        if (false_loc) {
            stop("axis_location should be 'right'/'left' for horizontal axis.")
        }
        h_just <- ifelse(axis_location == "right", 0, 1)
        v_just <- 0.5
    }

    list(
        geom_THaxis(name = name,
                    th_data = th_data, geom = GeomSegment,
                    aes(x = x, y = y,
                        xend = xend, yend = yend),
                    color = border_color, draw = "border",
                    axis_ticks_length = axis_ticks_length,
                    axis_direction = axis_direction,
                    axis_location = axis_location),
        geom_THaxis(name = name,
                    th_data = th_data, geom = GeomSegment,
                    aes(x = x, y = y,
                        xend = xend, yend = yend),
                    color = axis_color, draw = "axis",
                    axis_ticks_length = axis_ticks_length,
                    axis_direction = axis_direction,
                    axis_location = axis_location),
        geom_THaxis(name = name,
                    th_data = th_data, geom = GeomText,
                    aes(x = (x- 2*brk_x),
                        y = (y - 2*brk_y), label = label),
                    color = axis_label_color, draw = "axislabel",
                    axis_ticks_length = axis_ticks_length,
                    axis_direction = axis_direction,
                    axis_location = axis_location,
                    size = axis_label_size,
                    nudge_x = axis_label_nudge_x,
                    nudge_y = axis_label_nudge_y,
                    hjust = h_just,
                    vjust = v_just)
    )
}

#' ggTHaxis
#'
#' @param name a name to select heatmap
#' @param th_data a data frame. It should include one column named as
#'   \code{rowLab} to store the row name of the heatmap when \code{side} is
#'   \strong{left} or \strong{right}; otherwise, it should include one column
#'   named as \code{colLab} to store the column name of the heatmap when
#'   \code{side} is \strong{top} or \strong{bottom}.
#' @param axis_ticks_length a number to decide the lenght of ticks.
#' @param axis_direction either "h" for horizontal or "v" for vertical
#' @param axis_location "right" or "left" when \code{axis_direction = "v"};
#'   "top" or "bottom" when \code{axis_direction = "h"}
#' @param draw "border", "axis" or "axislabel"
#' @param border_color the color of the rectangular border
#' @param border_size the size of the border line
#' @import ggplot2
#' @return ggTHaxis (a geom layer)
#' @author Ruizhu Huang
geom_THaxis <- function(mapping = NULL,
                        th_data = NULL,
                        axis_ticks_length = NULL,
                        axis_location = NULL,
                        axis_direction = "h",
                        draw = NULL,
                        geom = Geom,
                        position = "identity",
                        ...,
                        na.rm = FALSE,
                        show.legend = NA,
                        inherit.aes = FALSE,
                        nudge_x = 0,
                        nudge_y = 0,
                        name = NULL) {


    if (!missing(nudge_x) || !missing(nudge_y)) {
        if (!missing(position)) {
            abort("You must specify either `position` or `nudge_x`/`nudge_y`.")
        }
        position <- position_nudge(nudge_x, nudge_y)
    }

    new_layer <- layer(
        stat = "identity", data = NULL, mapping = mapping,
        position = position, geom = geom,
        show.legend = show.legend, inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )



    if (axis_direction == "h") {
        if (is.null(axis_location)) { axis_location <- "bottom" }
    } else {
        if (is.null(axis_location)) { axis_location <- "left" }
    }
    th_params <- list(th_data = th_data,
                      axis_ticks_length = axis_ticks_length,
                      axis_direction = axis_direction,
                      draw = draw,
                      axis_location = axis_location,
                      name = name)
    ggproto("ggTHaxis", new_layer, th_params = th_params)

}


#' @method ggplot_add ggTHaxis
#' @import ggplot2
#' @export
ggplot_add.ggTHaxis <- function(object, plot, object_name) {

    # parameters
    th_data <- object$th_params$th_data
    axis_ticks_length <- object$th_params$axis_ticks_length
    axis_direction <- object$th_params$axis_direction
    draw <- object$th_params$draw
    axis_location <- object$th_params$axis_location
    if (is.null(th_data)){
        # the active layer of ggheat
        current <- .current_heatmap(plot = plot, object = object)

        if (axis_direction == "h") {
            th_data <- .row_axis(plot, current)
        } else {
            th_data <- .col_axis(plot, current)
        }

    }



    dff <- axis_ticks(df = th_data, len =  axis_ticks_length,
                      direction = axis_direction,
                      axis_location = axis_location)

    if (draw == "border") {
        object$data <- dff[dff$belong == "border", ,drop = FALSE]
    }

    if (draw == "axis") {
        object$data <- dff[dff$belong != "border", ,drop = FALSE]
    }

    if (draw == "axislabel") {
        object$data <- dff[dff$belong == "axis_ticks", ,drop = FALSE]
    }

    NextMethod()
}





axis_v_ticks <- function(df, len = NULL, axis_location = "left") {

    if (is.null(len)) {
        len <- diff(range(df$axis_maxX, df$axis_minX))*0.01
    }

    if (axis_location == "right") {
        xv <- "axis_maxX"
    } else {
        xv <- "axis_minX"
    }
    # horizontal ticks
    #ts <- pretty(df$axisY, n = n)
    ts <- df$axisY
    dff_h <- data.frame(x = unique(df[[xv]]),
                        xend = unique(df[[xv]]) + len,
                        y = ts,
                        yend = ts,
                        belong = "axis_ticks",
                        label = df$label,
                        brk_y = 0)
    # border lines
    dX <- c(df$axis_minX, df$axis_maxX)
    dff_b <- data.frame(x = c(min(dX), max(dX), max(dX), min(dX)),
                        xend = c(max(dX), max(dX), min(dX), min(dX)),
                        y = c(min(ts), min(ts), max(ts), max(ts)),
                        yend = c(min(ts), max(ts), max(ts), min(ts)),
                        belong = "border",
                        label = "",
                        brk_y = 0)

    # vertical ticks
    dff_v <- data.frame(x = unique(df[[xv]]),
                        xend = unique(df[[xv]]),
                        y = min(ts),
                        yend = max(ts),
                        belong = "axis_bone",
                        label = "",
                        brk_y = 0)
    # output data
    dff <- rbind(dff_b, dff_h, dff_v)
    if (axis_location == "left") {
        dff$brk_x <- 2*len
    } else {
        dff$brk_x <- -2*len
    }


    return(dff)
}

axis_h_ticks <- function(df, len, axis_location = "bottom") {
    if (is.null(len)) {
        len <- diff(range(df$axis_minY, df$axis_maxY))*0.01
    }

    if (axis_location == "top") {
        xv <- "axis_maxY"
    } else {
        xv <- "axis_minY"
    }
    # horizontal line
    #ts <- pretty(df$axisX, n = n)
    ts <- df$axisX
    dff_h <- data.frame(x = min(ts),
                        xend = max(ts),
                        y = unique(df[[xv]]),
                        yend = unique(df[[xv]]),
                        belong = "axis_bone",
                        label = "",
                        brk_x = 0)

    # border lines
    dY <- c(df$axis_minY, df$axis_maxY)
    dff_b <- data.frame(x = c(min(ts), min(ts), max(ts), max(ts)),
                        xend = c(min(ts), max(ts), max(ts), min(ts)),
                        y = c(min(dY), max(dY), max(dY), min(dY)),
                        yend = c(max(dY), max(dY), min(dY), min(dY)),
                        belong = "border",
                        label = "",
                        brk_x = 0)

    # vertical ticks
    dff_v <- data.frame(x = ts,
                        xend = ts,
                        y = unique(df[[xv]]),
                        yend = unique(df[[xv]]) + len,
                        belong = "axis_ticks",
                        label = df$label,
                        brk_x = 0)
    # output data
    dff <- rbind(dff_b, dff_h, dff_v)
    if (axis_location == "top") {
        dff$brk_y <- -2*len
    } else {
        dff$brk_y <- 2*len
    }

    return(dff)
}

axis_ticks <- function(df, direction, len = NULL, axis_location = NULL) {
    if (direction == "h") {
        if (is.null(axis_location)) {
            axis_location <- "bottom"
        }
        dff <- axis_h_ticks(df = df, len = len,
                            axis_location = axis_location)
    } else {
        if (is.null(axis_location)) {
            axis_location <- "left"
        }
        dff <- axis_v_ticks(df = df, len = len,
                            axis_location = axis_location)
    }
    return(dff)
}



