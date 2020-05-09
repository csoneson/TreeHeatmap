#' create a heatmap
#'
#' This creates a heatmap from a matrix
#'
#' @param data A matrix or data frame
#' @param show_rownames TRUE or FALSE. This specifies whether row names should
#'   be displayed. The default is TRUE.
#' @param rownames_position "right" or "left". The default is "left".
#' @param rownames_color A color on row names. The default is "black".
#' @param rownames_angle A numeric value. The angle of row names. The default is
#'   0.
#' @param rownames_nudge_x A numeric value to nudge row names by (on x-axis).
#'   The defaut is 0.
#' @param rownames_nudge_y A numeric value to nudge row names by (on y-axis).
#'   The defaut is 0.
#' @param rownames_size A numeric value to specify the font size of row names.
#' @param rownames_hjust The hjust for row names: 0 (left aligned); 0.5
#'   (centered); 1 (right aligned).
#' @param show_colnames TRUE or FALSE. This specifies whether column names should
#'   be displayed. The default is TRUE.
#' @param colnames_position "top" or "bottom". The default is "top".
#' @param colnames_color A color on column names. The default is "black".
#' @param colnames_angle A numeric value. The angle of column names. The default is
#'   0.
#' @param colnames_nudge_x A numeric value to nudge column names by (on x-axis).
#'   The defaut is 0.
#' @param colnames_nudge_y A numeric value to nudge column names by (on y-axis).
#'   The defaut is 0.
#' @param colnames_size A numeric value to specify the font size of column names.
#' @param colnames_hjust The hjust for column names: 0 (left aligned); 0.5
#'   (centered); 1 (right aligned).

#' @import ggplot2
#' @importFrom tidyr gather
#' @importFrom dplyr mutate "%>%" filter
#' @export
#' @author Ruizhu Huang
#'
#' @return heatmap
#' @examples
#'
#' library(dplyr)
#' # matrix
#' set.seed(2020-05-04)
#' dd <- matrix(rnorm(20), ncol=5)
#' rownames(dd) <- paste0('r', 1:4)
#' colnames(dd) <- paste0('c', 1:5)
#' (gh <- ggheat(data = dd))
#'


ggheat <- function(data,
                   show_rownames = TRUE,
                   rownames_position = "left",
                   rownames_color = "black",
                   rownames_angle = 0,
                   rownames_nudge_x = 0,
                   rownames_nudge_y = 0,
                   rownames_size = 4,
                   rownames_hjust = 0.5,
                   show_colnames = TRUE,
                   colnames_position = "top",
                   colnames_color = "black",
                   colnames_angle = 0,
                   colnames_nudge_x = 0,
                   colnames_nudge_y = 0,
                   colnames_size = 4,
                   colnames_hjust = 0.5){
    if (!(is.matrix(data) | is.data.frame(data))) {
        stop("data should be a matrix or data frame")
    }

    if (is.null(rownames(data)) | is.null(colnames(data))) {
        warning("data has no row/column names")
    }

    # wide form
    data_wide <- data.frame(data, check.names = FALSE) %>%
        mutate(rowLab = rownames(data))

    # long form
    data_long <- gather(data_wide, key = "colLab", value = "value",
                        - c(rowLab))

    # x, y, width, height
    data_long <- data_long %>%
        mutate(x = as.numeric(factor(colLab)),
               y = as.numeric(rev(factor(rowLab))),
               w = 1,
               h = 1)

    # plot heatmap
    hm <- ggplot(data = data_long) +
        geom_tile(aes(x = x, y = y, width = w, height = h, fill = value)) +
        theme_void()

    # show rownames
    if (show_rownames) {
        if (rownames_position == "left") {
            hm <- hm +
                geom_text(data = . %>%
                              filter(x == min(x, na.rm = TRUE)),
                          aes(x = min(x) - 0.6*w, y = y, label = rowLab),
                          color = rownames_color,
                          angle = rownames_angle,
                          nudge_x = rownames_nudge_x,
                          nudge_y = rownames_nudge_y,
                          hjust = rownames_hjust,
                          size = rownames_size)
        } else {
            hm <- hm +
                geom_text(data = . %>%
                              filter(x == max(x, na.rm = TRUE)),
                          aes(x = max(x) + 0.6*w, y = y, label = rowLab),
                          color = rownames_color,
                          angle = rownames_angle,
                          nudge_x = rownames_nudge_x,
                          nudge_y = rownames_nudge_y,
                          hjust = rownames_hjust,
                          size = rownames_size)
        }

    }

    # show colnames
    if (show_colnames) {
        if (colnames_position == "top") {
            hm <- hm +
                geom_text(data = . %>%
                              filter(y == max(y, na.rm = TRUE)),
                          aes(x = x, y = max(y) + 0.6*h, label = colLab),
                          color = colnames_color,
                          angle = colnames_angle,
                          nudge_x = colnames_nudge_x,
                          nudge_y = colnames_nudge_y,
                          hjust = colnames_hjust,
                          size = colnames_size)
        } else {
            hm <- hm +
                geom_text(data = . %>%
                              filter(y == min(y, na.rm = TRUE)),
                          aes(x = x, y = min(y) - 0.6*h, label = rowLab),
                          color = colnames_color,
                          angle = colnames_angle,
                          nudge_x = colnames_nudge_x,
                          nudge_y = colnames_nudge_y,
                          hjust = colnames_hjust,
                          size = colnames_size)
        }

    }


    # anchor data
    hm$row_anchor <- data_long %>%
        select(rowLab, x, y, h) %>%
        distinct() %>%
        mutate(minY = min(y, na.rm = TRUE),
               maxY = max(y, na.rm = TRUE),
               label = rowLab)

    hm$col_anchor <- data_long %>%
        select(colLab, x, w) %>%
        distinct() %>%
        mutate(minX = min(x, na.rm = TRUE),
               maxX = max(x, na.rm = TRUE),
               label = colLab)

    hm

}
