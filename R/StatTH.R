#' StatTH
#' @import ggplot2
#' @export
StatTH <- ggproto("StatTH", Stat,
                  setup_data = function(data, params) {
                      if (is.null(data$subset)) {
                          return(data)
                      }
                      data[which(data$subset), , drop = FALSE]
                  },
                  compute_group = function(data, scales, name, side,
                                           subset = NULL, th_data,
                                           extend_x, extend_y) {
                      data
                  },
                  optional_aes = c("subset")
)
