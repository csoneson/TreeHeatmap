#' Include subset in the Stat
#'
#' allow subset to be used in the 'aes'
#'
#' @param new_stat The name of the new Stat
#' @param old_stat The name of the old Stat
#' @importFrom ggplot2 ggproto
#' @author Ruizhu Huang
#' @export
allow_subset_stat <- function(new_stat, old_stat) {


    ggproto(new_stat, old_stat,
            setup_data = function(data, params) {
                if (is.null(data$subset)) {
                    return(data)
                }
                data[which(data$subset), , drop = FALSE]
            },
            compute_group = function(data, scales, subset) {
                data
            },
            optional_aes = c("subset"))
}
