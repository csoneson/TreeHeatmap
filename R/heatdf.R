


heatdf <- function(data){

    rn <- rownames(data)
    cn <- colnames(data)
    if (is.null(rn) | is.null(cn)) {
        stop("row/column names are required ...")
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
    data_long
}
