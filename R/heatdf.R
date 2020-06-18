heatdf <- function(data,
                   fig_rowtree = NULL,
                   fig_coltree = NULL,
                   split_col = NULL,
                   w = 1, h = 1){
    rn <- rownames(data)
    cn <- colnames(data)
    if (is.null(rn) | is.null(cn)) {
        stop("row/column names are required ...")
    }

    # wide form
    data_wide <- data.frame(data, check.names = FALSE) %>%
        mutate(rowLab = rownames(data))

    # if row tree is available, y & h (height) are obtained from the tree data
    if (is(fig_rowtree, "ggtree")){
        # tree data
        row_data <- fig_rowtree$data
        row_tree <- as.phylo(fig_rowtree)

        # y is from row_data
        # height is based from row_data
        desd_row <- findOS(tree = row_tree, node = data_wide$rowLab,
                           only.leaf = FALSE, self.include = TRUE)
        y_row <- lapply(desd_row, FUN = function(x){
            xx <- match(x, row_data$node)
            y <- row_data$y[xx]
            # the middle point
            mean(range(y, na.rm = TRUE))
        })
        data_wide$y <- unlist(y_row)

        # heatmap: height of a row
        h_row <- lapply(desd_row, FUN = function(x){
            xx <- match(x, row_data$node)
            y <- row_data$y[xx]

            cy <- colnames(row_data)
            if ("scale" %in% cy) {
                dt <- unique(row_data$scale[xx])
                if (length(dt) > 1) {
                    #dt <- setdiff(dt, 1)
                    dt <- max(setdiff(dt, 1), 1)
                }
            } else {
                dt <- 1
            }
            # the distance
            diff(range(y, na.rm = TRUE)) + dt
        })
        data_wide$h <- unlist(h_row)
    } else{
        data_wide <- data_wide %>%
            mutate(y = as.numeric(factor(rowLab))*h,
                   h = h)
    }

    data_long <- gather(data_wide, key = "colLab", value = "value",
                        - c(rowLab, y, h))

    # if column tree is available, x & w are obtained from the tree data
    if (is(fig_coltree, "ggtree")){
        # tree data
        col_data <- fig_coltree$data
        col_tree <- as.phylo(fig_coltree)

        # x is from col_data
        # width is based from col_data
        colLab <- colnames(data)
        desd_col <- findOS(tree = col_tree, node = colLab,
                           only.leaf = FALSE, self.include = TRUE)
        x_col <- lapply(desd_col, FUN = function(x){
            xx <- match(x, col_data$node)
            y <- col_data$y[xx]
            # the middle point
            mean(range(y, na.rm = TRUE))
        })
        data_long$x <- unlist(x_col[data_long$colLab])

        # heatmap: width of a row
        w_col <- lapply(desd_col, FUN = function(x){
            xx <- match(x, col_data$node)
            y <- col_data$y[xx]

            cy <- colnames(col_data)
            if ("scale" %in% cy) {
                dt <- unique(col_data$scale[xx])
                if (length(dt) > 1) {
                    #dt <- setdiff(dt, 1)
                    dt <- max(setdiff(dt, 1), 1)
                }
            } else {
                dt <- 1
            }
            # the distance
            diff(range(y, na.rm = TRUE)) + dt
        })
        data_long$w <- unlist(w_col[data_long$colLab])
    } else{
        data_long <- data_long %>%
            mutate(x = as.numeric(factor(colLab))*w,
                   w = w)
    }

    data_long
}
