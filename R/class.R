as.ggheat <- function(plot) {
    if (is(plot, "ggheat"))
        return(plot)

    if (!is(plot, 'gg')) {
        stop("input should be a 'gg' object.")
    }

    # anchor data
    row_df <- plot$data %>%
        select(rowLab, y, h) %>%
        distinct()
    col_df <- plot$data %>%
        select(colLab, x, w) %>%
        distinct()

    structure(list(plotlist = list(main = plot),
                   width = 1,
                   height = 1,
                   layout = matrix(1),
                   n = 1,
                   main_col = 1,
                   main_row = 1,
                   anchor = list(row_anchor = row_df,
                                 col_anchor = col_df)),
              class = "ggheat")

}



##' @method print ggheat
##' @importFrom patchwork plot_layout
##' @importFrom patchwork plot_spacer
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 theme_void
##' @export
print.ggheat <- function(x, ...) {
    grid.draw(x)
}

##' @importFrom ggplot2 ggplotGrob
##' @importFrom patchwork patchworkGrob
ggheatGrob <- function(x) {
    mp <- x$plotlist[[1]]
    if ( length(x$plotlist) == 1) {
        return(ggplotGrob(mp))
    }

    for (i in x$layout[, x$main_col]) {

        if (is.na(i)) next
        if (i == 1) next
        x$plotlist[[i]] <- suppressMessages(
            x$plotlist[[i]] + xlim2(x))

    }
    for (i in x$layout[x$main_row,]) {

        if(is.na(i)) next
        if (i == 1) next
        x$plotlist[[i]] <- suppressMessages(
            x$plotlist[[i]] + ylim2(x) )

    }

    idx <- as.vector(x$layout)
    idx[is.na(idx)] <- x$n + 1
    x$plotlist[[x$n+1]] <- ggplot() + theme_void() # plot_spacer()
    plotlist <- x$plotlist[idx]

    pp <- plotlist[[1]]
    for (i in 2:length(plotlist)) {
        pp <- pp + plotlist[[i]]
    }

    res <- pp + plot_layout(byrow=F, ncol=ncol(x$layout),
                            widths = x$width,
                            heights= x$height,
                            guides = 'collect')
    patchworkGrob(res)
}

##' @importFrom grid grid.draw
##' @method grid.draw ggheat
##' @export
grid.draw.ggheat <- function(x, recoding = TRUE) {
    grid::grid.draw(ggheatGrob(x))
}
