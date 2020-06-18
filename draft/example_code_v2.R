library(tidyr)
library(dplyr)
library(ggplot2)
library(ggtree)
library(aplot)
library(patchwork)


nr <- 10
nc <- 8

rowT <- rtree(nr)
rowT$tip.label <- paste0("r", seq_len(nr))
colT <- rtree(nc)
colT$tip.label <- paste0("c", seq_len(nc))

# matrix
set.seed(1)
dd <- matrix(rnorm(nr*nc, mean = 20), ncol=nc)
dd[seq_len(nr/2), seq_len(nc/2)] <- 4*dd[seq_len(nr/2), seq_len(nc/2)]
rownames(dd) <- rowT$tip.label
colnames(dd) <- colT$tip.label


main <- ggplot() +
    geom_heat(data = dd %>% heatdf(),
              aes(x = x , y = y, width = w, height = h, fill = value),
              name = "hm1") +
    geom_heat(data = dd %>% heatdf(),
              aes(x = x , y = y, width = w, height = h, fill = value),
              gap = 2, name = "hm2") +
    geom_heat(data = dd %>% heatdf(),
              aes(x = x , y = y, width = w, height = h, fill = value),
              gap = 2, name = "hm3")

main
main$col_anchor
main$row_anchor

main <- main +
    geom_heattext(aes(color = label), geom = "label", name = "hm2",
                  nudge_x = -0.5, size = 4, side = "left",
                  label.r = unit(0.5, "lines"), show.legend = FALSE) +
    geom_heattext(aes(color = label), geom = "text", name = "hm2",
                  size = 4, side = "top", show.legend = FALSE)

main

# cluster columns
hcc <- hclust(dist(t(dd)))
phc <- ggtree(hcc) +
    layout_dendrogram() +
    geom_tiplab()
phcc <- scaleClade2(tree_view = phc, node = 12, scale = 2)

# row tree
fig_row <- ggtree(rowT, branch.length = "none", layout = "circular") +
    geom_tippoint(color = "red", size = 4)
fig_row <- scaleClade2(fig_row, node = 15, scale = 3)
fig0 <- fig_row +
    geom_heat(data = heatData(ggtree_data = fig_row$data, hm_data = dd,
                              fig_coltree = phcc),
              aes(x = x , y = y, width = w, height = h, fill = value),
              name = "hm1", gap = 1) +
    geom_heattext(aes(color = label), geom = "text", name = "hm1",
                  size = 4, side = "top", show.legend = FALSE, nudge_y = 1) +
    geom_heat(data = dd %>% heatdf(fig_rowtree = fig_row, fig_coltree = phc),
              aes(x = x , y = y, width = w, height = h, fill = value),
              name = "hm2", gap = 3)
fig0
phc1 <- phcc
phc2<-phc
phc1$data$y <- phc1$data$y + 1 # gap: hm1
phc2$data$y <- phc$data$y + max(fig0$col_anchor$hm1$x) +3 # gap + maxX :hm2
phc3 <- phc1 + geom_tree(phc2$data) + xlim2(fig0)
phc3  + fig0 +
    plot_layout(ncol = 1)


## ====================== without fig_coltree =================================
fig_row +
    geom_heat(data = heatData(ggtree_data = fig_row$data,
                              hm_data = dd, rel_width = 2),
              aes(x = x , y = y, width = w, height = h, fill = value),
              name = "hm1", gap = 1) +
    geom_heattext(aes(color = label), geom = "text", name = "hm1",
                  size = 4, side = "top", show.legend = FALSE, nudge_y = 1) +
    geom_heat(data = dd %>% heatdf(fig_rowtree = fig_row, fig_coltree = phc),
              aes(x = x , y = y, width = w, height = h, fill = value),
              name = "hm2", gap = 3)

splitC <- setNames(rep(LETTERS[1:2], 4), colnames(dd))
ordC <- colnames(dd)[c(1, 3, 4, 2, 6, 5, 7, 8)]
fig_row +
    geom_heat(data = heatData(ggtree_data = fig_row$data,
                              hm_data = dd, rel_width = 2,
                              #split_column = splitC,
                              order_column = ordC),
              aes(x = x , y = y, width = w, height = h, fill = value),
              name = "hm1", gap = 1) +
    geom_heattext(aes(color = label), geom = "text", name = "hm1",
                  size = 4, side = "top", show.legend = FALSE, nudge_y = 2) +
    geom_heat(data = dd %>% heatdf(fig_rowtree = fig_row, fig_coltree = phc),
              aes(x = x , y = y, width = w, height = h, fill = value),
              name = "hm2", gap = 3)
