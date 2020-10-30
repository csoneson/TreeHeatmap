# TreeHeatmap
This package is created to visualize heatmap at different levels of the tree. It allows the visualization of the heatmap to be zoomed in specific branches of the tree, and also provides annotation functions to decorate the heatmap. The syntax of annotation functions mainly follows the commonly used package ggplot2. Geom layers that are generated in this package have a prefix geom_th to distinguish those provided in ggplot2 or ggtree.


# Installation
```
devtools::install_github("fionarhuang/TreeHeatmap")
```

# A short example
```
library(TreeHeatmap)
library(ggnewscale)
library(ggplot2)
library(ggtree)

data("toydata")
toytree <- toydata$toytree
count <- toydata$count
count_agg <- toydata$count_agg

ggtree(toytree, branch.length = "none", size = 0.1) +
    # the first heatmap & show column tree
    geom_th_heatmap(th_data = count, name = "hm1", gap = 1, 
                    cluster_column = TRUE, color_coltree = "blue",
                    size_coltree = 0.5) +
    scale_fill_viridis_c(option = "A", name = "hm1") +
    new_scale_fill() +
    # the second heatmap
    geom_th_heatmap(th_data = count_agg, name = "hm2", gap = 0.2) +
    scale_fill_viridis_c(option = "D", name = "hm2") +
    # annotation on the row tree
    geom_hilight(node = 18, fill = "blue", alpha = 0.5) +
    geom_hilight(node = 15, fill = "orange", alpha = 0.5) +
    geom_tippoint(color = "red", shape = 8) +
    # row / column names
    geom_th_text(name = "hm2", side = "right", nudge_x = 0.5,
                 color = "red") +
    geom_th_text(name = "hm2", side = "top", nudge_y = 0.5,
                 angle = 90, size = 3) +
    # display values 
    geom_th_addvalue(name = "hm1", color = "white", size = 4)


```




