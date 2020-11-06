suppressPackageStartupMessages({
    library(ggtree)
    library(TreeHeatmap)
    library(dplyr)
    library(ggplot2)
    library(ggnewscale)
})

# load data
data(stool)
# the normalized data
norm_count <- stool$norm_count
# the tree
tree <- stool$tree
# the data to annotate the tree
df_branch <- stool$df_branch


# column indicators for samples collected at different time
is_0M_c <- startsWith(colnames(norm_count), prefix = "0 M:c")
is_0M_v <- startsWith(colnames(norm_count), prefix = "0 M:v")

is_4M_c <- startsWith(colnames(norm_count), prefix = "4 M:c")
is_4M_v <- startsWith(colnames(norm_count), prefix = "4 M:v")

is_12M_c <- startsWith(colnames(norm_count), prefix = "12 M:c")
is_12M_v <- startsWith(colnames(norm_count), prefix = "12 M:v")

# Viz tree
ggtree(tree, layout = "fan", size = 0.5,
       branch.length = "none",
       open.angle = 50,
       aes(color = Detected)) %<+% df_branch %>%
    rotate_tree(140) +
    geom_point2(aes(subset = pick),
                color = "red", size = 2) +
    scale_color_manual(values = c("Yes" = "orange", "No" = "grey")) +
    new_scale_fill() +

# Viz data on the 0th month: heatmap
    geom_th_heatmap(name = "0M_c",
                    th_data = norm_count[, is_0M_c],
                    rel_width = 0.2, gap = 2) +
    geom_th_heatmap(name = "0M_v",
                    th_data = norm_count[, is_0M_v],
                    rel_width = 0.2) +
    geom_th_title(name = "0M_c", side = "top",
                  label = "c-section",
                  angle = 3, nudge_y = 15, size = 3) +
    geom_th_title(name = "0M_v", side = "top",
                  label = "vaginal", angle = 3,
                  nudge_y = 15, size = 3) +
    geom_th_border(name = "0M_v", side = "right",
                   th_geom = "label",gap = 1,
                   extend_y = c(0, 18),
                   params_label = list(fill = "orange",
                                       color = "black",
                                       size = 3,
                                       alpha = 0.5,
                                       nudge_y = 18,
                                       label = "0 M")) +

# Viz data on the 4th month: heatmap
    geom_th_heatmap(name = "4M_c",
                    th_data = norm_count[, is_4M_c],
                    rel_width = 0.2, gap = 2) +
    geom_th_heatmap(name = "4M_v",
                    th_data = norm_count[, is_4M_v],
                    rel_width = 0.2) +
    geom_th_title(name = "4M_c", side = "top",
                  label = "c-section", angle = 3,
                  nudge_y = 15, size = 3) +
    geom_th_title(name = "4M_v", side = "top",
                  label = "vaginal", angle = 3,
                  nudge_y = 15, size = 3) +
    geom_th_border(name = "4M_v", side = "right",
                   th_geom = "label",
                   gap = 1, extend_y = c(0, 18),
                   params_label = list(fill = "blue",
                                       color = "black",
                                       size = 3,
                                       alpha = 0.5,
                                       nudge_y = 18,
                                       label = "4 M")) +

# Viz data on the 12th month: heatmap
    geom_th_heatmap(name = "12M_c",
                    th_data = norm_count[, is_12M_c],
                    rel_width = 0.2, gap = 2) +
    geom_th_heatmap(name = "12M_v",
                    th_data = norm_count[, is_12M_v],
                    rel_width = 0.2) +
    geom_th_title(name = "12M_c", side = "top", label = "c-section",
                  angle = 3,  nudge_y = 15, size = 3) +
    geom_th_title(name = "12M_v", side = "top", label = "vaginal",
                  angle = 3,  nudge_y = 15, size = 3) +
    geom_th_border(name = "12M_v", side = "right", th_geom = "label",
                   gap = 1, extend_y = c(0, 18),
                   params_label = list(fill = "red",
                                       color = "black",
                                       size = 3,
                                       alpha = 0.5,
                                       nudge_y = 18,
                                       label = "12 M")) +
    scale_fill_viridis_c(option = "D") +
    theme(
        aspect.ratio = 1,
        legend.position = c(0.08, 0.7),
        legend.background = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        legend.key.size = unit(0.4, "cm"),
        legend.spacing.x = unit(0.2, "mm"),
        legend.margin = margin(t = 0, b = 0, r = 5, l = 0),
        legend.box.margin=margin(t = -5, b = -5, r = 0, l = -20),
        plot.margin = margin(t = -50, b = -50, r = -100, l = -100)
    )




