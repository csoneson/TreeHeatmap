## code to prepare `stool` dataset goes here

# the original data is from
BackhedF_2015.metaphlan_bugs_list.stool()

# 1) `stool` is obtained by processing the original data as shown in the
# workflow here
# (https://htmlpreview.github.io/?https://raw.githubusercontent.com/fionarhuang/treeclimbR_article/master/microbe/docs/index.html)
# 2) data objects (`df_br`, `loc`, `obs_ave`, `new_phy`) are generated here
# (https://htmlpreview.github.io/?https://raw.githubusercontent.com/fionarhuang/treeclimbR_article/master/microbe/docs/3-Visualization.html)

df_br <-  df_br %>%
    mutate(pick = node %in% loc[[1]])
stool <- list(
    norm_count = obs_ave,
    tree = new_phy,
    df_branch = df_br
)


usethis::use_data(stool, overwrite = TRUE)
