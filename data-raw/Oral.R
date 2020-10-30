## code to prepare `Oral` dataset goes here


# ---------------------- Load packages & functions ----------------------------
suppressPackageStartupMessages({
    library(HMP16SData)
    library(TreeSummarizedExperiment)
    library(ggplot2)
})


# define a function
subsetByLeaf <- function(tse, rowLeaf) {
    # if rowLeaf is provided as node labels, convert them to node numbers
    if (is.character(rowLeaf)) {
        rowLeaf <- convertNode(tree = rowTree(tse), node = rowLeaf)
    }
    # subset data by leaves
    sse <- subsetByNode(tse, rowNode = rowLeaf)
    # update the row tree
    ## -------------- new tree: drop leaves ----------
    oldTree <- rowTree(sse)
    newTree <- ape::keep.tip(phy = oldTree, tip = rowLeaf)
    ## -------------- update the row tree ----------
    # track the tree
    changeTree(x = sse, rowTree = newTree, rowNodeLab = rowLinks(sse)$nodeLab)
}


# -----------------------------------------------------------------------------
## Load data
# -----------------------------------------------------------------------------

v35 <- V35()

# Store as TSE
# add an assay: cpm
suppressWarnings({
    (tse_phy <- TreeSummarizedExperiment(assays = assays(v35),
                                     rowData = rowData(v35),
                                     colData = colData(v35),
                                     rowTree = metadata(v35)$phylogeneticTree,
                                     metadata = metadata(v35)["experimentData"]))
})

assays(tse_phy)$cpm <- apply(assays(tse_phy)[[1]], 2,
                             FUN = function(x) {
                                 x/sum(x)*1E6
                             } )
# -----------------------------------------------------------------------------
## Select samples:
#  1. center (BCM)
#  2. subsites (Saliva, Supragingival plaque, Throat)
#  3.  select samples with counts in [3000, 10000] (remove outliers)
# -----------------------------------------------------------------------------
cdat <- colData(tse_phy)
site <- c("Saliva", "Supragingival Plaque", "Throat")
sel_samp <- cdat$HMP_BODY_SUBSITE %in% site & cdat$RUN_CENTER == "BCM"
oral <- tse_phy[, sel_samp]
oral

# select samples with counts in [3000, 10000]
depth <- colSums(assays(oral)[[1]])
df_depth <- data.frame(site = colData(oral)$HMP_BODY_SUBSITE,
                       depth = depth)
ggplot(data = df_depth) +
    geom_boxplot(aes(x = site, y = depth)) +
    geom_hline(yintercept = c(3000, 10000),
               color = "red", linetype = "dotted")
oral <- oral[, depth >= 3000 & depth < 10000]
oral

# -----------------------------------------------------------------------------
## Select highly variable OTUs (the top 80)
# -----------------------------------------------------------------------------
count <- assays(oral)$cpm
vr <- sort(apply(log2(count+1), 1, var), decreasing = TRUE)
top_vr <- names(vr)[1:80]
oral_sel <- subsetByLeaf(oral, rowLeaf = top_vr)

# -----------------------------------------------------------------------------
## Generate a taxonomic tree
# -----------------------------------------------------------------------------
# use the taxonomic table
tax_order <- c("SUPERKINGDOM", "PHYLUM", "CLASS",
               "ORDER", "FAMILY", "GENUS")
tax_0 <- data.frame(rowData(oral_sel)[, tax_order])
tax_0$OTU <- rownames(oral_sel)

# resolve loops caused by the polyphyletic group or NA
suppressWarnings({
    tax_loop <- detectLoop(tax_tab = tax_0)
    #head(tax_loop[!is.na(tax_loop$child), ])
    tax_1 <- resolveLoop(tax_tab = tax_0)
})
tax_tree <- toTree(data = tax_1)

# -----------------------------------------------------------------------------
## Store data including:
#    1) the count table
#    2) the taxonomic table
#    3) the sample metadata
#    4) the taxonomic tree
#    5) the phylogenetic tree
# -----------------------------------------------------------------------------

Oral <- list(count_table = assays(oral_sel)$cpm,
             taxo_table = tax_0,
             meta_sample = data.frame(colData(oral_sel)),
             phylo_tree = rowTree(oral_sel),
             taxo_tree = tax_tree)

usethis::use_data(Oral, overwrite = TRUE)

