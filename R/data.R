#' 16S rRNA data (Oral site)
#'
#' A list of datasets containing HMP 16S rRNA data collected from the oral site.
#' It includes 80 OTUs in 124 samples. The OTUs are the top 80 highly variable
#' OTUs in samples. Note: Counts of OTUs are in cpm rather than the raw count.
#'
#' @format A list of datasets:
#' \describe{
#'   \item{count_table}{A matrix storing counts of 80 OTUs in 124 samples}
#'   \item{taxo_table}{The taxonomic table of the 80 OTUs in count_table}
#'   \item{meta_sample}{The metadata of the 124 samples in count_table}
#'   \item{phylo_tree}{A phylogenetic tree of the 80 OTUs}
#'   \item{taxo_tree}{A taxonomic tree of the 80 OTUs}
#' }
#'
#' @source The original data is from \code{\link[HMP16SData]{V35}}. The code
#'   used to generate the 'Oral' data is available as the script file
#'   \code{Oral.R} in the folder \code{data-raw} of this package.
"Oral"



#' The toy data
#'
#' It contains a toy tree, and a count matrix. The data is generated
#' randomly with the R script \code{toydata.R} in the \code{data-raw} folder of
#' this package. The toy tree has 10 leaves. The count matrix has 8 columns
#' representing 8 samples and 10 row corresponding to 10 leaves of
#' the tree.
#'
#' @format A list of datasets:
#' \describe{
#'   \item{toytree}{A tree with 10 leaves and 9 internal nodes.}
#'   \item{countA}{A matrix has 10 rows and 8 columns}
#' }
#'
#' @source The data is generated
#' randomly with the R script \code{toydata.R} in the \code{data-raw} folder of
#' this package.
"toydata"


#' The stool data
#'
#' It contains a normalized count table \code{norm_count}, and a tree, and a
#' data frame \code{df_branch}. Note: this is a processed data only for
#' visualization here, and we don't suggest to use it for data analysis.
#'
#' @format A list of datasets:
#' \describe{
#'   \item{norm_count}{A matrix with 464 rows and 30 columns. Each row
#'   represents a leaf of \code{tree}, and each column represent a sample.
#'   Samples are collected at different time points (0, 4, or 13 months) on
#'   babies that were born differently (c-section or vaginal). For example,
#'   \code{0 M:c_section_1} means that the sample was collected on 0 month
#'   (\code{0 M}) from baby (\code{_1}) in the c-section (\code{c_section}) group.}
#'   \item{tree}{A tree with 464 leaves and 463 internal nodes.}
#'   \item{df_branch}{A data frame has 464 rows and 3 columns. node: the node
#'   numeric id; Detected: Yes or No; pick: TRUE or FALSE.}
#' }
#'
#' @source The data is generated
#' randomly with the R script \code{stool.R} in the \code{data-raw} folder of
#' this package.
"stool"
