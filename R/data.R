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
#' It contains a toy tree, and two count matrices. The data is generated
#' randomly with the R script \code{toydata.R} in the \code{data-raw} folder of
#' this package. The toy tree has 10 leaves. Each count matrix has 8 columns
#' representing 8 samples. Each row of the count matrix corresponds to a node of
#' the tree.
#'
#' @format A list of datasets:
#' \describe{
#'   \item{toytree}{A tree with 10 leaves and 9 internal nodes.}
#'   \item{countA}{A matrix has 10 rows and 8 columns}
#'   \item{countB}{A matrix has 7 rows and 8 columns}
#' }
#'
#' @source The data is generated
#' randomly with the R script \code{toydata.R} in the \code{data-raw} folder of
#' this package.
"toydata"
