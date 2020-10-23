#' Find descendants (or offsprings)
#'
#' \code{.findOS} finds descendants of a node.
#'
#' @param node An internal node. It could be the node number or the node
#'   label.
#' @param tree A phylo object.
#' @param only.leaf A logical value, TRUE or FALSE. The default is TRUE. If
#'   default, only the leaf nodes in the descendant nodes would be returned.
#' @param self.include A logical value, TRUE or FALSE. The default is FALSE. If
#'   TRUE, the node specified in \strong{node} is included and the leaf node
#'   itself is returned as its descendant.
#' @param use.alias A logical value, TRUE or FALSE. The default is FALSE, and
#'   the node label would be used to name the output; otherwise, the alias of
#'   node label would be used to name the output. The alias of node label is
#'   created by adding a prefix \code{"alias_"} to the node number.
#' @return A vector of nodes. The numeric value is the node number, and the
#'   vector name is the corresponding node label. If a node has no label, it
#'   would have NA as name when \code{use.alias = FALSE}, and have the alias of
#'   node label as name when \code{use.alias = TRUE}.
#' @author Ruizhu Huang
#' @keywords internal

.findOS <- function(tree,
                   node,
                   only.leaf = TRUE,
                   self.include = FALSE,
                   use.alias = FALSE) {

    if (!inherits(tree, "phylo")) {
        stop("tree: should be a phylo object")
    }

    if (anyDuplicated(node)) {
        warning("duplicated values are found in input 'node'")
    }

    if (!(is.character(node) |
          is.numeric(node) |
          is.integer(node))) {
        stop("The argument (node) should be character or numeric")
    }
    # the edge matrix
    mat <- tree$edge
    matN <- .matTree(tree = tree)

    if (is.character(node)) {
        numA <- .transNode(tree = tree, node = node,
                          use.alias = TRUE,
                          message = FALSE)
    } else {
        numA <- node
        isOut <- !numA %in% mat
        if (any(isOut)) {
            stop("Node ", numA,
                 " can't be found in the ",
                 deparse(substitute(tree)), "\n")
        }

    }

    loc <- lapply(seq_len(ncol(matN)), FUN = function(x){
        xx <- matN[, x]
        xm <- match(xx, numA)
        # which elements in xx could be matched to numA
        xi <- which(!is.na(xm))
        # which elements in numA
        mi <- xm[!is.na(xm)]

        mm <- cbind("row" = xi,
                    "col" = rep(x, length(xi)),
                    "node" = numA[mi])

    })

    moc <- do.call(rbind, loc)


    # separate leaf and internal nodes
    mocL <- moc[moc[, "col"] == 1, , drop = FALSE]
    mocI <- moc[moc[, "col"] != 1, , drop = FALSE]

    if (only.leaf) {
        if (!self.include) {
            mocL<- NULL
        }
        if (nrow(mocI)) {
            mocI[, "col"] <- 1
        }

    } else {
        if (!self.include) {
            mocL <- NULL
            if (nrow(mocI)) {
                mocI[, "col"] <- mocI[, "col"] - 1
            }
        }
        ll <- lapply(mocI[, "col"],
                     FUN = function(x) {
                         seq(from = 1, to = x , by = 1)
                     })
        mocII <- cbind("row" = rep(mocI[, "row"], mocI[, "col"]),
                       "col" = unlist(ll),
                       "node" = rep(mocI[, "node"], mocI[, "col"]))
        mocI <- mocII
    }

    out <- vector("list", length(numA))
    if (is.null(mocL)) {
        mocC <- mocI
    } else {
        mocC <- rbind(mocL, mocI)
    }

    # descendants: get, remove duplicates and sort
    desd <- cbind("found" = matN[mocC[, c("row", "col"), drop = FALSE]],
                  "node" = mocC[, "node"])
    desd <- desd[!duplicated(desd), , drop = FALSE]
    od <- order(desd[, "node"], desd[, "found"], decreasing = FALSE)
    desd <- desd[od, , drop = FALSE]

    # split according to the parent node
    parent <- factor(desd[, "node"],
                     levels = unique(desd[, "node"]))
    dList <- split(desd[, "found"], f = parent)

    # output result in the order as the input node
    #o <- match(unique(desd[, "node"]), numA)
    # out[o] <- dList
    o <- match(numA, unique(desd[, "node"]))
    out <- dList[o]
    names(out) <- .transNode(tree = tree, node = numA,
                            use.alias = use.alias)
    return(out)
}



#' Transfer between node number and node label
#'
#' \code{transNode} does the transformation between the number and the label of
#' a node on a tree
#'
#' @param tree A phylo object
#' @param node A character or numeric vector representing tree node label(s) or
#'   tree node number(s)
#' @param use.alias A logical value, TRUE or FALSE. This is an optional argument
#'   that only requried when the input \code{node} is a numeric vector. The
#'   default is FALSE, and the node label would be returned; otherwise, the
#'   alias of node label would be output. The alias of node label is created by
#'   adding a prefix \code{"alias_"} to the node number.
#' @param message A logical value, TRUE or FALSE. The default is FALSE. If TRUE,
#'   message will show when a tree have duplicated labels for some internal
#'   nodes.
#'
#' @keywords internal
#' @return a vector
#' @author Ruizhu Huang
#'


.transNode <- function(tree, node, use.alias = FALSE,
                       message = FALSE) {

    if (!inherits(tree, "phylo")) {
        stop("tree: should be a phylo object. \n")
    }

    if (is.factor(node)) {
        stop("factor detected; The node label is required to be character or
             numeric.")
    }
    # node number & tip number
    mat <- tree$edge
    nodI <- sort(unique(mat[, 1]))
    tip <- sort(setdiff(mat[, 2], mat[, 1]))
    nodeA <- c(tip, nodI)

    # if node labels are given, check whether the length could match with the
    # length of internal nodes.
    if (!is.null(tree$node.label)) {
        if (length(tree$node.label) != length(nodI)) {
            stop("The length of internal node label isn't equal to
                 the length of the internal nodes. \n")
        }
    }

    # node labels
    nodeLab <- c(tree$tip.label, tree$node.label)
    nodeLab_alias <- paste("alias_", c(tip, nodI), sep = "")
    if (message) {
        if (any(duplicated(nodeLab))) {
            cat("There are more than one nodes using a same label or
                without any label.\n")
        }
    }

    # check whether the input node number exists in the provided tree
    if (is.numeric(node)) {
        if (!all(node %in% nodeA)) {
            stop("The node number ", node[!node %in% nodeA],
                 " can't be found in the ",
                 deparse(substitute(tree)), "\n")
        }
    }
    # check whether the input label exists in the provided tree
    # (allow nodeLab_alias)
    inLab <- all(node %in% nodeLab)
    inAlias <- all(node %in% nodeLab_alias)
    if (is.character(node)) {
        if (!any(inLab, inAlias)) {
            cat(setdiff(node, nodeLab),
                " can't be matched to any node label of the tree. \n")
            stop("Either the node label or the alias of node label should be
                 provided, but not a mixture of them. \n")

        }
    }

    # =============== Transformation ======================
    # transfer from the label to the number
    if (is.character(node)) {
        if (inLab) {
            names(nodeA) <- nodeLab
            final <- nodeA[node]
        } else {
            names(nodeA) <- nodeLab_alias
            final <- nodeA[node]
        }
    }

    # transfer from the number to the label
    if (is.numeric(node)) {
        if (use.alias) {
            sel <- match(node, nodeA)
            final <- nodeLab_alias[sel]
        } else {
            sel <- match(node, nodeA)
            final <- nodeLab[sel]
        }}

    # output
    return(final)

}

#' Transform a phylo object into a matrix.
#'
#' \code{matTree} transforms a phylo tree into a matrix. The entry of the matrix
#' is node number. Each row represents a path connecting a leaf node and the
#' root. The columns are arranged in the order as the path passing the nodes to
#' reach the root.
#'
#' @param tree A phylo object
#' @keywords internal
#' @return A matrix
#' @author Ruizhu Huang
#'

.matTree <- function(tree) {

    if (!inherits(tree, "phylo")) {
        stop("tree: should be a phylo object")
    }


    # the edge matrix
    mat <- tree$edge

    # the leaves
    L1 <- setdiff(mat[, 2], mat[, 1])

    # each path connects a tip with the root.
    # each path is stored as a row of matN
    # the first column stores the tips (or leaves)
    matN <- cbind(L1)
    i <- 1
    repeat {
        li <- mat[match(matN[, i], mat[, 2]), 1]
        ll <- length(unique(li[!is.na(li)]))
        if (ll == 0) {
            break
        }
        matN <- cbind(matN, li)
        i <- i + 1
    }
    rownames(matN) <- NULL
    colnames(matN) <- paste("L", seq_len(ncol(matN)), sep = "")

    return(matN)
}

