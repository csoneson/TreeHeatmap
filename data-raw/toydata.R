## code to prepare `toydata` dataset goes here

# a random tree: 10 leaves
set.seed(1)
tinyTree <- ape::rtree(10)
tinyTree$node.label <- paste0("N", 11:19)
tinyTree$tip.label <- paste0("N", 1:10)

# a random count matrix:
p1 <- c(rep(0.1/3, 3), rep(0.1, 4), rep(0.4/2, 2), 0.1)
p2 <- c(rep(0.4/3, 3), rep(0.1, 4), rep(0.1/2, 2), 0.1)

aa <- cbind(rmultinom(n = 4, size = 50, prob = p1),
            rmultinom(n = 4, size =50, prob = p2))
colnames(aa) <- paste("S", 1:8, sep = "")
rownames(aa) <- tinyTree$tip.label

# a new count matrix: aggregate some rows
bb <- rbind(colSums(aa[1:3, ]),
            aa[4:7, ],
            colSums(aa[8:9, ]),
            aa[10, ])
rownames(bb) <- c("N14", paste0("N", 4:7), "N19", "N10")

toydata <- list(toytree = tinyTree,
                countA = aa,
                countB = bb)


usethis::use_data(toydata, overwrite = TRUE)