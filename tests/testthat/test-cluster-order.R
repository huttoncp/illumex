# Clusters are numbered by size, largest first, and between two of one size
# the one whose first row comes first in the data. k-means numbers clusters
# as its random starts leave them, so the same partition could be "Cluster 1"
# in one run and "Cluster 3" in the next.

blobs <- function(sizes = c(30, 90, 60), seed = 1) {
  set.seed(seed)
  centres <- cbind(c(0, 8, 16)[seq_along(sizes)], 0)
  g <- rep(seq_along(sizes), sizes)
  list(m = centres[g, ] + matrix(rnorm(2 * sum(sizes), 0, 0.5), ncol = 2), g = g)
}

test_that("cluster 1 is the largest, and sizes fall with the number", {
  b <- blobs()
  cl <- ilm_cluster(b$m, k = 3, B = 5, seed = 1)
  expect_identical(cl$clusters$size, c(90L, 60L, 30L))
  ## the largest true group (rows 31 to 120) is cluster 1
  expect_true(all(cl$ind_cluster$cluster[31:120] == 1L))
  expect_true(all(cl$ind_cluster$cluster[121:180] == 2L))
  expect_true(all(cl$ind_cluster$cluster[1:30] == 3L))
})

test_that("the numbering is the same across seeds and a permutation of the rows", {
  b <- blobs()
  ref <- ilm_cluster(b$m, k = 3, B = 5, seed = 1)$ind_cluster$cluster
  for (s in 2:6)
    expect_identical(ilm_cluster(b$m, k = 3, B = 5, seed = s)$ind_cluster$cluster, ref,
                     label = paste("seed", s))
  ## rows in another order: each row keeps its cluster's number
  set.seed(42); p <- sample(nrow(b$m))
  got <- ilm_cluster(b$m[p, ], k = 3, B = 5, seed = 1)$ind_cluster$cluster
  expect_identical(got, ref[p])
  ## hclust too
  h <- ilm_cluster(b$m, k = 3, method = "hclust", B = 5, seed = 1)
  expect_identical(h$clusters$size, c(90L, 60L, 30L))
})

test_that("between clusters of one size, the one whose first row comes first is numbered first", {
  ## two groups of 50; the one at x = 8 holds the data's first row
  set.seed(3)
  m <- rbind(cbind(rnorm(50, 8, 0.5), rnorm(50, 0, 0.5)),
             cbind(rnorm(50, 0, 0.5), rnorm(50, 0, 0.5)))
  cl <- ilm_cluster(m, k = 2, B = 5, seed = 1)
  expect_identical(cl$clusters$size, c(50L, 50L))
  expect_identical(cl$ind_cluster$cluster[1], 1L)
  expect_true(all(cl$ind_cluster$cluster[1:50] == 1L))
  ## the same data with the groups' rows swapped: the first row's group is 1
  m2 <- m[c(51:100, 1:50), ]
  cl2 <- ilm_cluster(m2, k = 2, B = 5, seed = 1)
  expect_true(all(cl2$ind_cluster$cluster[1:50] == 1L))
  expect_identical(ilm_cluster_by_size(c(2L, 2L, 1L, 1L, 3L)), c(1L, 1L, 2L, 2L, 3L))
})

test_that("stability does not depend on the numbering", {
  b <- blobs()
  cl <- ilm_cluster(b$m, k = 3, B = 20, seed = 1)
  expect_true(all(cl$clusters$jaccard > 0.95))
  ## the same partition numbered another way scores each cluster the same
  fk <- ilm_cluster_funcluster("kmeans", "euclidean", "ward.D2", 25L, NULL)
  a <- cl$ind_cluster$cluster
  set.seed(7); j1 <- ilm_cluster_stability(b$m, fk, 3L, a, 10L)
  set.seed(7); j2 <- ilm_cluster_stability(b$m, fk, 3L, c(3L, 1L, 2L)[a], 10L)
  expect_equal(j2[c(3L, 1L, 2L)], j1)
})
