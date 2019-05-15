##siegel.tukey test
siegel.tukey = function(x,
                        y,
                        id.col = TRUE,
                        adjust.median = F,
                        rnd = -1,
                        alternative = "two.sided",
                        mu = 0,
                        paired = FALSE,
                        exact = FALSE,
                        correct = TRUE,
                        conf.int = FALSE,
                        conf.level = 0.95)
{
  if (id.col == FALSE) {
    data = data.frame(c(x, y), rep(c(0, 1), c(length(x), length(y))))
  } else {
    data = data.frame(x, y)
  }
  names(data) = c("x", "y")
  data = data[order(data$x), ]
  if (rnd > -1) {
    data$x = round(data$x, rnd)
  }

  if (adjust.median == T) {
    cat("\n", "Adjusting medians...", "\n", sep = "")
    data$x[data$y == 0] = data$x[data$y == 0] - (median(data$x[data$y ==
                                                                 0]))
    data$x[data$y == 1] = data$x[data$y == 1] - (median(data$x[data$y ==
                                                                 1]))
  }
  cat("\n", "Median of group 1 = ", median(data$x[data$y == 0]), "\n", sep =
        "")
  cat("Median of group 2 = ", median(data$x[data$y == 1]), "\n", "\n", sep =
        "")
  # cat("Testing median differences...","\n")
  # print(wilcox.test(data$x[data$y==0],data$x[data$y==1]))


  cat("Performing Siegel-Tukey rank transformation...", "\n", "\n")

  sort.x <- sort(data$x)
  sort.id <- data$y[order(data$x)]

  data.matrix <- data.frame(sort.x, sort.id)

  base1 <- c(1, 4)
  iterator1 <- matrix(seq(from = 1, to = length(x), by = 4)) - 1
  rank1 <- apply(iterator1, 1, function(x)
    x + base1)

  iterator2 <- matrix(seq(from = 2, to = length(x), by = 4))
  base2 <- c(0, 1)
  rank2 <- apply(iterator2, 1, function(x)
    x + base2)

  #print(rank1)
  #print(rank2)

  if (length(rank1) == length(rank2)) {
    rank <-
      c(rank1[1:floor(length(x) / 2)], rev(rank2[1:ceiling(length(x) / 2)]))
  } else{
    rank <-
      c(rank1[1:ceiling(length(x) / 2)], rev(rank2[1:floor(length(x) / 2)]))
  }


  unique.ranks <- tapply(rank, sort.x, mean)
  unique.x <- as.numeric(as.character(names(unique.ranks)))

  rank.matrix <- data.frame(unique.x, unique.ranks)

  ST.matrix <-
    merge(data.matrix, rank.matrix, by.x = "sort.x", by.y = "unique.x")

  print(ST.matrix)

  cat("\n", "Performing Siegel-Tukey test...", "\n", sep = "")

  ranks0 <- ST.matrix$unique.ranks[ST.matrix$sort.id == 0]
  ranks1 <- ST.matrix$unique.ranks[ST.matrix$sort.id == 1]

  cat("\n", "Mean rank of group 0: ", mean(ranks0), "\n", sep = "")
  cat("Mean rank of group 1: ", mean(ranks1), "\n", sep = "")

  print(
    wilcox.test(
      ranks0,
      ranks1,
      alternative = alternative,
      mu = mu,
      paired = paired,
      exact = exact,
      correct = correct,
      conf.int = conf.int,
      conf.level = conf.level
    )
  )
}
