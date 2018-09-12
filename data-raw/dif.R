# data and control file for DIF

generate_resp <- function(N = 1000, I = 20,
                          m1 = 0.0, sd1 = 1,
                          m2 = 0.5, sd2 = 1) {
  # generate thetas with two distinct groups (e.g. boys and girls) possibly
  # having different means and sds
  theta <- c(rnorm(N/2, mean = m1, sd = sd1),
             rnorm(N/2, mean = m2, sd = sd2)) # student abilities
  grp <- rep(1:2, each = N/2) # group membership
  delta <- seq(from = -3, to = 3, length.out = I)  # item difficulties
  p1 <- plogis(outer(theta, delta, "-"))  # probability of correct answer
  resp_prob <- matrix(runif(N * I), nrow = N, ncol = I)
  resp_prob[, 1] <- resp_prob[N/2, 1] + runif(N,  0.00, 0.25) # make item 1 slightly easier for first group
  resp_prob[, 2] <- resp_prob[N/2, 1] + runif(N, -0.25, 0.00) # make item 2 slightly harder for first group
  resp <- 1 * (p1 > resp_prob)  # generated item responses
  colnames(resp) <- paste("I", 1:I, sep = "")
  resp <- as.data.frame(resp)
  resp$grp <- grp
  return(resp)
}

library(tidyverse)
set.seed(123)
resp <- generate_resp() %>%
  as_tibble() %>%
  unite(items, -grp, sep = "") %>%
  unite(resps, sep = " ")

write_lines(resp$resps, here::here("data-raw", "dif.txt"))
