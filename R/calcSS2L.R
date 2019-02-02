# ------------------------------------------------------------------------
## testing a two-factor A,B decomposition, with B(A) and possibly unbalanced
# y_ijk = mu + alpha_i + beta_ij(i) + r_ij(i)k(i,j)
# We have:
# a     levels of A ( = length(x))
# b_i   levels of B(A_i) ( = length(x[[i]]))
# n_ij  replicates in B(A_i)_j ( = length(x[[i]][[j]]))
#
# The decomposition of SS is then
# SST = SSA + SSB + SSR
# with:
# SST = sum_i sum_j sum_k (y_ijk - y_...)^2 (total variation)
# SSR = sum_i sum_j sum_k (y_ijk - y_ij.)^2 (variation within each cell)
# SSB = sum_i sum_j n_ij (y_ij. - y_i..)^2 (variation across levels of B, sumed over all A_i)
# SSA = sum_i b_i (y_i.. - y_...)^2 (variation across levels of A)
# ------------------------------------------------------------------------

#' Two Factor Decomposition
#'
#' Description here (Eduardo) ...
#'
#' @param x a list of lists of vectors. The top level corresponds to levels of A,
#' the next one to levels of B (within each level of A), the final one is a vector
#' with the replicate values in the cell
#' @return a named vector of the sum of squares, named by their source:
#' \item{SST (total)}
#' \item{SSA (top level factor A)}
#' \item{SSB (second level, nested within A)}
#' \item{SSR (replicates, at cell level)}
#' @examples
#' \dontrun{
#' xdf <- read.csv(header = TRUE, stringsAsFactors = FALSE, text = "FEN1.12577.100.3", Robot, PlateId,
#' 811.5764,F1,Set 1
#' 775.4565,F1,Set 1
#' 799.5199,F1,Set 1
#' 765.9079,F1,Set 1
#' 788.8087,F1,Set 1
#' 788.8087,F1,Set 2
#' 802.8240,F1,Set 2
#' 773.7064,F1,Set 2
#' 784.7526,F1,Set 2
#' 797.6161,F1,Set 3
#' 766.6315,F1,Set 3
#' 784.0661,F1,Set 3
#' 816.1028,F1,Set 3
#' 788.8087,F1,Set 3
#' 801.6925,F2,Set 4
#' 788.8087,F2,Set 4
#' 759.9591,F2,Set 4
#' 790.6826,F2,Set 4
#' 780.8350,F2,Set 5
#' 788.8087,F2,Set 5
#' 802.2715,F2,Set 5
#' 796.9418,F2,Set 5
#' 771.4564,F2,Set 5
#' 808.4200,F2,Set 6
#' 788.8087,F2,Set 6
#' 784.6606,F2,Set 6
#' 819.8652,F2,Set 6
#' 770.0933,F2,Set 6
#' 788.8087,F3,Set 8
#' 786.3458,F3,Set 8
#' 801.2640,F3,Set 8
#' 793.1742,F3,Set 8
#' 789.7181,F3,Set 9
#' 794.5069,F3,Set 9
#' 788.8087,F3,Set 9
#' 785.0509,F3,Set 9
#' 784.4407,F3,Set 9
#' )
#' y <- lapply(split(xdf, xdf$Robot), function(l) lapply(split(l, l$PlateId), function(df) df$FEN1.12577.100.3))
#' # $F1
#' # $F1$`Set 1`
#' # [1] 811.5764 775.4565 799.5199 765.9079 788.8087
#' #
#' # $F1$`Set 2`
#' # [1] 788.8087 808.7406 802.8240 773.7064 784.7526
#' #
#' # $F1$`Set 3`
#' # [1] 797.6161 766.6315 784.0661 816.1028 788.8087
#' #
#' #
#' # $F2
#' # $F2$`Set 4`
#' # [1] 801.6925 788.8087 759.9591 788.7498 790.6826
#' #
#' # $F2$`Set 5`
#' # [1] 780.8350 788.8087 802.2715 796.9418 771.4564
#' #
#' # $F2$`Set 6`
#' # [1] 808.4200 788.8087 784.6606 819.8652 770.0933
#' #
#' #
#' # $F3
#' # $F3$`Set 8`
#' # [1] 788.8087 786.3458 801.2640 793.1742 787.4628
#' #
#' # $F3$`Set 9`
#' # [1] 789.7181 794.5069 788.8087 785.0509 784.4407
#' #
#'
#' calcSS2L(y)
#' # SST         SSA         SSB         SSR
#' # 6675.181441    9.869363  258.505639 6406.806439
#'
#' # With R Anova:
#' xdf$Robot <- factor(xdf$Robot)
#' xdf$PlateId <- factor(xdf$PlateId)
#' contrasts(xdf$PlateId) <- contr.sum
#' car::Anova(stats::aov(FEN1.12577.100.3 ~ Robot/PlateId, xdf), type = "II")
#'
#' # Anova Table (Type II tests)
#' # Response: FEN1.12577.100.3
#' # Sum Sq Df F value Pr(>F)
#' # Robot            9.9  2  0.0223 0.9779
#' # Robot:PlateId  258.5  5  0.2340 0.9444
#' }
#' @export
calcSS2L <- function(x) {
  # get the a, b, n_A, n
  a   <- length(x)
  b   <- sapply(x, length)
  n   <- lapply(x, function(xi) sapply(xi, length)) # nreps in each cell
  n_A <- sapply(n, sum)                             # nreps within each level of A
  # compute means at the different levels
  x_all <- unlist(x)
  mu_T  <- mean(x_all)       # global mean: y_...
  x_A   <- lapply(x, unlist)
  mu_A  <- sapply(x_A, mean) # mean for each level of A: y_i..
  #x_B   <- lapply(x, function(xi) lapply(xi, unlist)) # this is the logical progression; here we get x back
  mu_B  <- lapply(x, function(xi) sapply(xi, mean)) # mean for each level of B(A_i) (each cell here): y_ij.
  # compute the sums
  # SST <- sum(sapply(x, function(x_i) sum(sapply(x_i, function(x_ij) sum((x_ij-mu_T)^2))))) # easier to do with x_all, but wanted to make a point
  SST <- sum((x_all - mu_T)^2)
  SSA <- sum(n_A * (mu_A - mu_T)^2)
  SSB <- sum(sapply(seq(a), function(i)
                    sum(n[[i]] * (mu_B[[i]] - mu_A[[i]])^2)))
  SSR <- sum(sapply(seq(a), function(i)
             sum(sapply(seq(b[i]), function(j)
                 sum((x[[i]][[j]] - mu_B[[i]][j])^2)))))
  return(c(SST = SST, SSA = SSA, SSB = SSB, SSR = SSR))
}
