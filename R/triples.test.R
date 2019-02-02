
#' Triples Test
#'
#' Doesn't quite work right yet ...
#'
#' @param x A numeric vector of values
#' @param alpha Significance level
#' @return return_value
#' @note Ho: null hypothesis is that the data are symmetric:
#' \code{x = c(17.4, 17.9, 17.6, 18.1, 17.6)}
#' @author Stu Field
#' @references From Hollander & Wolfe (1999); NonParametric Statistical Methods, pg 87-91.
#' @examples
#'
#'
#' @export
triples.test <- function(x, alpha=0.05) {

   n <- length(x)

   #if ( n <=10 )
   #   warning('This test is inaccurate for distributions of n < 10')

   comb.iter <- t(combn(n,3))
   #print(dim(comb.iter))
   Zi <- sapply(seq(nrow(comb.iter)), function(i) {
                ijk <- comb.iter[ i,]
                lo <- x[ ijk[1] ]
                middle <- x[ ijk[2] ]
                up <- x[ ijk[3] ]
                sign(lo+middle-2*up) + sign(lo+up-2*middle) + sign(middle+up-2*lo)
            })

   #print(Zi)
   TT <- sum(Zi)
   Bt <- sapply(1:n, function(i) {
                which.i <- which(comb.iter==i, arr.ind=TRUE)[,1]
                #print(Zi[ which.i ])
                sum(Zi[ which.i ])
            })

   print(Bt)

   comb.iter.2 <- t(combn(n,2))   # all n.choose.2 pairwise combos
   #print(dim(comb.iter.2))
   Bst <- sapply(seq(nrow(comb.iter.2)), function(i) {
                 which.st <- apply(comb.iter, 1, function(x) all(comb.iter.2[i,]%in%x))
                 #print(Zi[ which.st ])
                 sum(Zi[ which.st ])
            })

   print(sum(Bst^2))

   sigma1 <- sum(Bt^2) * ((n-3)*(n-4))/((n-1)*(n-2))
   sigma2 <- sum(Bst^2) * (n-3)/(n-4)
   sigma3 <- (n*(n-1)*(n-2)) / 6
   sigma4 <- (1 - ((n-3)*(n-4)*(n-5)) / sigma3*6) * TT^2
   #print(c(sigma1,sigma2,sigma3,sigma4))

   var <- sigma1 + sigma2 + sigma3 - sigma4
   #print(var)

   z <- TT / sqrt(var)
   p <- 2 * pnorm(-abs(z))

   if ( p>alpha )
      cat('*  Ho = TRUE; the data are symmetric ...\n')
   else
      cat('*  Ho = FALSE; heavy tails detected ...\n')

   data.frame(Zstat=z, p.value=p, Ho=p>alpha)

}





# David's version?
test.triples.x <- function(x) {

   n=length(x)
   TT = 0
   Tx = numeric(n)
   t = tic()
   Txy = matrix(0,nrow=n-1,ncol=n)
   for (i in 1:(n-2)) {
      for (j in (i+1):(n-1)) {
         for (k in (j+1):n) {
            RL <- sign(x[i]+x[k]-x[j]-x[j])
            if ( RL!=0 ) {
               TT = TT + RL
               Tx[c(i,j,k)] = Tx[c(i,j,k)] + RL
               Txy[i,c(j,k)] = Txy[i,c(j,k)] + RL
               Txy[j,k] = Txy[j,k] + RL
            }
         }
      }
   }
   print(toc(t))
   list(Total=TT, Tx=Tx, Txy=sum(Txy^2))
}


