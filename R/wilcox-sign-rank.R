
wilcox.sign.rank <- function(x, y=NULL, two.tailed=TRUE, verbose=FALSE) {
  # from Hollander & Wolfe; pg 36-42
  # Does NOT implement the continuity correction
  # example from Hollander & Wolfe AND ?wilcox.test
  #x = c(1.83,.5,1.62,2.48,1.68,1.88,1.55,3.06,1.3)
  #y = c(.878,.647,.598,2.05,1.06,1.29,1.06,3.14,1.29)
  #x = c(12500,22300,14500,32300,20800,19200,15800,17500,23300,42100,16800,14500)
  #y = c(11750,20900,14800,29900,21500,18400,14500,17900,21400,43200,15200,14200)

  if ( is.null(y) ) {
    d <- x
    y <- rep(NA,length(x))
  } else {
    d <- x - y
  }

  n <- length(d)
  data <- data.frame(x,y,d,abs=abs(d))

   data$psi <- sapply(1:n, function(i) if (d[i]>0) 1 else 0)
  ord.data <- data[ order(data$abs), ]
  ord.data <- ord.data[ ord.data$abs>0, ]
  n <- nrow(ord.data)
  ord.data$Ri <- rank(ord.data$abs)
  ord.data$Ri.psi <- ord.data$Ri * ord.data$psi
  V <- abs(sum(ord.data$Ri.psi))

  if ( verbose ) {
    print(ord.data)
  }

  if ( n < 10 ) {
    warning("Large-sample Normal approximation of Wilcox Sign Rank test statistic invalid for n < 10")
  }

  # for large-sample approx. W doesn't change when there are tied ranks, but null variance does. 
  # This correction adjusts the variance calculation and reduces to 0 when there are no ties  
  expected.V <- ( n*(n+1) ) / 4
  ties.correction <- 0.5 * sum(sapply(table(ord.data$Ri), function(.x) .x*(.x-1)*(.x+1)))
  var.V <- ( n*(n+1)*(2*n+1) - ties.correction ) / 24
  z <- (V - expected.V) / sqrt(var.V)
  p <- ifelse(two.tailed,2,1) * pnorm(-abs(z))
  data.frame(V=V, n=n, TiesFactor=ties.correction, Zscore=z, p.value=p)
}

# Check
#wilcox.test(x, y, paired=TRUE, exact=FALSE, correct=FALSE)    # H&W large sample approximation

