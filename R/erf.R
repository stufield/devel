#####################################
# General inverse error functions
# this is likely deprecated since R
# has it's own function for the inverse
# error function, namely pnorm()
# this appears to be just a hack
#####################################
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1

erf.inv <- function(x) qnorm((x + 1)/2)/sqrt(2)

erfc <- function(x) 2 * pnorm(x * sqrt(2), lower=FALSE)

