#######################
# I'm having trouble transposing a list and retaining row names
# problem is that in transforming a data frame to a list and back,
# you lose the rownames (because rownames make no sense for a list of vectors
#######################

# create list of lists (actually data frames)
xl <- list(A=data.frame(a=1:3, b=4:6, c=7:9),
           B=data.frame(a=10:12, b=13:15, c=16:18),
           C=data.frame(a=19:21, b=22:24, c=25:27))

# add rownames
for (i in names(xl)) rownames(xl[[i]]) <- c("foo","bar","bash")

# print it
xl


#' Transpose a List
#' 
#' Turns a list-of-lists "inside-out"; it turns a pair of
#' lists into a list of pairs, or a list of pairs into pair of lists.
#' For example, if you had a list of length n where each component
#' had values \code{a} and \code{b}, \code{transposeList()} would make a list with
#' elements \code{a} and \code{b} that contained lists of length \code{n}. 
#' It's called transpose because \code{x[[1]][[2]]} is equivalent 
#' to \code{transpose(x)[[2]][[1]]}.
transposeList <- function(x) {
      Reduce(intersect, lapply(x, names)) %>% {
               lapply(., function(com) lapply(x, "[[", com)) %names% .}
}


# now you want to transpose xl, pull out the first element of each
# data frame, make a new data frame of the three 1st columns, 
# then repeat for column 2 and 3.
txl <- transposeList(xl)   # transpose 
txl

# what we actually want to do in the end is this
txl <- transposeList(xl) %>% lapply(data.frame)   # transpose & return to data frame
txl



# The problem is that we lose the rownames while doing this; how do we keep them?
# an ugly way is saving them up front and sticking them back on after
# that's also dangerous because the rownames aren't always guaranteed to be in same order

# So far I've tried this:
rns <- rownames(xl[[1]])   # store up front
txl <- transposeList(xl) %>% lapply(data.frame, row.names=rns)   # ugly & dangerous; assumes all same
txl

# and this (which is fails and is wrong for many reasons):
txl <- xl %>% {
   rns <- lapply(., rownames); print(rns); .}
   transposeList %>% { lapply(., data.frame, row.names=rn(.)) }


# but I think there ought to be a way with "%>%" to keep the element-specific rownames
# temporarily stored within the scope, add them back, then lost them when the scope closes
# nice, safe, and clean ... but I can't seem to figure it out
# Ideas???

