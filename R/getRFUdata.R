#' Get Data Matrix of RFU Data Only
#'
#' Simple function to subset the feature data (RFU columns)
#' and remove any meta data columns that might be present.
#' Some functions require a simple data matrix of measurements
#' to be passed, this this function strips non-proteomic data
#' and recasts the data frame to a matrix class.
#'
#' @param x A data frame of class `soma_adat`.
#' @return A data matrix of the RFU data only.
#' @author Stu Field
#' @seealso \code{\link{data.matrix}}
#' @examples
#' class(sample.adat)
#' dim(sample.adat)
#' class(getRFUdata(x))
#' dim(getRFUdata(x))
#' @export
getRFUdata <- function(x) {
   data.matrix(x[, getAnalytes(x), drop = FALSE])
}
