#' Newton-Raphson Method
#'
#' The Newton-Raphson Method is an iterative algorithm for calculating the root
#'   (x-intercept) of an arbitrary function `f(x)`.
#'
#' The idea is to start with an initial guess which is reasonably close to the
#'   true root, then to approximate the function by its tangent line using
#'   calculus, and finally to compute the x-intercept of this tangent line by
#'   elementary algebra. This x-intercept will typically be a better
#'   approximation to the original function's root than the first guess, and
#'   the method can be iterated until some threshold tolerance is crossed.
#'
#' Newton-Raphson Method for calculating next `x1` has the equation:
#'
#' \deqn{
#'  x1 = x0 - f(x0) / f'(x0)
#' }
#'
#' @param init `numeric(1)`. The initial `x` value to begin
#'   Newton-Raphson steps.
#' @param tol `numeric(1)`. The tolerance for the error in the root.
#'   Used to stop the algorithm once the root estimate no longer
#'   changes by this much in `x`.
#'
#' @return The function generates a \pkg{ggplot2} of the algorithm, as well
#'   as a list containing:
#'   \item{x_traj}{The iterative "guesses" for the function root.}
#'   \item{niter}{The number of iterations required to find the root.}
#'   \item{root}{The estimate of the root (x-intercept).}
#'
#' @examples
#' newton_raphson(3)
#' newton_raphson(-3)
#' @source \url{https://en.wikipedia.org/wiki/Newton%27s_method}
#' @importFrom ggplot2 geom_function ggplot aes ggtitle labs geom_point
#' @importFrom ggplot2 geom_line geom_vline geom_hline
#' @export
newton_raphson <- function(init, tol = 1e-04) {

  # set arbitrary x-limits
  x <- seq(-4, 4)

  # objective function to get root
  fx <- function(x) exp(-x^2) - x^3 + 2

  # objective function (first derivative)
  newtonRaphson <- function(x) {
    x - ((exp(-x^2) - x^3 + 2) / (-2 * exp(-x^2) - 3 * x^2))
  }

  x_tr <- x1 <- init  # initial guess
  old  <- init + 1    # sets arbitrary 'old' to enter first loop
  iter <- 1

  while ( abs(old - x1) > tol ) {
    old  <- x1
    x1   <- newtonRaphson(x1)
    iter <- iter + 1
    x_tr[iter] <- x1
  }

  # summary plot
  p1 <- ggplot(data.frame(x = x), aes(x)) +
    geom_function(fun = fx, colour = "blue", size = 1) +
    geom_vline(xintercept = init, colour = "green", size = 0.5, linetype = "dashed") +
    geom_vline(xintercept = x_tr[-1L], colour = "red", alpha = 0.5) +
    geom_hline(yintercept = 0, colour = "black") +
    ggtitle("Newton-Raphson Method", subtitle = sprintf("root = %0.3f", x1)) +
    labs(title = "Newton-Raphson Method", y = "f(x)")

  # algorithm trajectory
  p2 <- ggplot() +
    geom_line(aes(y = x_tr, x = seq_along(x_tr))) +
    geom_point(aes(y = x_tr, x = seq_along(x_tr)), size = 2) +
    ggtitle("Algorithm Trajectory") +
    labs(y = "Root proposal", x = "Iteration")
  withr::with_namespace("patchwork", print(p2 + p1))
  list(x_traj = x_tr, niter = iter, root = x1)
}
