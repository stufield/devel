# ---------------------------------
# Create static objects for package
# ---------------------------------

tree_data <- tibble::tibble(
  tree = 1:20,
  spp = c("PICO", "ABLA", "ABLA", "PICO", "POTR",
          "POTR", "ABLA", "ABLA", "ABLA", "PICO",
          "PICO", "POTR", "POTR", "POTR", "PICO",
          "PIFL", "PIFL", "PIFL", "PIFL", "PIFL"),
  season = c("Summer", "Fall", "Summer", "Spring", "Winter",
             "Winter", "Summer", "Spring", "Fall", "Fall",
             "Spring", "Summer", "Spring", "Winter", "Winter",
             "Fall", "Winter", "Summer", "Spring", "Fall"),
  Infected = c("Yes", "Yes", "No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No",
               "Yes", "No", "No", "No", "Yes", "No", "No", "Yes", "No", "Yes"),
  dbh = c(42.2, 13.4, 13.1, 15.0, 14.2, 20.0,  9.1,  9.2, 27.3, 11.2,
          18.0, 9.0, 15.4, 24.1, 24.2, 14.4, 13.1, 21.5, 13.4, 16.2),
  SapDepth = c(8.4, 2.0, 1.5, 4.1, 3.6, 5.9, 2.1, 2.5, 3.0, 3.4,
               5.9, 3.0, 4.1, 6.2, 7.0, 4.2, 2.2, 5.1, 2.2, 3.5),
  BarkThick = c(0.3, 0.4, 0.3, 0.2, 0.0, 0.0, 0.3, 0.5, 0.6, 0.3,
                0.3, 0.0, 0.0, 0.0, 0.5, 0.5, 0.6, 0.6, 0.5, 0.5),
  NobarkArea = c(1359.2, 124.7, 122.7, 167.4, 158.4, 314.2, 56.7, 52.8, 535.0, 88.2,
                 237.8, 63.6, 186.3, 456.2, 422.7, 141.0, 111.2, 323.7, 120.8, 181.5),
  Heartwood = c(483.1, 58.1, 70.9, 32.2, 38.5, 52.8, 14.5, 8.0, 317.3, 11.3,
                24.6, 7.1, 40.7, 107.5, 66.5, 19.6, 44.2, 80.1, 50.3, 52.8),
  SapArea = c(876.1, 66.6, 51.8, 135.2, 119.9, 261.3, 42.2, 44.8, 217.7, 76.9,
              213.2, 56.5, 145.5, 348.7, 356.3, 121.4, 67.0, 243.5, 70.5, 128.6)
)

Ri_data <- data.frame(
  one   = c(380, 376, 360, 368, 372, 366, 374, 382, NA, NA, NA, NA, NA),
  two   = c(350, 356, 358, 376, 338, 342, 366, 350, 344, 364, NA, NA, NA),
  three = c(354, 360, 362, 352, 366, 372, 362, 344, 342, 358, 351, 348, 348),
  four  = c(376, 344, 342, 372, 374, 360, NA, NA, NA, NA, NA, NA, NA)
)


set.seed(101)
test_data <- tibble::tibble(
  pid       = sprintf("%02i", 1:40),
  Pop       = rep(utils::head(LETTERS, 10), 4),
  Sample    = sample(c("small", "medium", "large"), 40, replace = TRUE),
  TimePoint = rep(c("baseline", "6 months", "12 months", "24 months"), each = 10),
  a         = round(stats::rnorm(40, mean = 160, sd = 20), 0),
  b         = sample(0:6, 40, replace = TRUE),
  seq.1234.56 = stats::rnorm(40, mean = 25, sd = 3.5),
  seq.6969.42 = stats::rnorm(40, mean = 25, sd = 1.5),
  x         = stats::rnorm(40, mean = 5, sd = 3),
  y         = round(stats::runif (40, 10, 20), 0),
  z         = round(stats::rnorm(40, mean = 1, sd = 0.5), 3),
  Response  = factor(sample(c("Control", "Disease"), 40, replace = TRUE))
)

test_data <- dplyr::mutate(
  test_data,
  Sample    = factor(Sample, levels = c("small", "medium", "large")),
  TimePoint = factor(TimePoint,
                     levels = c("baseline", "6 months", "12 months", "24 months"))
)
