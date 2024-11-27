InfectionByCounty <- utils::read.csv("inst/data-raw/InfectionByCounty.csv", sep = ";") |>
  tibble::as_tibble()
save(InfectionByCounty, file = "data/InfectionByCounty.rda", compress = "xz")
