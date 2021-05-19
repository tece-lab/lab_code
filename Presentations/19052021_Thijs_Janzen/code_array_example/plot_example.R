library(tidyverse)
setwd("/Users/janzen/example/")
f <- list.files(pattern = "results_*")
found_results <- c()
for (x in f) {
  temp_results <- read_delim(x, delim = " ", col_names = FALSE,
                                    col_types = cols())
  found_results <- rbind(found_results, temp_results)
}

colnames(found_results) <- c("speciation_rate",
                             "extinction_rate",
                             "number_of_species")
found_results <- as_tibble(found_results)
ggplot(found_results, aes(x = speciation_rate, y = number_of_species)) +
  geom_point()
ggplot(found_results, aes(x = extinction_rate, y = number_of_species)) +
  geom_point()

ggplot(found_results, aes(x = extinction_rate, y = speciation_rate)) +
  geom_tile(aes(fill = number_of_species))

