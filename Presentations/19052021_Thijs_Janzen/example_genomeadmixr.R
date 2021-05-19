library(GenomeAdmixR)

simulated_pop <- simulate_admixture(pop_size = 10000,
                                    total_runtime = 100,
                                    num_threads = -1,
                                    verbose = TRUE)

plot(simulated_pop$population[[1]])











# now with more than 2 ancestries:
simulated_pop <- simulate_admixture(
                        module = ancestry_module(number_of_founders = 10),
                        pop_size = 10000,
                        total_runtime = 100,
                        num_threads = -1,
                        verbose = TRUE)
plot(simulated_pop$population[[10]])

plot_frequencies(simulated_pop)









# let's impose selection!
s <- 0.1
h <- 0.5
selection_matrix <- matrix(nrow = 1, ncol = 5)
selection_matrix[1, ] <- c(0.5, 1, 1 + h * s, 1 + s, 1)

simulated_pop <- simulate_admixture(
  module = ancestry_module(number_of_founders = 10,
                           markers = seq(0.4, 0.6, by = 0.01)),
  pop_size = 10000,
  total_runtime = 100,
  num_threads = -1,
  select_matrix = selection_matrix,
  verbose = TRUE)

plot_over_time(simulated_pop$frequencies, 0.5)

plot_joyplot_frequencies(simulated_pop$frequencies, time_points = c(0, 10, 20, 40,  50, 75, 99))







s <- 0.05
h <- 0.5
selection_matrix <- matrix(nrow = 2, ncol = 5)
selection_matrix[1, ] <- c(0.4, 1, 1 + h * s, 1 + s, 1)
selection_matrix[2, ] <- c(0.5, 1, 1 + h * s, 1 + s, 2)







simulated_pop <- simulate_admixture(
  module = ancestry_module(number_of_founders = 4,
                           markers = seq(0.3, 0.6, by = 0.01)),
  pop_size = 10000,
  total_runtime = 100,
  num_threads = -1,
  select_matrix = selection_matrix
  verbose = TRUE)

plot_over_time(simulated_pop$frequencies, 0.5)
plot_over_time(simulated_pop$frequencies, 0.4)


plot_joyplot_frequencies(simulated_pop$frequencies, time_points = c(0, 10, 20, 40,  50, 75, 99))


