library(Rcpp)
sourceCpp("R_demonstration.cpp")
calc_square(17)

calc_square(2)

vx <- 1:10
calc_squares(vx)

calc_squares(1:10)

input <- matrix(1:4, ncol=2, nrow=2)
input
calc_squares_matrix(input)

rand_numbers <- rngCpp(1e5)
par(mfrow=c(2,2))
hist(rand_numbers[,1], main = "Uniform")
hist(rand_numbers[,2], main = "Normal")
hist(rand_numbers[,3], main = "Gamma")
hist(rand_numbers[,4], main = "Poisson")












sourceCpp("simulation.cpp")
sim_result <- do_simulation(time = 1000,
                            population_size = 1000,
                            mutation_rate = 0.01)
plot(sim_result$mean_trait_values, ylim = c(-1,1), type = "l")

sim_result <- do_simulation(time = 1000,
                            population_size = 1000,
                            mutation_rate = 0.01)

plot(sim_result$mean_trait_values, ylim = c(-1,1), type = "l")

plot(rowMeans(sim_result$trait_distribution), ylim = c(-1,1), type = "l")
minvals <- apply(sim_result$trait_distribution, 1, min)
maxvals <- apply(sim_result$trait_distribution, 1, max)
lines(minvals, col="grey")
lines(maxvals, col = "grey")




