do_complicated_simulation <- function(a, b) {
  tree <- TreeSim::sim.bd.age(age = 5, numbsim = 1, lambda = a,
                              mu = b, complete = FALSE)
  
  if (length(tree[[1]]) > 1) {
    return(tree[[1]]$Nnode)
  }
  return(0)
}

args <- commandArgs(trailingOnly = TRUE)
sim_number <- as.numeric(args[[1]])

param_grid <- expand.grid(spec_rate = seq(0, 1, length.out = 10),
                          ext_rate  = seq(0, 1, length.out = 10))

speciation_rate <- param_grid$spec_rate[sim_number]
extinction_rate <- param_grid$ext_rate[sim_number]

num_species <- do_complicated_simulation(speciation_rate,
                                         extinction_rate)

file_name <- paste0("results_", sim_number, ".txt")

cat(speciation_rate, extinction_rate, num_species,
    file = file_name, append = TRUE)
