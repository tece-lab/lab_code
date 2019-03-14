#--- CLUSTER FUNCTIONS -----
# These functions are meant to work on a folder structure of the kind:
# dropbox/university/'project_name'
# This is my library to analyze sls results downloading the results
# from the cluster using R.
# To use them is strongly advised to create a couple public + private keys
# on the cluster and then copying them in the .ssh folder on your local
# machine. Remember to change your p-number.
# Functions that can be useful for a generic user but that need to be modified:
# "download_results"
# "check_jobs"
# "upload_cluster_scripts"
# "execute_experiment" (needs a bash script on cluster)
# "execute_next_setup"

#' @title Get dropbox folder address
#' @author Giovanni Laudanno
#' @description Get dropbox folder address
#' @inheritParams default_params_doc
#' @return dropbox folder address
get_dropbox_folder <- function() {

  if (!require("RJSONIO")) {install.packages("RJSONIO")}

  if (Sys.info()['sysname'] == 'Darwin')
  {
    info <- RJSONIO::fromJSON(
      file.path(path.expand("~"),'.dropbox','info.json'))
  }
  if (Sys.info()['sysname'] == 'Windows')
  {
    info <- RJSONIO::fromJSON(
      if (file.exists(file.path(Sys.getenv('APPDATA'), 'Dropbox','info.json'))) {
        file.path(Sys.getenv('APPDATA'), 'Dropbox', 'info.json')
      } else {
        file.path(Sys.getenv('LOCALAPPDATA'),'Dropbox','info.json')
      }
    )
  }
  dropbox_base <- info$personal$path
  dropbox_base
}

#' @title Get project folder address
#' @author Giovanni Laudanno
#' @description Get project folder address
#' @inheritParams default_params_doc
#' @return project folder address
get_project_folder <- function(project_name = get_pkg_name()) {
  db_dir <- get_dropbox_folder()
  home_dir <- db_dir
  home_dir <- file.path(db_dir, "university", "Progress")
  home_files <- list.files(paste0(home_dir))
  project_folder <- home_files[which(grepl(
    pattern = project_name,
    x = home_files
  ))]
  project_folder <- file.path(home_dir, project_folder)
  project_folder
}

#' @title Read saved results
#' @author Giovanni Laudanno
#' @description Read saved results
#' @inheritParams default_params_doc
#' @return results
read_results <- function(project_name = get_pkg_name()) {
  project_folder <- get_project_folder(project_name)
  if (is.null(project_folder)) {
    if (.Platform$OS.type == "windows") {
      project_folder <- system.file("extdata", package = get_pkg_name())
    }
  }
  if (!dir.exists(project_folder)) {
    stop("This directory does not exist")
  }
  if (length(list.files(project_folder)) == 0) {
    stop(paste0(project_folder, " is empty."))
  }
  dir_results <- file.path(project_folder, "results")
  files_results <- list.files(dir_results, pattern = ".txt")
  if (length(files_results) == 0) {
    stop(paste0(dir_results, " is empty."))
  }
  all_results <- data.frame()
  for (file_results in files_results) {
    x <- utils::read.csv(
      file.path(
        dir_results,
        file_results
      )
    )[, -1]
    all_results <- rbind(all_results, x)
  }
  return(all_results)
}

#' @title Subsets the data according to the experiment
#' @author Giovanni Laudanno
#' @description Subsets the data according to the experiment
#' @inheritParams default_params_doc
#' @return subset data
subset_experiment_data <- function(data) {
  # project_folder <- get_project_folder(project_name)
  setups <- experiment_setup()
  data <- data[data$sim_lambda_m %in% setups$lambda_m, ]
  data <- data[data$sim_mu_m %in% setups$mu_m, ]
  data <- data[data$sim_lambda_s %in% setups$lambda_s, ]
  data <- data[data$sim_mu_s %in% setups$mu_s, ]
  data <- data[data$cond %in% setups$cond, ]
  data <- data[data$t_0_1 %in% setups$crown_age, ]
  data <- data[data$t_0_2 %in% setups$shift_time, ]
  data
}

#' @title Results overview
#' @author Giovanni Laudanno
#' @description Results overview
#' @inheritParams default_params_doc
#' @return Results overview
results_overview <- function(data = NULL) {
  if (is.null(data)) {
    data <- subset_experiment_data(read_results())
  }
  setups <- experiment_setup()
  suppressWarnings(
    model <- cut_loglik_from_name(sls_logliks_experiment())
  )
  set1 <- cbind(setups, model[1])
  set2 <- cbind(setups, model[2])
  names(set1) <- names(set2) <- c(
   "sim_lambda_m",
   "sim_mu_m",
   "sim_lambda_s",
   "sim_mu_s",
   "cond",
   "t_0_1",
   "t_0_2",
   "model"
  )
  setups <- rbind(set1, set2)
  data$setting <- interaction(
    data$sim_lambda_m,
    data$sim_mu_m,
    data$sim_lambda_s,
    data$sim_mu_s,
    data$cond,
    data$t_0_1,
    data$t_0_2,
    data$model
  )
  settings <- unique(data$setting)
  overview <- data.frame()
  i <- 1
  for (s in settings) {
    how_many <- length(which(data$setting == s))
    pars <- unique(data[which(data$setting == s), names(setups)])
    overview[i, names(setups)] <- pars
    overview$N[i] <- how_many
    i <- i + 1
  }
  rownames(overview) <- NULL
  overview
}

#' @title Download the results to the results folder of the project
#' @author Giovanni Laudanno
#' @description Download the results to the results folder of the project
#' @inheritParams default_params_doc
#' @return nothing
download_results <- function(
  project_name = get_pkg_name(),
  connection = NULL
) {

  project_folder <- get_project_folder(project_name)
  if (!require(ssh)) {install.packages("ssh")}
  if (is.null(connection)) {
    connection <- ssh_connect("p274829@peregrine.hpc.rug.nl")
  }
  remote_results_folder <- file.path(get_pkg_name(), "results")
  local_results_folder <- file.path(project_folder, "results")
  testit::assert(dir.exists(local_results_folder))
  system.time(
    scp_download(
      session = connection,
      files = paste0(remote_results_folder, "/*"),
      to = local_results_folder
    )
  )
  rm(connection); gc()
  return()
}

#' @title Export cluster scripts
#' @author Giovanni Laudanno
#' @description Export cluster scripts
#' @inheritParams default_params_doc
#' @return nothing
upload_cluster_scripts <- function(project_name = get_pkg_name()) {

  project_folder <- get_project_folder(project_name)
  remote_project_folder <- file.path(project_name)
  local_cluster_folder <- file.path(project_folder, "cluster_scripts")
  testit::assert(dir.exists(local_cluster_folder))
  if (!require(ssh)) {install.packages("ssh")}
  x <- ssh_connect("p274829@peregrine.hpc.rug.nl")
  ssh_exec_wait(x, command = paste0("mkdir -p ", project_name))
  system.time(
    scp_upload(
      session = x,
      files = paste0(
        local_cluster_folder,
        "/",
        list.files(local_cluster_folder, pattern = ".bash")
      ),
      to = remote_project_folder
    )
  )
  rm(x); gc()
  return()
}

#' @title Execute experiment
#' @author Giovanni Laudanno
#' @description Execute experiment
#' @inheritParams default_params_doc
#' @return nothing
execute_experiment <- function(project_name = get_pkg_name()) {

  if (!require(ssh)) {install.packages("ssh")}
  x <- ssh_connect("p274829@peregrine.hpc.rug.nl")
  bash_file <- file.path(
    project_name,
    paste0(project_name, "_experiment_test.bash")
  )
  ssh_exec_wait(session = x, command = paste0("cat ", bash_file))
  ssh_exec_wait(session = x, command = paste0("sbatch ", bash_file))
  ssh_exec_wait(session = x, command = "squeue -u $USER --long")
  rm(x); gc()
  return()
}

#' @title Check jobs on cluster
#' @author Giovanni Laudanno
#' @description Check jobs on cluster
#' @inheritParams default_params_doc
#' @return nothing
check_jobs <- function() {

  if (!require(ssh)) {install.packages("ssh")}
  x <- ssh_connect("p274829@peregrine.hpc.rug.nl")
  ssh_exec_wait(session = x, command = "squeue -u $USER --long")
  ssh_exec_wait(session = x, command = "sshare -u $USER")
  rm(x); gc()
  return()
}

#' @title Experiment setups
#' @author Giovanni Laudanno
#' @description Experiment setups
#' @inheritParams default_params_doc
#' @return Experiment setups
experiment_setup <- function() {
  lambda_m_vec <- c(0.2, 0.3, 0.4, 0.5)
  mu_m_vec <- c(0, 0.05, 0.1, 0.2)
  lambda_s_vec <- c(0.6)
  mu_s_vec <- c(0.1)
  cond_vec <- c(2, 3)
  crown_age_vec <- c(15)
  shift_time_vec <- c(5, 10)
  setups <- expand.grid(
    lambda_m_vec,
    mu_m_vec,
    lambda_s_vec,
    mu_s_vec,
    cond_vec,
    crown_age_vec,
    shift_time_vec
  )
  colnames(setups) <- c(
    "lambda_m",
    "mu_m",
    "lambda_s",
    "mu_s",
    "cond",
    "crown_age",
    "shift_time"
  )
  setups
}

#' @title Find next setup
#' @author Giovanni Laudanno
#' @description Find next setup
#' @inheritParams default_params_doc
#' @return nothing
find_next_setup <- function(
  project_name = get_pkg_name(),
  success_threshold = 0.8
) {
  data <- read_results(project_name = project_name)
  max_sims <- 1000
  setups <- experiment_setup()
  for (s in 1:nrow(setups)) {
    setup <- setups[s, ]
    data_setup_names <- c(
      "sim_lambda_m",
      "sim_mu_m",
      "sim_lambda_s",
      "sim_mu_s",
      "cond",
      "t_0_1",
      "t_0_2"
    )
    cm <- data[, colnames(data) %in% data_setup_names]
    coords <- which(apply(cm, MARGIN = 1, FUN = function(x) all(x == setup)) == TRUE)
    missing_seeds <- (1:max_sims)[!(1:max_sims %in% data[coords, ]$seed)]
    if (length(missing_seeds) > success_threshold * max_sims) {
      right_setup <- setup
      break
    }
  }
  right_setup
}

#' @title Execute next setup
#' @author Giovanni Laudanno
#' @description Execute next setup
#' @inheritParams default_params_doc
#' @return nothing
execute_next_setup <- function(
  project_name = get_pkg_name(),
  max_sims = 1000
  ) {

  if (!require(ssh)) {install.packages("ssh")}
  connection <- ssh_connect("p274829@peregrine.hpc.rug.nl")

  download_results(project_name = project_name, connection = connection)

  project_folder <- get_project_folder(project_name)
  right_setup <- find_next_setup(project_name = project_name)
  if (is.null(project_folder)) {
    if (.Platform$OS.type == "windows") {
      project_folder <- system.file("extdata", package = get_pkg_name())
    }
  }
  if (!dir.exists(project_folder)) {
    stop("This directory does not exist")
  }
  if (length(list.files(project_folder)) == 0) {
    stop(paste0(project_folder, " is empty."))
  }
  dir_results <- file.path(project_folder, "results")
  files_results <- list.files(dir_results, pattern = ".txt")
  pars_string <- paste(paste(unname(right_setup[1:4])), collapse = "-")
  times_string <- paste(paste(unname(right_setup[6:7])), collapse = "-")
  cond_string <- paste0("cond=", right_setup$cond)
  right_files <- files_results[which(
    grepl(pattern = pars_string, x = files_results) &
      grepl(pattern = times_string, x = files_results) &
      grepl(pattern = cond_string, x = files_results)
  )]
  right_results <- data.frame()
  for (file_results in right_files) {
    y <- utils::read.csv(
      file.path(
        dir_results,
        file_results
      )
    )[, -1]
    right_results <- rbind(right_results, y)
  }
  seeds <- unique(right_results$seed)
  missing_seeds <- (1:max_sims)[-seeds]

  bash_file <- file.path(
    project_name,
    paste0(project_name, "_single_seed.bash")
  )
  ssh_exec_wait(session = connection, command = paste0("cat ", bash_file))
  for (seed in missing_seeds) {
    ssh_exec_wait(session = connection, command = paste0(
      "sbatch ",
      bash_file,
      " ",
      right_setup$lambda_m,
      " ",
      right_setup$mu_m,
      " ",
      right_setup$lambda_s,
      " ",
      right_setup$mu_s,
      " ",
      right_setup$cond,
      " ",
      right_setup$crown_age,
      " ",
      right_setup$shift_time,
      " ",
      seed,
      " ",
      "gelifes"
    )
    )
  }
  rm(connection); gc()
}

#--- FUNCTION CALLS -----
if (1 == 2) {
  upload_cluster_scripts()
  # execute_experiment()
  find_next_setup()
  execute_next_setup()
  check_jobs()
  download_results()
  data <- subset_experiment_data(read_results())
  results_overview(data = data)
  showplot_box(data = subset_experiment_data(read_results()))
}
