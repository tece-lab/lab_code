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

#--- CLUSTER FUNCTIONS -----
#' @title Get cluster accounts
#' @author Giovanni Laudanno
#' @description Get cluster accounts
#' @inheritParams default_params_doc
#' @return Cluster accounts
get_available_accounts <- function() {
  c("p274829", "p257011")
}

#' @title Get dropbox folder address
#' @author Giovanni Laudanno
#' @description Get dropbox folder address
#' @inheritParams default_params_doc
#' @return dropbox folder address
get_dropbox_folder <- function() {

  while (!require("RJSONIO")) {install.packages("RJSONIO")}

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
  if (length(files_results) > 0) {
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
  } else {
    return(NULL)
  }
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
  names(set1) <- names(set2) <- par_names <- c(
   "sim_lambda_m",
   "sim_mu_m",
   "sim_lambda_s",
   "sim_mu_s",
   "cond",
   "t_0_1",
   "t_0_2",
   "model"
  )
  est_pars_names <- c(
    "lambda_m",
    "mu_m",
    "lambda_s",
    "mu_s"
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
    which_ones <- which(data$setting == s)
    how_many <- length(which_ones)
    pars <- unique(data[which_ones, names(setups)])
    overview[i, names(setups)] <- pars
    overview$N[i] <- how_many
    i <- i + 1
  }
  rownames(overview) <- NULL
  overview
}

#' @title Get available results
#' @author Giovanni Laudanno
#' @description Get available results
#' @inheritParams default_params_doc
#' @return Results that you can actually use for plots
get_available_data <- function(
  data,
  max_sims = 1000,
  percentage = 0.8
  ) {
  threshold <- max_sims * percentage
  data <- subset_experiment_data(data)
  good_data <- data[
    (data$lambda_m >= 0 &
      data$mu_m >= 0 &
      data$lambda_s >= 0 &
      data$mu_s >= 0),
    ]
  overview <- results_overview(data = good_data)
  already_done <- overview[which(overview$N > threshold), 1:7]
  par_data <- good_data[,
    names(good_data) %in% names(already_done)
    ]

  available_data <- good_data[
  which(
    apply(X = par_data, MARGIN = 1, FUN = function(x) toString(x)) %in%
      apply(X = already_done, MARGIN = 1, FUN = function(x) toString(x))
  ), ]
  available_overview <- results_overview(available_data)
  testit::assert(all(available_overview$N > threshold))
  available_data
}

#' @title Download the results to the results folder of the project
#' @author Giovanni Laudanno
#' @description Download the results to the results folder of the project
#' @inheritParams default_params_doc
#' @return nothing
download_results <- function(
  project_name = get_pkg_name()
) {

  project_folder <- get_project_folder(project_name)
  remote_results_folder <- file.path(get_pkg_name(), "results")
  local_results_folder <- file.path(project_folder, "results")
  testit::assert(dir.exists(local_results_folder))

  # download files
  if (!require(ssh)) {install.packages("ssh")}
  accounts <- get_available_accounts()
  for (account in accounts) {
    cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
    connection <- ssh_connect(cluster_address)

    system.time(
      scp_download(
        session = connection,
        files = paste0(remote_results_folder, "/*"),
        to = local_results_folder
      )
    )
    rm(connection); gc()
  }
  return()
}

#' @title Export cluster scripts
#' @author Giovanni Laudanno
#' @description Export cluster scripts
#' @inheritParams default_params_doc
#' @return nothing
upload_cluster_scripts <- function(
  project_name = get_pkg_name()
) {

  accounts <- get_available_accounts()

  for (account in accounts) {

    cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
    if (!require(ssh)) {install.packages("ssh")}
    connection <- ssh_connect(cluster_address)

    # folder structure
    project_folder <- get_project_folder(project_name)
    remote_project_folder <- file.path(project_name)
    local_cluster_folder <- file.path(project_folder, "cluster_scripts")
    testit::assert(dir.exists(local_cluster_folder))

    ssh_exec_wait(connection, command = paste0("mkdir -p ", project_name))
    # ssh_exec_wait(connection, command = paste0(
    #   "mkdir -p ",
    #   file.path(project_name, "results", fsep = "/")
    # ))
    system.time(
      scp_upload(
        session = connection,
        files = paste0(
          local_cluster_folder,
          "/",
          list.files(local_cluster_folder, pattern = ".bash")
        ),
        to = remote_project_folder
      )
    )
    rm(connection); gc()
  }
  return()
}

#' @title Execute experiment
#' @author Giovanni Laudanno
#' @description Execute experiment
#' @inheritParams default_params_doc
#' @return nothing
execute_experiment <- function(
  project_name = get_pkg_name(),
  account = "p274829"
) {

  upload_cluster_scripts()

  # connection
  if (account == "cyrus" || account == "Cyrus" || account == "Cy" || account == "cy") { # nolint
    account <- "p257011"
  }
  if (account == "giovanni" || account == "Giovanni" || account == "Gio" || account == "gio") { # nolint
    account <- "p274829"
  }
  cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
  if (!require(ssh)) {install.packages("ssh")}
  connection <- ssh_connect(cluster_address)

  bash_file <- file.path(
    project_name,
    paste0(project_name, "_experiment_test.bash")
  )
  ssh_exec_wait(session = connection, command = paste0("cat ", bash_file))
  ssh_exec_wait(session = connection, command = paste0("sbatch ", bash_file))
  ssh_exec_wait(session = connection, command = "squeue -u $USER --long")
  rm(connection); gc()
  return()
}

#' @title Check jobs on cluster
#' @author Giovanni Laudanno
#' @description Check jobs on cluster
#' @inheritParams default_params_doc
#' @return nothing
check_jobs <- function(account = "p274829") {

  # connection
  if (account == "cyrus" || account == "Cyrus" || account == "Cy" || account == "cy") { # nolint
    account <- "p257011"
  }
  if (account == "giovanni" || account == "Giovanni" || account == "Gio" || account == "gio") { # nolint
    account <- "p274829"
  }
  cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
  if (!require(ssh)) {install.packages("ssh")}
  connection <- ssh_connect(cluster_address)

  ssh_exec_wait(session = connection, command = "squeue -u $USER --long")
  ssh_exec_wait(session = connection, command = "sshare -u $USER")
  rm(connection); gc()
  return()
}

#' @title Experiment setups
#' @author Giovanni Laudanno
#' @description Experiment setups
#' @inheritParams default_params_doc
#' @return Experiment setups
experiment_setup <- function() {
  lambda_m_vec <- c(0.2, 0.3, 0.4, 0.5)
  mu_m_vec <- c(0, 0.05, 0.1, 0.15)
  lambda_s_vec <- c(0.6)
  mu_s_vec <- c(0.1)
  cond_vec <- c(2, 3)
  crown_age_vec <- c(10)
  shift_time_vec <- c(4, 7)
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
  success_threshold = 0.8,
  account,
  max_sims = 1000
) {

  # connection
  if (account == "cyrus" || account == "Cyrus" || account == "Cy" || account == "cy") { # nolint
    account <- "p257011"
  }
  if (account == "giovanni" || account == "Giovanni" || account == "Gio" || account == "gio") { # nolint
    account <- "p274829"
  }

  data <- read_results(project_name = project_name)

  setups <- experiment_setup()
  t_0_2s <- unique(setups$shift_time)
  testit::assert(length(t_0_2s) == 2)
  required_t_0_2 <- t_0_2s[1] * (account == "p274829") + t_0_2s[2] * (account == "p257011")
  setups <- setups[setups$shift_time == required_t_0_2,]
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
    if (is.null(data)) {
      right_setup <- setup
      break
    }
    cm <- data[, colnames(data) %in% data_setup_names]
    coords <- which(apply(cm, MARGIN = 1, FUN = function(x) all(x == setup)) == TRUE)
    missing_seeds <- (1:max_sims)[!(1:max_sims %in% data[coords, ]$seed)]
    if (
      length(missing_seeds) > success_threshold * max_sims & setup$shift_time == required_t_0_2
    ) {
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
  max_sims = 1000,
  account = "p274829",
  download_files = TRUE,
  partition = "gelifes"
) {

  if (!(partition == "gelifes" || partition == "regular")) {
    stop("This is not a legitimate cluster partition")
  }

  project_folder <- get_project_folder(project_name)

  # download files
  if (download_files == TRUE) {
    download_results(project_name = project_name)
  }

  # upload scripts
  upload_cluster_scripts(project_name = project_name)

  # connection
  if (account == "cyrus" || account == "Cyrus" || account == "Cy" || account == "cy") { # nolint
    account <- "p257011"
  }
  if (account == "giovanni" || account == "Giovanni" || account == "Gio" || account == "gio") { # nolint
    account <- "p274829"
  }
  right_setup <- find_next_setup(
    project_name = project_name,
    account = account,
    max_sims = max_sims
  )
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
  if (!is.null(seeds)) {
    missing_seeds <- (1:max_sims)[-seeds]
  } else {
    missing_seeds <- (1:max_sims)
  }

  cluster_address <- paste0(account, "@peregrine.hpc.rug.nl")
  if (!require(ssh)) {install.packages("ssh")}
  connection <- ssh_connect(cluster_address)

  ssh_exec_wait(session = connection, command = paste0(
    "chmod +x ", file.path(project_name, "install_packages.bash")
  ))
  ssh_exec_wait(session = connection, command = paste0(
    "./", file.path(project_name, "install_packages.bash")," 'Giappo/", project_name,"'"
  ))
  ssh_exec_wait(session = connection, command = "sleep 5")
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
      partition
    ))
  }
  rm(connection); gc()
}

#--- PLOT FUNCTIONS -----

#' @title Create boxplots
#' @author Giovanni Laudanno
#' @description Create boxplots
#' @inheritParams default_params_doc
#' @return boxplots for all results
showplot_box <- function(
  data = read_results(),
  project_name = get_pkg_name()
) {

  library(ggplot2); library(reshape2); library(scales)

  project_folder <- get_project_folder(project_name)
  results_folder <- file.path(project_folder, "results")
  models <- unique(data$model)
  models <- sort(models)
  model_comparison_folder <- file.path(results_folder, paste0(models[1], "_vs_", models[2]))
  if (!file.exists(model_comparison_folder)) {
    dir.create(model_comparison_folder, showWarnings = FALSE)
  }
  path <- file.path(model_comparison_folder, "boxplots")
  if (!file.exists(path)) {
    dir.create(file.path(path), showWarnings = FALSE)
  }

  data1 <- data
  data1$sim_lamu <- interaction(data1$sim_lambda_m, data1$sim_mu_m)
  data1_m <- melt(
    data1, id.vars = 'sim_lamu',
    measure.vars = "model"
  )
  data1_m$delta_lambda_m  <- data1$lambda_m - data1$sim_lambda_m
  data1_m$delta_lambda_m2 <- (data1$lambda_m - data1$sim_lambda_m)/data1$sim_lambda_m
  data1_m$delta_mu_m      <- data1$mu_m - data1$sim_mu_m
  data1_m$delta_mu_m2     <- (data1$mu_m - data1$sim_mu_m)/data1$sim_mu_m
  data1_m$delta_mulambda  <- (data1$mu_m/data1$lambda_m) - (data1$sim_mu_m/data1$sim_lambda_m)
  data1_m$model <- data1$model
  data1_m$cond  <- data1$cond
  # levels(data1_m$cond) <- c("No Cond", "Cond 1", "Cond 2")
  xlabels <- unique(paste0(data1$sim_lambda_m, "\n", data1$sim_mu_m))

  pl <- ggplot(
    data1_m
  ) +
    geom_boxplot(
      aes(x = sim_lamu, y = delta_lambda_m, color = model)
    ) +
    facet_grid(
      . ~ cond
    ) +
    xlab(
      bquote("Parameter setting" ~ "(" ~ lambda[M] ~ "," ~ mu[M] ~ ")")
    ) +
    ylab(
      bquote(Delta ~ lambda[M])
    ) +
    theme(
      axis.line = element_line(
        colour = "darkblue",
        size = 1,
        linetype = "solid"
      )
    ) +
    ylim(
      1.5 * min(data1_m[data1_m$model == "sls_p",]$delta_lambda_m),
      1.5 * max(data1_m[data1_m$model == "sls_p",]$delta_lambda_m)
    ) +
    scale_x_discrete(labels = xlabels); pl

  pm <- ggplot(
    data1_m
  ) +
    geom_boxplot(
      aes(x = sim_lamu, y = delta_mu_m, color = model)
    ) +
    facet_grid(
      . ~ cond
    ) +
    xlab(bquote("Parameter setting" ~ "(" ~ lambda[M] ~ "," ~ mu[M] ~ ")")
    ) +
    ylab(
      bquote(Delta ~ mu[M])
    ) +
    theme(
      axis.line = element_line(
        colour = "darkblue",
        size = 1,
        linetype = "solid"
      )
    ) +
    ylim(
      1.5 * min(data1_m[data1_m$model == "sls_p",]$delta_mu_m),
      1.5 * max(data1_m[data1_m$model == "sls_p",]$delta_mu_m)
    ) +
    scale_x_discrete(labels = xlabels); pm

  plm1 <- ggplot(
    data1_m
  ) +
    geom_boxplot(
      aes(x = sim_lamu, y = (delta_lambda_m - delta_mu_m), color = model)
    ) +
    facet_grid(
      . ~ cond
    ) +
    xlab(
      bquote("Parameter setting" ~ "(" ~ lambda[M] ~ "," ~ mu[M] ~ ")")
    ) +
    ylab(
      bquote(Delta ~ (lambda[M] - mu[M]))
    ) +
    theme(
      axis.line = element_line(
        colour = "darkblue",
        size = 1,
        linetype = "solid"
      )
    ) +
    scale_x_discrete(labels = xlabels); plm1

  plm2 <- ggplot(
    data1_m
  ) +
    geom_boxplot(
      aes(x = sim_lamu, y = delta_mulambda, color = model)
    ) +
    facet_grid(
      . ~ cond
    ) +
    xlab(
      bquote("Parameter setting" ~ "(" ~ lambda[M] ~ "," ~ mu[M] ~ ")")
    ) +
    ylab(
      bquote(Delta ~ (mu[M]/lambda[M]))
    ) +
    theme(
      axis.line = element_line(
        colour = "darkblue",
        size = 1,
        linetype = "solid"
      )
    ) +
    scale_x_discrete(labels = xlabels); plm2

  pdfname <- paste0("boxplot_lambda_m")
  grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
  print(pl)
  grDevices::dev.off()

  pdfname <- paste0("boxplot_mu_m")
  grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
  print(pm)
  grDevices::dev.off()

  pdfname <- paste0("boxplot_lambda_m-mu_m")
  grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
  print(plm1)
  grDevices::dev.off()

  pdfname <- paste0("boxplot_ratio_mu_m-lambda_m")
  grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
  print(plm2)
  grDevices::dev.off()

  return(list(pl, pm, plm1, plm2))
}

#' @title Create correlation plots
#' @author Giovanni Laudanno
#' @description Create correlation plots
#' @inheritParams default_params_doc
#' @return correlation plots for all results
showplot_correlation <- function(
  data,
  project_folder
) {

  library(ggplot2); library(reshape2); library(scales)

  results_folder <- get_results_folder()
  models <- unique(data$model)
  models <- sort(models)
  model_comparison_folder <- file.path(results_folder, paste0(models[1], "_vs_", models[2]))
  if (!file.exists(model_comparison_folder)) {
    dir.create(model_comparison_folder, showWarnings = FALSE)
  }
  path <- file.path(model_comparison_folder, "boxplots")
  if (!file.exists(path)) {
    dir.create(file.path(path), showWarnings = FALSE)
  }

  data2 <- data; names(data2)
  data2$parsetting <- interaction(
    data2$sim_lambda_m,
    data2$sim_mu_m,
    data2$sim_lambda_s,
    data2$sim_mu_s,
    data2$cond
  )

  ds <- data2[data2$model == models[1],]
  names(ds) <- paste0("mod1.", names(data2)); names(ds)
  dD <- data2[data2$model == models[2],]
  names(dD) <- paste0("mod2.", names(data2)); names(dD)
  df <- data.frame(ds, dD); names(df)
  df$parsetting <- interaction(
    df$mod1.sim_lambda_m,
    df$mod1.sim_mu_m,
    df$mod1.sim_lambda_s,
    df$mod1.sim_mu_s,
    df$mod1.cond
  )

  for (var in unique(df$parsetting)) {
    subdata2 <- df[df$parsetting == var,]; names(subdata2)
    laM <- unique(subdata2$mod1.sim_lambda_m)
    muM <- unique(subdata2$mod1.sim_mu_m)
    laS <- unique(subdata2$mod1.sim_lambda_s)
    muS <- unique(subdata2$mod1.sim_mu_s)
    conditioning <- unique(subdata2$mod1.cond)

    pars_string <- paste0(
      laM,
      "-",
      muM,
      "-",
      laS,
      "-",
      muS,
      "-",
      conditioning
    )
    labA <- levels(droplevels(models[1]))
    labB <- levels(droplevels(models[2]))

    #lambda
    pdfname <- paste0("lambdaM_", pars_string)
    grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
    p5 <- ggplot2::ggplot(
      subdata2,
      ggplot2::aes(
        x = mod1.MLE.lambda_m,
        y = mod2.MLE.lambda_m
      )
    ) +
    ggplot2::geom_point(
      color = "firebrick4"
    ) +
    ggplot2::labs(
      x = bquote(lambda[M]^.(labA)),
      y = bquote(lambda[M]^.(labB))
    ) +
    ggplot2::theme_bw(
    ) +
    ggtitle(
      bquote(list(
        "Parameter setting: " ~
        lambda[M]==.(laM),
        mu[M]==.(muM),
        lambda[S]==.(laS),
        mu[S]==.(muS),
        cond==.(conditioning)
      ))
    ) +
    theme(axis.title = element_text(size = 18)); print(p5)
    grDevices::dev.off()

    #mu
    pdfname <- paste0("muM_", pars_string)
    grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
    p6 <- ggplot2::ggplot(
      subdata2,
      ggplot2::aes(
        x = mod1.MLE.mu_m,
        y = mod2.MLE.mu_m
      )
    ) +
    ggplot2::geom_point(
      color = "firebrick4"
    ) +
    ggplot2::labs(
      x = bquote(mu[M]^.(labA)),
      y = bquote(mu[M]^.(labB))
    ) +
    ggplot2::theme_bw(
    ) +
    ggtitle(
      bquote(list(
        "Parameter setting: " ~
        lambda[M]==.(laM),
        mu[M]==.(muM),
        lambda[S]==.(laS),
        mu[S]==.(muS),
        cond==.(conditioning)
      ))
    ) +
    theme(axis.title = element_text(size = 18)); print(p6)
    grDevices::dev.off()

    #lambda - mu
    pdfname <- paste0("lambdaM-muM_", pars_string)
    grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
    p7 <- ggplot2::ggplot(
      subdata2,
      ggplot2::aes(
        x = mod1.MLE.lambda_m - mod1.MLE.mu_m,
        y = mod2.MLE.lambda_m - mod2.MLE.mu_m
      )
    ) +
      ggplot2::geom_point(
        color = "firebrick4"
      ) +
    ggplot2::labs(
      x = bquote(lambda[M]^.(labA) - mu[M]^.(labA)),
      y = bquote(lambda[M]^.(labB) - mu[M]^.(labB))
    ) +
    ggplot2::theme_bw(
    ) +
    ggtitle(
      bquote(list(
        "Parameter setting: " ~
        lambda[M]==.(laM),
        mu[M]==.(muM),
        lambda[S]==.(laS),
        mu[S]==.(muS),
        cond==.(conditioning)
      ))
    ) +
    theme(axis.title = element_text(size = 18)); print(p7)
    grDevices::dev.off()

    #mu/lambda
    #lambda - mu
    pdfname <- paste0("ratiomuMlambdaM_", pars_string)
    grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
    p8 <- ggplot2::ggplot(
      subdata2,
      ggplot2::aes(
        x = mod1.MLE.mu_m/mod1.MLE.lambda_m,
        y = mod2.MLE.mu_m/mod2.MLE.lambda_m
      )
    ) +
      ggplot2::geom_point(
        color = "firebrick4"
      ) +
      ggplot2::labs(
        x = bquote(mu[M]^.(labA)/lambda[M]^.(labA)),
        y = bquote(mu[M]^.(labB)/lambda[M]^.(labB))
    ) +
    ggplot2::theme_bw(
    ) +
    ggtitle(
      bquote(list(
        "Parameter setting: " ~
        lambda[M]==.(laM),
        mu[M]==.(muM),
        lambda[S]==.(laS),
        mu[S]==.(muS),
        cond==.(conditioning)
      ))
    ) +
    theme(axis.title = element_text(size = 18)); print(p8)
    grDevices::dev.off()
  }

}

#' @title Create cloud plots
#' @author Giovanni Laudanno
#' @description Create cloud plots
#' @inheritParams default_params_doc
#' @return cloud plots for all results
showplot_cloud <- function(data) {

  library(ggplot2); library(reshape2); library(scales)

  results_folder <- get_results_folder()
  models <- unique(data$model)
  models <- sort(models)

  m <- 1
  for (m in seq_along(models))
  { # m loop
    model_folder <- paste0(results_folder, "\\", models[m])
    if (!file.exists(model_folder)) {
      dir.create(model_folder)
    }
    path <- paste0(model_folder, "/cloudplots")
    if (!file.exists(path)) {
      dir.create(file.path(path), showWarnings = FALSE)
    }

    data2 <- data[data$model == models[m],]; names(data2)
    data2$parsetting <- interaction(
      data2$sim_lambda_m,
      data2$sim_mu_m,
      data2$sim_lambda_s,
      data2$sim_mu_s,
      data2$cond
    )

    df <- data2
    df$parsetting <- interaction(
      df$sim_lambda_m,
      df$sim_mu_m,
      df$sim_lambda_s,
      df$sim_mu_s,
      df$cond
    )

    for (var in unique(df$parsetting))
    { # parsetting loop
      subdata2 <- df[df$parsetting == var,]; names(subdata2)
      laM <- unique(subdata2$sim_lambda_m)
      muM <- unique(subdata2$sim_mu_m)
      laS <- unique(subdata2$sim_lambda_s)
      muS <- unique(subdata2$sim_mu_s)
      conditioning <- unique(subdata2$cond)

      pars_string <- paste0(laM,"-",muM,"-",laS,"-",muS,"-",conditioning)
      labA <- levels(droplevels(models[m]))

      #lambda vs mu
      pa <- ggplot2::ggplot(
        subdata2,
        ggplot2::aes(
          x = MLE.lambda_m,
          y = MLE.mu_m
        )
      ) +
      ggplot2::geom_point(
        color = "firebrick4"
      ) +
      ggplot2::labs(
        x = bquote(lambda[M]^.(labA)),
        y = bquote(mu[M]^.(labA))
      ) +
      ggplot2::theme_bw(
      ) +
      ggtitle(
        bquote(list(
          "Parameter setting: " ~
            lambda[M]==.(laM),
          mu[M]==.(muM),
          lambda[S]==.(laS),
          mu[S]==.(muS),
          cond==.(conditioning)
        ))
      ); pa#print(pa)

      #log lambda vs log mu
      pb <- ggplot2::ggplot(
        subdata2,
        ggplot2::aes(
          x = MLE.lambda_m,
          y = MLE.mu_m
        )
      ) +
        ggplot2::geom_point(
          color = "firebrick4"
      ) +
      ggplot2::labs(
        x = bquote(lambda[M]^.(labA)),
        y = bquote(mu[M]^.(labA))
      ) +
      ggplot2::theme_bw(
      ) +
      ggtitle(
        bquote(list(
          "Parameter setting: " ~
          lambda[M]==.(laM),
          mu[M]==.(muM),
          lambda[S]==.(laS),
          mu[S]==.(muS),
          cond==.(conditioning)
        ))
      ) +
      scale_x_continuous(trans = log2_trans()
      ) +
      scale_y_continuous(trans = log2_trans()
      ); pb#print(pb)

      #(lambda-mu) vs (mu/lambda)
      pc <- ggplot2::ggplot(
        subdata2,
                            ggplot2::aes(
                              x = (MLE.lambda_m - MLE.mu_m),
                              y = (MLE.mu_m / MLE.lambda_m)
                            )
      ) +
        ggplot2::geom_point(color = "firebrick4"
        ) +
        ggplot2::labs(
          x = bquote(lambda[M]^.(labA) - mu[M]^.(labA)),
          y = bquote(mu[M]^.(labA) / lambda[M]^.(labA))
        ) +
        ggplot2::theme_bw(
        ) +
        ggtitle(
          bquote(list(
            "Parameter setting: " ~
            lambda[M]==.(laM),
            mu[M]==.(muM),
            lambda[S]==.(laS),
            mu[S]==.(muS),
            cond==.(conditioning)
          ))
        ); pc#print(pc)

      #log(lambda-mu) vs log(mu/lambda)
      pd <- ggplot2::ggplot(
        subdata2,
                            ggplot2::aes(
                              x = (MLE.lambda_m - MLE.mu_m),
                              y = (MLE.mu_m / MLE.lambda_m)
                            )
      ) +
        ggplot2::geom_point(
          color = "firebrick4"
        ) +
        ggplot2::labs(
          x = bquote(lambda[M]^.(labA) - mu[M]^.(labA)),
          y = bquote(mu[M]^.(labA) / lambda[M]^.(labA))
        ) +
        ggplot2::theme_bw(
        ) +
        ggtitle(
          bquote(list(
            "Parameter setting: " ~
            lambda[M]==.(laM),
            mu[M]==.(muM),
            lambda[S]==.(laS),
            mu[S]==.(muS),
            cond==.(conditioning)
          ))
        ) +
        scale_x_continuous(trans = log2_trans()
        ) +
        scale_y_continuous(trans = log2_trans()
        ); pd#print(pd)

      #print outputs on pdf
      pdfname <- paste0("lambda_vs_mu", pars_string)
      grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
      print(pa)
      grDevices::dev.off()

      pdfname <- paste0("lambda_vs_mu[logscale]", pars_string)
      grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
      print(pb)
      grDevices::dev.off()

      pdfname <- paste0("lambda-mu_vs_muoverlambda", pars_string)
      grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
      print(pc)
      grDevices::dev.off()

      pdfname <- paste0("lambda-mu_vs_muoverlambda[logscale]", pars_string)
      grDevices::pdf(file = paste0(path, "/", pdfname, ".pdf"));
      print(pd)
      grDevices::dev.off()
    } # parsetting loop
  }# m loop

}

#--- FUNCTION CALLS -----
if (1 == 2) {
  # execute_next_setup(account = "Cyrus", max_sims = 2, download_files = FALSE)
  # upload_cluster_scripts()
  # execute_experiment()
  next_setup_gio <- find_next_setup(account = "Giovanni"); next_setup_gio
  next_setup_cy <- find_next_setup(account = "Cyrus"); next_setup_cy
  check_jobs(account = "Giovanni")
  check_jobs(account = "Cyrus")
  execute_next_setup(account = "Giovanni", partition = "gelifes")
  execute_next_setup(account = "Cyrus", partition = "gelifes")
  download_results()
  data <- subset_experiment_data(read_results())
  available_data <- get_available_data(data)
  # overview <- results_overview(data = subset_experiment_data(read_results())); overview
  overview <- results_overview(subset_experiment_data(read_results())); overview
  overview <- results_overview(get_available_data(subset_experiment_data(read_results()),percentage = 0.5)); overview
  available_overview <- results_overview(get_available_data(data)); available_overview
  showplot_box(data = get_available_data(data)) #boxplots only for data that are after 80% of coverage
  showplot_box(data = subset_experiment_data(data))
  # showplot_box(data = subset_experiment_data(read_results()))
  # showplot_box(data = get_available_data(subset_experiment_data(read_results())))
}
