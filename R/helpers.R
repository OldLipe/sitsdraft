#' @title Format path
#' @param ... a \code{character} vector with paths
#'
#' @return a \code{character} vector with merged paths
path <- function(...) {

    dir <- paste0(unlist(list(...), use.names = FALSE), collapse = "/")

    return(dir)
}

#' @title Create directory
#' @param a \code{character} vector with path
#'
#' @return a \code{character} vector with the path of the created directory
make_dir <- function(...) {

    dir <- path(...)

    if (dir.exists(dir))
        return(dir)

    dir.create(dir, showWarnings = FALSE, recursive = TRUE)
    stopifnot(dir.exists(dir))

    dir
}

#' @title Get the extension name of file
#' @param file a \code{character} with the name of file
#'
#' @return a \code{character} with the extension name of files
extension <- function(file) {
    sub(".*\\.(.*)$", "\\1", file)
}

#' @title Get the path of samples
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of samples
samples_dir <- function(proj_dir) {
    path(proj_dir, "samples")
}

#' @title Get the path of samples in csv
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of samples
samples_file_csv <- function(proj_dir) {
    path(samples_dir(proj_dir), "samples.csv")
}

#' @title Get the path of samples in rds
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of samples
samples_file_rds <- function(proj_dir) {
    path(samples_dir(proj_dir), "samples.rds")
}

#' @title Get the path of samples that will be spitted
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of samples
splits_dir <- function(proj_dir) {
    path(samples_dir(proj_dir), "splits")
}

#' @title Get the path of samples that will be spitted in csv
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of samples
splits_file_csv <- function(proj_dir) {
    list.files(path = splits_dir(proj_dir = proj_dir),
               pattern = "\\.csv$", full.names = TRUE)
}

#' @title Get the path of samples that will be spitted in rds
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of samples
splits_file_rds <- function(proj_dir) {
    list.files(path = splits_dir(proj_dir = proj_dir),
               pattern = "\\.rds$", full.names = TRUE)
}

#' @title Get the model directory
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of model
model_dir <- function(proj_dir) {
    path(proj_dir, "model")
}

#' @title Get the model directory in rds
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of model
model_file_rds <- function(proj_dir) {
    path(model_dir(proj_dir = proj_dir), "model.rds")
}

#' @title Get the probability images directory
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of probability images
probs_dir <- function(proj_dir) {
    path(proj_dir, "probs")
}

#' @title Get the probability images directory in rds
#' @param proj_dir a \code{character} with project directory
#'
#' @return a \code{character} with path of probability images
probs_file_rds <- function(proj_dir) {
    path(probs_dir(proj_dir = proj_dir), "probs.rds")
}

#' @title Creates project directory
#'
#' @param proj_dir a \code{character} with project directory
#' @param samples_file a \code{character} with samples directory
#' @param force a \code{logical} if TRUE the samples will be overwritten
#'
#' @return an invisible NULL
#' @export
make_proj_dir <- function(proj_dir, samples_file, force = FALSE) {

    stopifnot(extension(samples_file) %in% c("csv", "rds"))
    stopifnot(file.exists(samples_file))

    make_dir(proj_dir)

    make_dir(samples_dir(proj_dir))

    if (!force && file.exists(samples_file_csv(proj_dir))) {
        message("Samples already exists. Use 'force=TRUE' to overwrite them.")
        return(invisible(NULL))
    }

    if (extension(samples_file) == "rds") {
        samples <- readRDS(samples_file)
        sits::sits_metadata_to_csv(samples, file = samples_file_csv(proj_dir))
        file.copy(from = samples_file, to = samples_file_rds(proj_dir))
    } else {
        file.copy(from = samples_file, to = samples_file_csv(proj_dir))
    }

    invisible(NULL)
}

#' @title Get time series from csv file
#'
#' @param csv_file \code{character} with csv samples directory
#' @param cube a \code{sits_cube} object
#' @param force a \code{logical} if TRUE the samples will be overwritten
#'
#' @return a \code{tibble} with time series extracted
get_ts_from_csv <- function(csv_file, cube, force = FALSE) {

    rds_file <- paste0(sub("^(.*)\\.csv$", "\\1.rds", csv_file))

    if (!force && file.exists(rds_file)) {
        return(rds_file)
    }

    samples <- sits::sits_get_data(cube = cube, file = csv_file)

    saveRDS(samples, file = rds_file)

    rds_file
}

#' @title Split samples
#' @param proj_dir a \code{character} with project directory
#' @param num_splits a \code{numeric} with number of partitions will be splitted
#'
#' @return an invisible NULL
do_split <- function(proj_dir, num_splits = 10) {

    stopifnot(file.exists(samples_file_csv(proj_dir = proj_dir)))

    samples <- read.csv(samples_file_csv(proj_dir = proj_dir))

    stopifnot(all(c("latitude", "longitude") %in% names(samples)))

    # manage to improve performance
    samples <- dplyr::arrange(samples, longitude)
    samples <- dplyr::arrange(samples, latitude)

    if (num_splits > nrow(samples))
        num_splits <- ceiling(nrow(samples) / 10)

    # split csv files
    index <- cut(seq_len(nrow(samples)), num_splits, labels = FALSE)

    for (i in seq_len(num_splits)) {
        csv_file <- tempfile(tmpdir = splits_dir(proj_dir = proj_dir),
                             fileext = ".csv")
        write.csv(samples[index == i, ], file = csv_file, row.names = FALSE)
    }

    invisible(NULL)
}

#' @title Get time series
#' @param proj_dir a \code{character} with project directory
#' @param cube a \code{sits_cube} object
#' @param csv_files a \code{character} with csv samples directory
#' @param num_splits a \code{numeric} with number of partitions will be splitted
#' @param multicores a \code{numeric} with number of cores will be used
#' @param force a \code{logical} if TRUE the samples will be overwritten
#'
#' @return an invisible NULL
#' @export
do_get_ts <- function(proj_dir, cube, csv_files, num_splits = 10, multicores = 20,
                      force = FALSE) {

    stopifnot(dir.exists(proj_dir))
    stopifnot(inherits(cube, "raster_cube"))
    stopifnot(num_splits > 0)
    stopifnot(multicores > 0)

    if (!force && file.exists(samples_file_rds(proj_dir = proj_dir))) {
        message(paste("Time series already extracted. Use 'force=TRUE'",
                      "to extract again."))
        return(invisible(NULL))
    }

    make_dir(splits_dir(proj_dir = proj_dir))

    if (force || length(csv_files) == 0) {

        unlink(splits_file_csv(proj_dir = proj_dir))
        unlink(splits_file_rds(proj_dir = proj_dir))
        do_split(proj_dir = proj_dir, num_splits = num_splits)
    }

    # retrieve time series in parallel
    plan <- future::plan("multisession", workers = multicores)
    on.exit(future::plan(plan))

    files_rds <- furrr::future_map(csv_files, get_ts_from_csv,
                                   cube = cube, force = force,
                                   .progress = length(csv_files) >= 3)

    samples <- dplyr::bind_rows(lapply(files_rds, readRDS))
    saveRDS(samples, file = samples_file_rds(proj_dir))

    invisible(NULL)
}

#' @title Train model
#' @param proj_dir a \code{character} with project directory
#' @param ml_method a \code{sits_model} object
#' @param force a \code{logical} if TRUE the samples will be overwritten
#'
#' @return an invisible NULL
#' @export
do_train <- function(proj_dir, ml_method, force = FALSE) {

    stopifnot(dir.exists(proj_dir))
    stopifnot(is.function(ml_method))
    stopifnot(file.exists(samples_file_rds(proj_dir = proj_dir)))

    make_dir(model_dir(proj_dir = proj_dir))

    if (!force && file.exists(model_file_rds(proj_dir = proj_dir))) {
        message("Model already trained. Use 'force=TRUE' to overwrite it.")
        return(invisible(NULL))
    }

    samples <- readRDS(samples_file_rds(proj_dir = proj_dir))

    model <- sits::sits_train(samples, ml_method = ml_method)

    saveRDS(model, file = model_file_rds(proj_dir = proj_dir))

    invisible(NULL)
}

#' @title Fix name of cubes
#' @param cube a \code{sits_cube} object
#'
#' @return a \code{sits_cube} object with fixed name
fix_cube_names_uniqueness <- function(cube) {
    if (length(cube$name) != length(unique(cube$name))) {
        if (all(!is.na(cube$tile))) {
            cube$name <- paste0(cube$name, "_", cube$tile)
        } else {
            cube$name <- paste0(cube$name, "_",
                                formatC(seq_len(nrow(cube)),
                                        flag = 0,
                                        width = log(nrow(cube), 10) + 1))
        }
    }
    return(cube)
}

#' @title Create a name for probs cube files
#'
#' @param proj_dir a \code{character} with project directory
#' @param cube a \code{sits_cube} object
#' @param version a \code{character} with version
#'
#' @return a \code{character} of cube probs files
cube_probs_files <- function(proj_dir, cube, version) {

    # set the name of the output cube
    name <- paste0(cube$name, "_probs")

    # get start and end dates
    timeline <- as.Date(sits::sits_timeline(cube))
    start_date <- timeline[[1]]
    end_date <- timeline[[length(timeline)]]

    # define the file name for the classified images
    file_name <- paste0(probs_dir(proj_dir = proj_dir), "/",
                        paste0(name, "_", start_date, "_", end_date, "_",
                               version, ".rds"))

    file_name
}

#' @title
#'
#' @param proj_dir a \code{character} with project directory
#' @param cube a \code{sits_cube} object
#' @param roi a \code{list} with interested region
#' @param version a \code{character} with version
#' @param memsize a \code{numeric} with the memory size that will be used
#' @param multicores a \code{numeric} with number of cores will be used
#' @param force a \code{logical} if TRUE the samples will be overwritten
#'
#' @return an invisible NULL
#' @export
do_classify <- function(proj_dir, cube, roi = NULL, version = "v1",
                        memsize = 20, multicores = 20, force = FALSE) {

    stopifnot(dir.exists(proj_dir))
    stopifnot(inherits(cube, "raster_cube"))
    stopifnot(file.exists(model_file_rds(proj_dir = proj_dir)))

    make_dir(probs_dir(proj_dir = proj_dir))

    model <- readRDS(file = model_file_rds(proj_dir = proj_dir))

    # prepare cube name uniqueness
    cube <- fix_cube_names_uniqueness(cube = cube)

    # get cube probabilities rds files
    files_rds <- cube_probs_files(proj_dir = proj_dir, cube = cube,
                                  version = version)

    # log
    Sys.setenv("__SITS_DEBUG__" = TRUE)

    # support to sits resume glitch
    Sys.setenv("__SITS_RESUME__" = TRUE)

    # implements tile resuming
    for (i in seq_len(nrow(cube))) {

        # process each tile
        tile <- cube[i, ]

        message(paste0("Processing tile ", tile$name,
                       " (", i, "/", nrow(cube), ")"))


        if (!force && file.exists(files_rds[[i]])) {

            message("Tile already classified. Use 'force=TRUE' to classify again.")
            next
        }

        # show classification time
        time_start <- Sys.time()
        message(paste("Starting classification at", time_start))

        # classify
        probs <- sits::sits_classify(data = tile, ml_model = model, roi = roi,
                                     memsize = memsize, multicores = multicores,
                                     output_dir = probs_dir(proj_dir = proj_dir),
                                     version = version)
        saveRDS(probs, file = files_rds[[i]])

        # finish classification
        time_end <- Sys.time()
        message(paste("Classification finished at", time_end))
        message(paste("Elapsed time", time_end - time_start))

    }

    probs <- do.call(rbind, args = lapply(files_rds, readRDS))
    saveRDS(probs, file = probs_file_rds(proj_dir))

    invisible(NULL)
}
