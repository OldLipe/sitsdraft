#' @description
#' @author
#' TODO: remove the @examples below using your code

# ---- Loading packages ----
#' @example
#' \dontrun{
#'  import(sits)
#'  import(...)
#' }

# ---- Setting seed ----
#' @example
#' \dontrun{
#'  set.seed(...)
#' }

# ---- Setting work dir ----
#' @example
#' \dontrun{
#'  setwd(...)
#' }

# ---- Creating sits cube ----
#' @example
#' \dontrun{
#'  sits_cube <- sits::sits_cube(...)
#' }

# ---- Extracting time series ----
#' @example
#' \dontrun{
#'  ts <- sits::sits_get_data(...)
#'
#'  # we strongly recommend to save the extracted time series
#'  saveRDS(ts, "./data/derived_data/samples/time_series.rds")
#' }

# ---- Kfold validation ----
#' @example
#' \dontrun{
#'  conf_matrix.mx <- sits::sits_kfold_validate(...)
#'
#'  evaluate_model <- sits::sits_conf_matrix(conf_matrix.mx)
#' }

# ---- Train model ----
#' @example
#' \dontrun{
#'  model <- sits::train_model(ts, ...)
#'
#'  # we strongly recommend to save the trained model
#'  saveRDS(model, "./data/derived_data/model/mymodel.rds")
#' }

# ---- Classifying maps ----
#' @example
#' \dontrun{
#'  probs_cube <- sits::sits_classify(...)
#'
#'  # we strongly recommend to save the probs cube
#'  saveRDS(probs_cube, "./data/derived_data/classification/probs_cube.rds")
#' }

# ---- Smoothing maps ----
#' @example
#' \dontrun{
#'  smoothed_cube <- sits::sits_smooth(...)
#'
#'  # we strongly recommend to save the smoothed cube
#'  saveRDS(smoothed_cube, "./data/derived_data/classification/smoothed_cube.rds")
#' }

# ---- Final labeled map ----
#' @example
#' \dontrun{
#'  maps <- sits::sits_label_classification(...)
#'
#'  # we strongly recommend to save the smoothed cube
#'  saveRDS(maps, "./data/derived_data/classification/final_labeled_map.rds")
#' }
