# ---- run script ----

proj_dir <- "~/public/rolf/cerrado_v4"
samples_file <- "~/samples/cerrado/samples_cerrado_v2.rds"
make_proj_dir(proj_dir = proj_dir, samples_file = samples_file, force = FALSE)

# # CHECK for BDC_ACCESS_TOKEN environment variable
# Sys.setenv(BDC_ACCESS_KEY = <BDC_ACCESS_KEY>)

lc8_cube <- sits_cube(source      = "BDC",
                      url         = "http://datacube-005.dpi.inpe.br:8010/stac/",
                      name        = "cerrado",
                      bands       = c("BAND1", "BAND2", "BAND3", "BAND4",
                                      "BAND5", "BAND6", "BAND7", "EVI",
                                      "NDVI", "FMASK4"),
                      collection  = "LC8_30_16D_STK-1",
                      tiles       = c("042050", "044049", "044046"),
                      start_date  = "2017-09-01",
                      end_date    = "2018-08-31")

# ---- extract time series of samples ----

do_get_ts(proj_dir = proj_dir, cube = lc8_cube, num_splits = 100,
          multicores = 20, force = FALSE)

# ---- tunning model ----

# do_tunning(proj_dir = proj_dir, ml_method = sits::sits_rfor(), k_folds = 10,
#            force = FALSE)
# do_tunning(proj_dir = proj_dir, ml_method = sits::sits_svm(), k_folds = 10,
#            force = FALSE)

# ---- train model ----
do_train(proj_dir = proj_dir, ml_method = sits::sits_rfor(num_trees = 200),
         force = FALSE)


# ---- classification ----

#
# library(randomForest)
cerrado <- do_classify(proj_dir = proj_dir, cube = lc8_cube,
                       memsize = 40, multicores = 20)

#
# bayesian smooth

#
# generate maps

plot(probs[1,])
