#' @title Creates a template package based on reproducible research to put sits
#'  analysis.
#' @name sitsdraft
#' @description This functions uses as basis the package \code{rrtools} to
#'  create a template package that follows a base structure of reproducible
#'  researcher package.
#'
#' @param pkgname  a \code{character} with the path and package name
#' @param template a \code{character} with the path of the template file
#' @param filename a \code{character} with the name of template file
#' @param rstudio  verify if the user uses rstudio, if so, a new project
#'  file will be created.
#' @param open     a \code{boolean} to open a new session after creating the
#'  template. By default is TRUE.
#' @param simple   a \code{boolean} to create an R directory in template
#'  package. if FALSE the R directory will be created. By default is FALSE.
#'
#' @return After creating the template, if the user are in rstudio, the session
#' will be redirect to the created template.
#' @export
sitsdraft <- function(pkgname = "sitsdraft",
                      template = "template/classification.R",
                      filename = "classification.R",
                      rstudio = TRUE,
                      open = TRUE,
                      simple = FALSE) {


    # verify if the provided directory exists
    if (!dir.exists(pkgname)) {
        dir.create(pkgname)
        message("The directory ", pkgname, " has been created.")
    } else {
        message("Creating the compendium in the current directory: ", pkgname)
    }

    # initialize the new project with useful features
    if (rstudio & open) {

        fileConn <- file(file.path(pkgname, ".Rprofile"))
        writeLines(
            c(
                # run additional commands
                "message('This template was created based on rrtools package, please visit their github to add more options:')",
                "message('https://github.com/benmarwick/rrtools')"
            ),
            fileConn
        )
        close(fileConn)

        # create new project
        rrtools::use_compendium(
            pkgname,
            rstudio = rstudio,
            open = open,
            simple = simple,
            welcome_message = FALSE
        )

    } else {

        # create new project
        rrtools::use_compendium(
            pkgname,
            rstudio = rstudio,
            open = open,
            simple = simple,
            welcome_message = TRUE
        )

        # switch to new dir
        setwd(pkgname)

        # run additional commands
        #usethis::use_mit_license(copyright_holder = get_git_config('user.name', global = TRUE))
        cat('\n')
        rrtools::use_readme_rmd(render_readme = FALSE)
        cat('\n')
        #rrtools::use_analysis(data_in_git = data_in_git)
        cat('\n')

        usethis::ui_done("The working directory is now {getwd()}")

    }

    # create initial template
    edited_file   <- paste0(pkgname, "/R/", filename)
    template_file <- system.file(template, package = "sitsdraft")
    file.copy(template_file, edited_file)

    # dir structure
    dir.create(paste0(pkgname, "/data"))
    dir.create(paste0(pkgname, "/data/raw_data"))
    dir.create(paste0(pkgname, "/data/derived_data"))
    dir.create(paste0(pkgname, "/data/derived_data/samples"))
    dir.create(paste0(pkgname, "/data/derived_data/classification"))
    dir.create(paste0(pkgname, "/data/derived_data/model"))
    dir.create(paste0(pkgname, "/supplementary-materials"))
}
