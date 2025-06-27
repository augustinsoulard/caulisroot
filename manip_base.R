renv::install("devtools")

devtools::install()

usethis::use_git_ignore()# ne pas transmettre les .RData etc... (déjà sur le projet)


# NAMESPACE
roxygen2::roxygenise()

devtools::document()
