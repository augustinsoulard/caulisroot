renv::install("devtools")

usethis::use_git_ignore()# ne pas transmettre les .RData etc... (déjà sur le projet)


# NAMESPACE
roxygen2::roxygenise()

devtools::document()
devtools::install(force = TRUE)  # <-- force la mise à jour même si le numéro de version ne change pas


# usethis::create_package("C:/Users/augus/AppData/Local/R/win-library/4.4/caulisroot")
