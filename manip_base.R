renv::install("devtools")

usethis::use_git_ignore()# ne pas transmettre les .RData etc... (déjà sur le projet)


# NAMESPACE
roxygen2::roxygenise()


# (Re)génère la doc et installe ton package ####
devtools::document()
devtools::install(dependencies = TRUE, force = TRUE)
# puis si tu veux tester
devtools::load_all()


# usethis::create_package("C:/Users/augus/AppData/Local/R/win-library/4.4/caulisroot")





# Méthode problème packages  ####
renv::activate()

# Installe les 4 dépendances + quelques deps usuelles de sf
renv::install(c("rinat", "leaflet"))
Y
# Optionnel : vérifie les libs externes vues par sf
sf::sf_extSoftVersion()

# Fige l’état de l’env
renv::snapshot()
