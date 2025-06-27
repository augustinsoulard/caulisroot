#' @title charger_gpkg
#' @description Fonction permettant de charger rapidement les couches contenu dans un geopackage
#' @param layers Liste de layer du geopackage
#' @param path chemin vers le fichier (choix de fichier si pas de chemin)
#' @return Un ou plusieurs objets `sf` nommés comme les couches importées
#' @examples charger_gpkg(layers = c('Flore','Releve_Phyto'))
#' @export

charger_gpkg <- function(layers = NULL,path=NULL) {
  if(!require("sf")){install.packages("sf")} ; library("sf")
  if(!require("tcltk")){install.packages("tcltk")} ; library("tcltk")

  # Choix du fichier GeoPackage
  if(is.null(path)){
    fichier <- tk_choose.files(
      caption = "Choisir un fichier GeoPackage",
      filter = matrix(c("GeoPackage", "*.gpkg"), ncol = 2, byrow = TRUE)
    )
  } else{
    fichier = path
  }

  if (length(fichier) == 0 || fichier == "") {
    message("Aucun fichier sélectionné.")
    return(NULL)
  }

  # Liste des couches disponibles
  couches_dispo <- st_layers(fichier)$name

  # Si aucune couche spécifiée, on les charge toutes
  if (is.null(layers)) {
    layers <- couches_dispo
  }

  # Vérification des couches demandées
  couches_incorrectes <- setdiff(layers, couches_dispo)
  if (length(couches_incorrectes) > 0) {
    warning("Couches non trouvées dans le fichier : ", paste(couches_incorrectes, collapse = ", "))
    layers <- intersect(layers, couches_dispo)
  }

  # Chargement et assignation dans l'environnement global
  resultats <- list()
  for (nom_couche in layers) {
    donnees <- st_read(fichier, layer = nom_couche, quiet = TRUE)
    assign(nom_couche, donnees, envir = .GlobalEnv)
    resultats[[nom_couche]] <- donnees
    message(paste("✓ Couche chargée :", nom_couche))
  }

  invisible(resultats)
}

