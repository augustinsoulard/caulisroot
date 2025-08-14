

# A TRAITER ####

findtaxa <- function(listetaxa,
                     data = NULL,
                     referenciel = "taxref",
                     actualisation_cd_nom = TRUE,
                     cd_nom_type="character") {

  # Vérifier que listetaxa est un nom de colonne unique
  if (!is.character(listetaxa) || length(listetaxa) != 1) {
    stop("'listetaxa' doit être un nom de colonne (une seule chaîne de caractères).")
  }

  # Charger le référentiel
  if (referenciel == "taxref") {
    taxref <- dbGetQuery(con, "SELECT * FROM public.taxref_flore_fr_syn")
  } else {
    stop("Seul le référentiel 'taxref' est pris en charge.")
  }

  # Déterminer si l'entrée est un vecteur ou une colonne dans un data.frame fourni
  if (is.null(data)) {
    # Cas d'un vecteur simple
    data <- data.frame(lb_nom = listetaxa, stringsAsFactors = FALSE)
    input_type <- "vector"
  } else {
    # Cas d'un data.frame
    if (!is.data.frame(data)) {
      stop("'data' doit être un data.frame.")
    }
    if (!(listetaxa %in% names(data))) {
      stop(paste0("La colonne '", listetaxa, "' est absente du data.frame fourni."))
    }
    data <- dplyr::rename(data, lb_nom = !!listetaxa)
    input_type <- "dataframe"
  }

  # Jointure
  listetaxa_join <- dplyr::left_join(data, taxref, by = "lb_nom", relationship = "many-to-many")

  # Actualisation cd_nom si demandé
  if (actualisation_cd_nom) {
    listetaxa_join$cd_nom <- updatetaxa(listetaxa_join$cd_nom)
  }

  #choix du type
  listetaxa_join$cd_nom <- switch(cd_nom_type,
                                  character = as.character(listetaxa_join$cd_nom),
                                  integer   = as.integer(listetaxa_join$cd_nom))

  # Résultat selon type d'entrée
  if (input_type == "vector") {
    return(listetaxa_join$cd_nom)
  } else {
    return(listetaxa_join)
  }
}
