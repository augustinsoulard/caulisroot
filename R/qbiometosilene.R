#' @title Convertir des données QBiome au standard SILENE/SINP
#' @description
#' Transforme une couche d'observations (GeoPackage QBiome) vers un tableau
#' conforme au standard **Silene/SINP**, en complétant automatiquement
#' les champs (UUID, structure, programme, origine, diffusion, commune/INSEE,
#' coordonnées L93, altitude, date, observateurs, espèce, effectifs, remarques).
#'
#' @param donnees_path Chemin vers le fichier de données spatiales (ex. GeoPackage)
#'   contenant au minimum les couches `Flore` (obligatoire) et `Typologie` (facultative).
#'   Par défaut `file.choose()`.
#' @param standard_silene \code{data.frame} modèle du standard Silene/SINP
#'   (toutes les colonnes attendues). Par défaut, récupéré depuis PostgreSQL via
#'   \code{dbGetQuery(con, "SELECT * FROM donnees.standard_donnees_silene")}.
#' @param structure Nom de la structure productrice (champ \emph{Structure productrice}).
#'   Défaut : \code{"BIODIV"}.
#' @param origine Origine de la donnée (\code{"Pr"} ou \code{"Pu"}).
#'   Défaut : \code{"Pu"}.
#' @param diffusion Niveau de diffusion si \code{origine == "Pr"} (entier 1–5).
#'   Défaut : \code{5}.
#' @param type_loc Précision du type de localisation (ex. \code{"précis"}).
#'   Actuellement non utilisé dans le corps de la fonction (réservé pour
#'   une future version).
#' @param communes Table des communes (codes INSEE, nom, géométrie) servant à la
#'   jointure spatiale. Par défaut, récupérée depuis PostgreSQL via
#'   \code{dbGetQuery(con, "SELECT * FROM limiteadmin.communes_paca")}.
#'
#' @return Un \code{data.frame} (ou \code{tibble}) conforme au standard Silene/SINP,
#'   avec une ligne par observation de la couche \code{Flore}.
#'
#' @details
#' \strong{Entrées attendues :}
#' \itemize{
#'   \item La couche \code{Flore} doit contenir (au minimum) les champs
#'     \code{uuid}, \code{Projet}, \code{Date}, \code{Observateur}, \code{Nom},
#'     \code{Nombre}, \code{Precision} et une géométrie ponctuelle.
#'   \item La table \code{communes} doit posséder une colonne géométrique
#'     \code{geom} (EWKB) et les champs \code{nom_officiel}, \code{code_insee},
#'     \code{code_insee_du_departement}.
#' }
#'
#' \strong{Système de coordonnées :}
#' Les coordonnées produites sont renseignées dans les champs \emph{Coord X L93} /
#' \emph{Coord Y L93} et supposent que la couche \code{Flore} est en
#' \strong{Lambert-93 (EPSG:2154)}. Si ce n'est pas le cas, transformer en amont,
#' par exemple :
#' \preformatted{
#'   data_qbiome <- sf::st_transform(data_qbiome, 2154)
#' }
#'
#' \strong{Hypothèses et mise en garde :}
#' \itemize{
#'   \item La géométrie est \emph{ponctuelle}; \code{sf::st_coordinates()} extrait
#'     (X, Y[, Z]). Si \code{Z} (altitude) est absent, la colonne \emph{Altitude}
#'     sera \code{NA}.
#'   \item Les observateurs sont séparés par virgule et espace \code{", "}.
#'     Adapter le séparateur si nécessaire.
#'   \item Le champ \emph{Diffusion donnée si Origine = Pr} est rempli quel que soit
#'     \code{origine}; il appartient au flux aval de n’en tenir compte que si \code{origine == "Pr"}.
#' }
#'
#' @examples
#' \donttest{
#' # 1) Connexion (exemple) puis récupération des gabarits depuis PostgreSQL
#' # con <- copo()  # fonction de connexion maison
#' # standard <- DBI::dbGetQuery(con, "SELECT * FROM donnees.standard_donnees_silene")
#' # communes  <- DBI::dbGetQuery(con, "SELECT * FROM limiteadmin.communes_paca")
#' #
#' # 2) Conversion depuis un GeoPackage QBiome
#' # out <- qbiometosilene(
#' #   donnees_path   = "data/qbiome.gpkg",
#' #   standard_silene = standard,
#' #   structure       = "CBNMed",
#' #   origine         = "Pu",
#' #   diffusion       = 5,
#' #   type_loc        = "précis",
#' #   communes        = communes
#' # )
#' #
#' # head(out)
#' }
#'
#' @seealso \code{\link{copo}} pour la connexion PostgreSQL.
#'
#' @author Augustin (package QBiome)
#' @keywords data-import GIS SINP Silene
#'
#' @import sf
#' @import dplyr
#' @import tidyr
#' @importFrom DBI dbGetQuery
#'
#' @encoding UTF-8
#' @export




qbiometosilene = function(donnees_path = file.choose(),
                          standard_silene = dbGetQuery(con, "SELECT * FROM donnees.standard_donnees_silene"),
                          structure = "BIODIV",
                          origine = "Pu", # Pr ou Pu
                          diffusion = 5, # 1 un5 selon ouverture des données
                          type_loc = "précis",
                          communes = dbGetQuery(con, "SELECT * FROM limiteadmin.communes_paca")
                          ){

  library(tidyr)
  library(dplyr)
  library(sf)

  ##Chargement des données
  data_qbiome = st_read(donnees_path, layer = "Flore")
  typologie = st_read(donnees_path, layer = "Typologie") # A paramétrer

  ## Préparation du tableau ####
  standard_silene = standard_silene[rep(1, nrow(data_qbiome)),]

  ## uuid ####
  standard_silene$`UUID Sinp` = data_qbiome$uuid

   ## Structure productrice ####
  standard_silene$`Structure productrice `= structure

  ## Programme d'aquisition  ####
  standard_silene$`Programme d'acquisition` = data_qbiome$Projet

  ## Origine
  standard_silene$`Origine donnée` = origine

  ## Diffusion
  standard_silene$`Diffusion donnée si Origine = Pr` = diffusion

  ## Commune code insee et département ####
  # Jointure spatiale
  communes$geom = sf::st_as_sfc(communes$geom, EWKB = TRUE)
  communes_sf <- sf::st_sf(communes, sf_column_name = "geom")

  data_join_communes <- st_join(data_qbiome, communes_sf, join = st_intersects)
  standard_silene$Commune = data_join_communes$nom_officiel
  standard_silene$`Code INSEE` =  data_join_communes$code_insee
  standard_silene$Département =  data_join_communes$code_insee_du_departement

  ## Affectation des coordonnées  ####
  # Extraire les coordonnées sous forme de data.frame
  coords <- st_coordinates(data_qbiome)
  # ajout dans le standard
  standard_silene$`Coord X L93` =  coords[,1]
  standard_silene$`Coord Y L93`= coords[,2]
  standard_silene$Altitude = coords[,3]

## Date format jj/mm/yyyy ####
  standard_silene$`Date min` =  format(data_qbiome$Date, format = "%d/%m/%Y")

  ## Observateurs ####

  # Séparer la colonne en trois colonnes distinctes

  # 1. Trouver le nombre maximal d'observateurs dans la colonne
  max_observateurs <- max(lengths(strsplit(data_qbiome$Observateur, ", ")))

  # 2. Créer une liste de noms de colonnes dynamiques
  noms_colonnes <- paste0("observateur_", 1:max_observateurs)

  # 3. Séparer la colonne en autant de colonnes que nécessaire
  data_qbiome_sep <- data_qbiome %>%
    separate(
      Observateur,
      into = noms_colonnes,
      sep = ", ",
      fill = "right"
    )

  # 4. Ajouter ces colonnes à standard_silene
  for (i in 1:max_observateurs) {
    standard_silene[[paste0("Observateur ", i)]] <- data_qbiome_sep[[paste0("observateur_", i)]]
  }

    ## Nom d'espèce ####
  standard_silene$Espèce = data_qbiome$Nom

  ## Nombre ####
  standard_silene$`Effectif min` = data_qbiome$Nombre

  ## Précision Nombre ####
  standard_silene$`Précision effectif` = data_qbiome$Precision


## Remarques ####
  standard_silene$Remarque = data_qbiome$Commentaire

#à paramétrer avec TYPOLOGIE####
## Habitat ####
  # standard_silene$Habitat = data_qbiome$Habitat

return(standard_silene)

}
