#' @title updatetaxa
#' @description
#' Passe une liste de codes CD_NOM (identifiants TAXREF) vers leur code CD_REF correspondant,
#' en interrogeant la table taxref_flore_fr_syn (PostgreSQL).
#'
#' @param liste_cd_nom Vector (ou data.frame convertible en vector). Liste des codes CD_NOM à convertir.
#' @param taxagroup Character. Sourcer des noms d'entrée ("flore", "faune" ou fonge), utilisé pour ajuster la correspondance au bon groupe taxonomique (défaut : "flore").
#'
#' @return Vector. Liste des codes CD_REF correspondants (NA si aucun match trouvé).
#'
#' @details
#' Interroge la table `public.taxref_flore_fr_syn` et retourne, pour chaque CD_NOM fourni,
#' le CD_REF associé. La connexion à la base de données PostgreSQL (objet `con`)
#' doit être disponible dans l'environnement global.
#'
#' @examples
#'updatetaxa(data$cd_nom,taxagroup = "flore")
#' @export

updatetaxa = function(liste_cd_nom,
                      taxagroup = "flore"){
  library(dplyr)
  con = copo()
  if(taxagroup == "flore"){
    taxref = dbGetQuery(con, "SELECT * FROM public.taxref_flore_fr_syn")
  } else if(taxagroup == "faune"){
    taxref =dbGetQuery(con, "SELECT * FROM public.taxrefv18 WHERE regne = 'Animalia'")
  }
  taxref$cd_nom = as.character(taxref$cd_nom)
  liste_cd_nom = data.frame("cd_nom" = as.character(liste_cd_nom))
  liste_cd_nom_join = left_join(liste_cd_nom,taxref,by="cd_nom") %>% dplyr::select(cd_ref)
  CD_NOM_actuel = liste_cd_nom_join$cd_ref
  dbDisconnect(con)
  return(CD_NOM_actuel)
}
