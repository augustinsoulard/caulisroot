#' @title updatetaxa
#' @description
#' Passe une liste de codes CD_NOM (identifiants TAXREF) vers leur code CD_REF correspondant,
#' en interrogeant la table taxref_flore_fr_syn (PostgreSQL).
#'
#' @param liste_cd_nom Vector (ou data.frame convertible en vector). Liste des codes CD_NOM à convertir.
#'
#' @return Vector. Liste des codes CD_REF correspondants (NA si aucun match trouvé).
#'
#' @details
#' Interroge la table `public.taxref_flore_fr_syn` et retourne, pour chaque CD_NOM fourni,
#' le CD_REF associé. La connexion à la base de données PostgreSQL (objet `con`)
#' doit être disponible dans l'environnement global.
#'
#' @examples
#'updatetaxa(data$cd_nom)
#' @export

updatetaxa = function(liste_cd_nom){
  con = copo()
  TAXREFv17_FLORE_FR_SYN = dbGetQuery(con, "SELECT * FROM public.taxref_flore_fr_syn")
  TAXREFv17_FLORE_FR_SYN$cd_nom = as.character(TAXREFv17_FLORE_FR_SYN$cd_nom)
  liste_cd_nom = data.frame("cd_nom" = as.character(liste_cd_nom))
  liste_cd_nom_join = left_join(liste_cd_nom,TAXREFv17_FLORE_FR_SYN,by="cd_nom") %>% dplyr::select(cd_ref)
  CD_NOM_actuel = liste_cd_nom_join$cd_ref
  return(CD_NOM_actuel)
}
