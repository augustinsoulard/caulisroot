#' @title scrap_data_inat
#' @description Récupère les données Inaturalist et les injectes dans la BDD
#' @param requete Requête SQL qui selection la zone d'étude
#' @param year Année de selection dans Inaturalist
#' @param maxresult Résultats maximum pour la requêtes INaturalist
#' @param filter_user_login nom d'utilisateur si filtrer par utilisateur
#' @return Pas le plus important, mais returne les tableaux injectés dans postgreSQL
#' @examples scrap_data_inat(requete="SELECT * FROM projet.zone_etude WHERE code IN (27);",year=NULL,maxresult=900,filter_user_login = "augustinsoulard")

#' @export
####Function
scrap_data_inat = function(requete=NULL,
                           year=NULL,
                           maxresult=10000,
                           filter_user_login = NULL){

  # Chargement des packages ####
  if(!require("sf")){install.packages("sf")} ; library("sf")
  if(!require("leaflet")){install.packages("leaflet")} ; library("leaflet")
  if(!require("rinat")){install.packages("rinat")} ; library("rinat")
  if(!require("rtaxref")){install_github("Rekyt/rtaxref")} ; library("rtaxref")
  source("function/taxabase.R")
  source("function/postgres/postgres_manip.R")

  con = copo()
  if(!is.null(requete)){
    # Lecture des observation via INaturalist ou un fichier téléchargé ####
    obs = inat_from_polygon(requete = requete,
                            year = year,
                            maxresult = maxresult)
  }else{
    obs = read.csv(choose.files()) #Pour choisir un fichier csv téléchargé depuis INaturalist

  }

  # Nettoyage des données
  obs = obs[grepl(" ", obs$scientific_name)  & !is.na(obs$scientific_name) & obs$iconic_taxon_name == 'Plantae',]

  # Filtrer par user si demandé
  if(!is.null(filter_user_login)){
    obs = obs %>% filter(user_login==filter_user_login)
  }
  # Tester de voir si la base de données connait les codes taxon INaturalist ####

  corresp_know = dbGetQuery(con, "SELECT * FROM inaturalist.corresp_taxref")

  # A REMPLIRIIR

  obs_unknow = obs %>% filter(!obs$taxon_id %in% corresp_know$code_taxa_entree)

  #Enregistrement des nouvelles correspondances
  if(nrow(obs_unknow)>0){
    unknow_taxa = unique(obs_unknow[, c("scientific_name", "taxon_id")])

    # find_taxaref ####
    new_corresp = find_taxaref(lb_taxa_entree = unknow_taxa$scientific_name,
                               code_taxa_entree = unknow_taxa$taxon_id,
                               ref ='taxref',
                               input_ref="inaturalist")
    write_to_schema(con, "inaturalist", "corresp_taxref", new_corresp, append = TRUE)
    cat("De nouvelles correspondances existent vérifier manuellement dans BiodiversitySQL")
    return(new_corresp)
    stop("De nouvelles correspondances existent vérifier manuellement dans BiodiversitySQL")

  }

  inat_augustinsoulard = dbGetQuery(con, "SELECT * FROM inaturalist.augustinsoulard")
  obs_to_add <- obs %>%
    filter(!id %in% inat_augustinsoulard$id)
  if(nrow(obs_to_add)>0){
    write_to_schema(con, "inaturalist", "augustinsoulard", obs_to_add, append = TRUE)
    cat("Données Inaturalist sauvegardée sur BiodiversitySQL")
  }

  obs = left_join(obs,corresp_know,by=c("taxon_id"="code_taxa_entree"))
  obs = obs %>% select(-lb_nom)
  #Ajout des bon lb_nom
  taxref = dbGetQuery(con, "SELECT lb_nom,cd_ref FROM public.taxrefv18_fr_plantae_ref")
  obs = left_join(obs,taxref,by="cd_ref")
  # Étape 1 : renommer les colonnes
  obs_renamed <- obs %>%
    rename(
      Nom = lb_nom,
      Date = datetime,
      latitude = latitude,
      longitude = longitude,
      Commentaire = url,
      Observateur = user_name
    )

  # Étape 2 : création objet sf avec coordonnées WGS84
  points_wgs84 <- st_as_sf(obs_renamed, coords = c("longitude", "latitude"), crs = 4326)

  # Étape 3 : reprojection vers Lambert 93 (EPSG:2154)
  points_l93 <- st_transform(points_wgs84, crs = 2154)

  # Étape 4 : extraction des coordonnées X/Y
  coords <- st_coordinates(points_l93)
  points_l93$X <- coords[, 1]
  points_l93$Y <- coords[, 2]

  # Étape 5 : réorganiser les colonnes comme demandé
  final_table <- points_l93 %>%
    st_drop_geometry() %>%
    select(Nom, Date, X, Y, Commentaire, Observateur)


  st_write(points_l93, dsn = con, layer = c("inaturalist.inat_work_data"), delete_layer = TRUE)

  # Reprise depuis l'objet sf avec géométrie
  points_l93$geom <- st_as_text(st_geometry(points_l93))  # WKT
  df_no_geom <- points_l93 %>% st_drop_geometry()
  DBI::dbWriteTable(con, SQL("inaturalist.inat_work_data"), df_no_geom, overwrite = TRUE)
  # Étape 1 : ajouter la colonne geometry
  DBI::dbExecute(con, "
  ALTER TABLE inaturalist.inat_work_data
  ADD COLUMN geometry geometry(Point, 2154);
")

  # Étape 2 : convertir la colonne 'geom' (WKT) en géométrie PostGIS
  DBI::dbExecute(con, "
  UPDATE inaturalist.inat_work_data
  SET geometry = ST_GeomFromText(geom, 2154);
")

  # Étape 3 : supprimer la colonne WKT temporaire
  DBI::dbExecute(con, "
  ALTER TABLE inaturalist.inat_work_data
  DROP COLUMN geom;
")


  return = dbGetQuery(con, "SELECT * FROM inaturalist.inat_work_data")
  return(return)

}
