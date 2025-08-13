#' @title copo
#' @description Fonction permettant de se connecter à la base de données PostgreSQL en utilisant un fichier `.Renviron`.
#' @return Une connexion active à la base PostgreSQL (objet de type `DBIConnection`)
#' @details La fonction suppose que le fichier `.Renviron` contient les variables d’environnement `PGUSER` et `PGPASSWORD`, et que la base `BiodiversitySQL` est accessible.
#' @examples
#' con = copo()
#' BDC_STATUTS <-   dbGetQuery(con, "SELECT * FROM public.bdc_statuts_18")
#' @export


copo = function(){ #COnnexion à la bdd POstgres
  # Préparation de la BDD ####
  ### Déclaraiton des variables d'environnement pour l'authentification ####
  if (file.exists("D:/Logiciel/R/.Renviron")) {
    Sys.setenv(R_ENVIRON = "G:/Mon Drive/Administratif/hz/.Renviron")
    readRenviron(Sys.getenv("R_ENVIRON"))
  } else {
    stop("Le fichier .Renviron n'existe pas ou n'est pas accessible.")
  }
  readRenviron(Sys.getenv("R_ENVIRON"))
  Sys.setenv(PGUSER = Sys.getenv("PGUSER"), PGPASSWORD = Sys.getenv("PGPASSWORD")) # Lancer cette commade avec les bons indentifiant


  ### Chargement des packages ####
  if(!require("RPostgreSQL")){install.packages("RPostgreSQL")} ; library("RPostgreSQL")
  if(!require("RPostgres")){install.packages("RPostgres")} ; library("RPostgres")


  # Charger le driver PostgreSQL
  drv <- dbDriver("PostgreSQL")

  # Se connecter à la base de données
  con <- dbConnect(RPostgres::Postgres(), dbname = "BiodiversitySQL")
  return(con)
}
