% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrap_data_inat.R
\name{scrap_data_inat}
\alias{scrap_data_inat}
\title{scrap_data_inat}
\usage{
scrap_data_inat(
  requete = NULL,
  year = NULL,
  maxresult = 10000,
  filter_user_login = NULL
)
}
\arguments{
\item{requete}{Requête SQL qui selection la zone d'étude}

\item{year}{Année de selection dans Inaturalist}

\item{maxresult}{Résultats maximum pour la requêtes INaturalist}

\item{filter_user_login}{nom d'utilisateur si filtrer par utilisateur}
}
\value{
Pas le plus important, mais returne les tableaux injectés dans postgreSQL
}
\description{
Récupère les données Inaturalist et les injectes dans la BDD
}
\examples{
scrap_data_inat(requete="SELECT * FROM projet.zone_etude WHERE code IN (27,29);",year=NULL,maxresult=900,filter_user_login = "augustinsoulard")
}
