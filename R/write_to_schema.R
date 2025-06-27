#' @title write_to_schema
#' @description Fonction permettant d'ajouter des lignes dans une BDD
#' @param con Connexion à la BDD (contruite via copo())
#' @param schema Schema de la BDD
#' @param table Schema de la BDD
#' @param data Schema de la BDD
#' @param append Pour ajouter des ligne si TRUE
#' @param overwrite Pour remplacer le tableau si TRUE
#' @examples write_to_schema(con, "inaturalist", "corresp_taxref", new_corresp, append = TRUE)
#' @export

write_to_schema <- function(con, schema, table, data, append = FALSE, overwrite = FALSE) {
  # Vérifie que le schéma existe
  existing_schemas <- dbGetQuery(con, "SELECT schema_name FROM information_schema.schemata;")$schema_name
  if (!(schema %in% existing_schemas)) {
    stop(paste("Le schéma", schema, "n'existe pas dans la base de données."))
  }

  # Crée le nom complet de la table (schéma.nom_table)
  full_table_name <- DBI::Id(schema = schema, table = table)

  # Écrit dans la base
  DBI::dbWriteTable(con, name = full_table_name, value = data, append = append, overwrite = overwrite, row.names = FALSE)
}
