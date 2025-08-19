#' Points (sf) -> Grille d'atlas par espèce (polygones sf)
#'
#' @param point_data Objet `sf` de géométrie POINT ou MULTIPOINT, contenant au moins une colonne espèce.
#' @param lb_nom Colonne espèce, en *tidy-eval* non quotée (ex. `lb_nom = espece`).
#' @param grid_size Taille de maille pour la grille générée (numérique > 0). Unités = unités du CRS projeté utilisé
#'   (en mètres si `crs_target = 2154`). Ignoré si `grid` est fourni.
#' @param grid Grille existante en `sf` (géométries POLYGON/MULTIPOLYGON). Si fourni, la grille est utilisée telle quelle
#'   (reprojetée si nécessaire) et un identifiant `grid_id` est ajouté si absent.
#' @param crs_target EPSG cible pour reprojeter les points lorsque `point_data` est en lon/lat
#'   (défaut `2154`, Lambert-93). Sert aussi d’espace de travail pour la génération de la grille.
#' @param dissolve `TRUE` pour dissoudre les cellules par espèce (un MULTIPOLYGON par espèce). `FALSE` pour garder une ligne
#'   par couple (espèce, cellule).
#' @param count `TRUE` pour calculer et joindre `n_obs` (nombre d’occurrences par (espèce, cellule) ; si `dissolve = TRUE`,
#'   somme par espèce). `FALSE` pour ne pas compter.
#' @param file_chose `TRUE` pour choisir interactivement un fichier/dossier et écrire un `.gpkg` si `out_path` est `NULL`
#'   (ignoré en session non interactive).
#' @param out_path Chemin de sortie optionnel (ex. `"chemin/atlas.gpkg"`). Si `NULL` et `file_chose = TRUE`, un nom est généré
#'   automatiquement dans le dossier choisi. Si non `NULL`, un fichier est écrit via `sf::st_write()`.
#' @param layer Nom de couche dans le fichier de sortie (défaut `"atlas_grid_by_species"`).
#'
#' @return Un objet `sf` de polygones :
#' \itemize{
#'   \item si `dissolve = FALSE` : une ligne par (espèce, `grid_id`) avec la géométrie de cellule ; colonne `n_obs` ajoutée si `count = TRUE` ;
#'   \item si `dissolve = TRUE` : une ligne par espèce (géométrie union des cellules) ; colonne `n_obs` = somme par espèce si `count = TRUE`.
#' }
#' Lorsque aucun point ne tombe dans la grille, une `sf` vide est renvoyée avec le schéma attendu.
#'
#' @details
#' - Si `point_data` possède un CRS géographique (lon/lat), il est reprojeté en `crs_target` avant génération/jointure.
#' - Si `grid` est fourni, il est reprojeté au CRS des points ; sinon une grille carrée est générée avec `st_make_grid()`
#'   et un identifiant `grid_id` est créé.
#' - La jointure spatiale utilise `st_within` (un point compte pour la cellule dans laquelle il tombe).
#'
#' @import sf dplyr rlang
#' @export


crible_point_to_atlas_grid <- function(point_data,
                                       lb_nom,
                                       grid_size = 100,
                                       grid = NULL,
                                       crs_target = 2154,
                                       dissolve   = TRUE,
                                       count      = TRUE,
                                       file_chose = FALSE,
                                       out_path   = NULL,
                                       layer      = "atlas_grid_by_species") {
  # Dépendances
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' requis.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' requis.")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' requis.")
  library(sf); library(dplyr); library(rlang)

  # Vérifs basiques
  if (!inherits(point_data, "sf")) stop("`point_data` doit être un objet sf.")
  geom_type <- unique(sf::st_geometry_type(point_data))
  if (!all(geom_type %in% c("POINT", "MULTIPOINT"))) {
    stop("`point_data` doit être en POINT/MULTIPOINT.")
  }

  lb_nom_quo <- rlang::enquo(lb_nom)
  colname <- rlang::as_name(lb_nom_quo)
  if (!colname %in% names(point_data)) {
    stop("La colonne d'espèce `", colname, "` est introuvable dans `point_data`.")
  }

  # Projection en métrique si nécessaire
  crs_in <- sf::st_crs(point_data)
  if (is.null(crs_in)) stop("`point_data` doit avoir un CRS défini.")
  pts <- point_data
  if (sf::st_is_longlat(crs_in)) {
    pts <- sf::st_transform(pts, crs = crs_target)
  }

  # Grille : fournie ou générée
  if (is.null(grid)) {
    # cellsize en unités de la projection (m si Lambert-93)
    gr <- sf::st_make_grid(pts, cellsize = grid_size, what = "polygons", square = TRUE)
    grid_sf <- sf::st_sf(grid_id = seq_along(gr), geometry = gr, crs = sf::st_crs(pts))
  } else {
    if (!inherits(grid, "sf")) stop("`grid` doit être un sf.")
    gtype <- unique(sf::st_geometry_type(grid))
    if (!all(gtype %in% c("POLYGON", "MULTIPOLYGON"))) {
      stop("`grid` doit être POLYGON/MULTIPOLYGON.")
    }
    grid_sf <- grid
    # Alignement de CRS si besoin
    if (sf::st_crs(grid_sf) != sf::st_crs(pts)) {
      grid_sf <- sf::st_transform(grid_sf, sf::st_crs(pts))
    }
    # Identifiant de cellule
    if (!"grid_id" %in% names(grid_sf)) {
      grid_sf$grid_id <- seq_len(nrow(grid_sf))
    }
  }

  # Jointure spatiale point -> cellule
  # left = FALSE : on garde seulement les points tombant dans la grille
  joined <- sf::st_join(pts, grid_sf, join = sf::st_within, left = FALSE)

  if (nrow(joined) == 0) {
    warning("Aucun point ne tombe dans la grille (CRS/extent/taille ?)")
    # renvoyer une sf vide mais au bon schema
    empty <- grid_sf[0, ]
    empty[[colname]] <- empty$grid_id[0]
    return(empty)
  }

  # Table par (espèce, cellule)
  by_cell <- joined |>
    sf::st_drop_geometry() |>
    dplyr::select(grid_id, !!lb_nom_quo) |>
    dplyr::distinct()

  if (isTRUE(count)) {
    counts <- joined |>
      sf::st_drop_geometry() |>
      dplyr::count(!!lb_nom_quo, grid_id, name = "n_obs") |>
      dplyr::rename(!!colname := !!lb_nom_quo)

    by_cell <- dplyr::left_join(by_cell, counts, by = c(colname, "grid_id"))

  }

  # Rejoindre géométries de cellules
  res <- dplyr::left_join(by_cell, grid_sf, by = "grid_id") |>
    sf::st_as_sf()

  # Option de dissolution par espèce
  if (isTRUE(dissolve)) {
    # On perd grid_id; on garde n_obs total si demandé
    if (isTRUE(count)) {
      res <- res |>
        dplyr::group_by(!!lb_nom_quo) |>
        dplyr::summarise(n_obs = sum(.data$n_obs, na.rm = TRUE),
                         geometry = sf::st_union(geometry),
                         .groups = "drop")
    } else {
      res <- res |>
        dplyr::group_by(!!lb_nom_quo) |>
        dplyr::summarise(geometry = sf::st_union(geometry),
                         .groups = "drop")
    }
    # Nettoyage géométrique (optionnel mais souvent utile)
    res <- sf::st_make_valid(res)
  }

  # Écriture éventuelle
  if (isTRUE(file_chose) && is.null(out_path) && interactive()) {
    # On utilise file.choose() pour sélectionner un fichier existant
    # puis on fabrique un chemin de sortie dans le même dossier.
    chosen <- file.choose()
    dir_out <- dirname(chosen)
    fname   <- paste0("atlas_grid_", format(Sys.Date()), ".gpkg")
    out_path <- file.path(dir_out, fname)
  }
  if (!is.null(out_path)) {
    # Devine le driver via l'extension; pour .gpkg pas besoin de layer unique
    sf::st_write(res, dsn = out_path, layer = layer, append = FALSE, quiet = TRUE)
  }

  return(res)
}
