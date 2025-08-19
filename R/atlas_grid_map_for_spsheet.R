#' Cartes OSM centrées sur une emprise unique (une carte par espèce)
#'
#' @param atlas_polys sf POLYGON/MULTIPOLYGON (sortie de `crible_point_to_atlas_grid`), contenant au moins une colonne espèce.
#' @param lb_nom Colonne espèce, en *tidy-eval* non quotée (ex. `lb_nom = espece`).
#' @param focus Vecteur d'espèces à cartographier (caractères). `NULL` = toutes les espèces présentes.
#' @param export `TRUE` pour exporter des PNG, `FALSE` pour renvoyer seulement les objets `ggplot`.
#' @param file_chose `TRUE` pour ouvrir un dialogue de fichier et récupérer le dossier (ignoré si `out_dir` non‐NULL ou si session non interactive).
#' @param out_dir Dossier de sortie. Si `NULL` et `file_chose = TRUE`, choisi via dialogue ; sinon `getwd()`.
#' @param filename_fmt Modèle du nom de fichier. Utiliser `{species}` qui sera remplacé par un *slug* du nom d'espèce (ex. `"{species}_atlas.png"`).
#' @param width_px,height_px,dpi Dimensions d'export en pixels et résolution (ppi). La taille en pouces est calculée comme `width_px / dpi` et `height_px / dpi`.
#' @param basemap Type de tuiles pour `ggspatial::annotation_map_tile` (ex. `"osm"`). Mettre `NULL` pour ne pas afficher de fond.
#' @param basemap_alpha Opacité (0–1) appliquée au fond de carte (par défaut `0.5`).
#' @param fill,fill_alpha,border_col,border_size Style des polygones des mailles (couleur de remplissage, opacité, couleur/épaisseur de bord).
#' @param extent_source Source de l'emprise unique utilisée pour toutes les cartes : `"atlas"` (défaut, union des mailles), `"points"` (d'après `points_sf`), ou `"custom"`.
#' @param points_sf Objet `sf` de géométrie POINT/MULTIPOINT utilisé si `extent_source = "points"`. Doit avoir un CRS défini.
#' @param extent_custom Emprise personnalisée si `extent_source = "custom"` : soit un objet `sf/sfc` (n'importe quelle géométrie), soit un vecteur numérique `c(xmin, ymin, xmax, ymax)`.
#' @param extent_crs EPSG de `extent_custom` quand celui-ci est un vecteur numérique (défaut `4326`).
#' @param margin_km Marge (km) tampon ajoutée autour de l'emprise unique.
#' @param crs_basemap EPSG utilisé pour la carte et le fond (par défaut `3857`, Web Mercator).
#'
#' @return `list(plots = <liste nommée de ggplot par espèce>, files = <chemins des PNG exportés si export = TRUE>)`
#' @import sf ggplot2 ggspatial dplyr rlang
#' @export
atlas_grid_map_for_spsheet <- function(
    atlas_polys,
    lb_nom,
    focus        = NULL,
    export       = FALSE,
    file_chose   = FALSE,
    out_dir      = NULL,
    filename_fmt = "{species}_atlas.png",
    width_px     = 1600,
    height_px    = 1200,
    dpi          = 200,
    basemap      = "osm",
    basemap_alpha = 0.5,
    fill         = "#2C7FB8",
    fill_alpha   = 0.35,
    border_col   = "black",
    border_size  = 0.5,
    extent_source = c("atlas","points","custom"),
    points_sf     = NULL,
    extent_custom = NULL,
    extent_crs    = 4326,
    margin_km     = 1,
    crs_basemap   = 3857
){
  if (!requireNamespace("sf", quietly = TRUE)) stop("Package 'sf' requis.")
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Package 'ggplot2' requis.")
  if (!requireNamespace("ggspatial", quietly = TRUE)) stop("Package 'ggspatial' requis.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Package 'dplyr' requis.")
  if (!requireNamespace("rlang", quietly = TRUE)) stop("Package 'rlang' requis.")
  library(sf); library(ggplot2); library(ggspatial); library(dplyr); library(rlang)

  if (!inherits(atlas_polys, "sf")) stop("`atlas_polys` doit être un sf.")
  extent_source <- match.arg(extent_source)

  sp_quo <- enquo(lb_nom)
  sp_col <- as_name(sp_quo)
  if (!sp_col %in% names(atlas_polys)) stop("Colonne espèce introuvable: `", sp_col, "`.")

  # Filtre éventuel
  x <- atlas_polys
  if (!is.null(focus)) {
    x <- dplyr::filter(x, !!sp_quo %in% focus)
    if (nrow(x) == 0) stop("Aucune géométrie après filtrage `focus`.")
  }

  # CRS & Web Mercator
  if (is.null(st_crs(x))) stop("`atlas_polys` doit avoir un CRS défini.")
  x3857 <- st_transform(x, crs_basemap)

  # Déterminer l'emprise UNIQUE pour toutes les cartes
  margin_m <- margin_km * 1000
  bbox_all <- switch(
    extent_source,
    "atlas" = {
      st_bbox(st_buffer(st_union(st_geometry(x3857)), margin_m))
    },
    "points" = {
      if (is.null(points_sf)) stop("`points_sf` doit être fourni quand extent_source='points'.")
      if (!inherits(points_sf, "sf")) stop("`points_sf` doit être un sf.")
      pts <- st_transform(points_sf, crs_basemap)
      st_bbox(st_buffer(st_union(st_geometry(pts)), margin_m))
    },
    "custom" = {
      if (inherits(extent_custom, "sf") || inherits(extent_custom, "sfc")) {
        bb <- st_bbox(st_transform(st_as_sf(extent_custom), crs_basemap))
        st_bbox(st_buffer(st_as_sfc(bb), margin_m))
      } else if (is.numeric(extent_custom) && length(extent_custom) == 4) {
        bb <- st_bbox(c(xmin=extent_custom[1], ymin=extent_custom[2],
                        xmax=extent_custom[3], ymax=extent_custom[4]), crs = st_crs(extent_crs))
        bb_3857 <- st_bbox(st_transform(st_as_sfc(bb), crs_basemap))
        st_bbox(st_buffer(st_as_sfc(bb_3857), margin_m))
      } else {
        stop("`extent_custom` doit être un sf/sfc ou un numeric c(xmin, ymin, xmax, ymax).")
      }
    }
  )
  xlim <- c(bbox_all["xmin"], bbox_all["xmax"])
  ylim <- c(bbox_all["ymin"], bbox_all["ymax"])

  # Export : dossier
  if (export) {
    if (isTRUE(file_chose) && is.null(out_dir) && interactive()) {
      chosen <- utils::file.choose()
      out_dir <- dirname(chosen)
    }
    if (is.null(out_dir)) out_dir <- getwd()
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Basemap : fallback si rosm/prettymapr manquants
  if (!is.null(basemap)) {
    if (!requireNamespace("prettymapr", quietly = TRUE) ||
        !requireNamespace("rosm", quietly = TRUE)) {
      warning("Basemap demandé mais 'prettymapr'/'rosm' non installés -> carte sans fond.\n",
              "Installez-les avec: install.packages(c('prettymapr','rosm'))")
      basemap <- NULL
    }
  }

  slugify <- function(s){
    s <- enc2utf8(as.character(s))
    s <- iconv(s, to = "ASCII//TRANSLIT")
    s <- tolower(gsub("[^a-z0-9]+", "-", s))
    gsub("(^-|-$)", "", s)
  }

  # -- Une figure par espèce, même xlim/ylim pour toutes --
  make_one <- function(sfi){
    species <- as.character(dplyr::pull(sfi, !!sp_quo)[1])
    ncells  <- nrow(sfi)
    nobs    <- if ("n_obs" %in% names(sfi)) sum(sfi$n_obs, na.rm = TRUE) else NA
    subtitle <- if (!is.na(nobs)) sprintf("%d maille(s) — %d obs.", ncells, nobs)
    else sprintf("%d maille(s)", ncells)

    p <- ggplot()
    if (!is.null(basemap)) p <- p + ggspatial::annotation_map_tile(type = basemap,alpha = basemap_alpha)
    p <- p +
      geom_sf(data = sfi, fill = fill, alpha = fill_alpha,
              color = border_col, linewidth = border_size) +
      coord_sf(crs = crs_basemap, xlim = xlim, ylim = ylim, expand = FALSE) +
      ggspatial::annotation_scale(location = "bl", width_hint = 0.25) +
      ggspatial::annotation_north_arrow(location = "tl", which_north = "true",
                                        style = ggspatial::north_arrow_fancy_orienteering) +
      theme_minimal(base_size = 12) +
      theme(axis.title = element_blank(), axis.text = element_blank(),
            axis.ticks = element_blank(), panel.grid = element_blank(),
            plot.title = element_text(face = "bold"), legend.position = "none")

    file_out <- NULL
    if (export) {
      w_in <- width_px / dpi; h_in <- height_px / dpi
      fname <- gsub("\\{species\\}", slugify(species), filename_fmt)
      file_out <- file.path(out_dir, fname)
      ggsave(file_out, p, width = w_in, height = h_in, dpi = dpi, units = "in")
    }
    list(species = species, plot = p, file = file_out)
  }

  res <- x3857 |>
    group_split(!!sp_quo, .keep = TRUE) |>
    lapply(make_one)

  list(
    plots = setNames(lapply(res, `[[`, "plot"), sapply(res, `[[`, "species")),
    files = unname(na.omit(unlist(lapply(res, `[[`, "file"))))
  )
}
