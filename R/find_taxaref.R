#' @title find_taxaref
#' @description
#' Recherche les correspondances entre une liste de taxons (issus d'une source externe, par ex. iNaturalist)
#' et la base TAXREF (ou autre référentiel taxonomique). Nettoie, standardise et enrichit les entrées,
#' puis associe les codes et rangs correspondants depuis le référentiel.
#'
#' @param lb_taxa_entree Vector de chaînes de caractères. Libellés de taxons à rechercher (ex. noms scientifiques).
#' @param code_taxa_entree Vector (optionnel). Codes des taxons d'entrée, si disponibles (défaut : NA).
#' @param ref Character. Référentiel de correspondance, par défaut "taxref".
#' @param input_ref Character. Source des noms d'entrée ("inaturalist", "baseflor", etc.), utilisé pour ajuster la correspondance (défaut : "inaturalist").
#'
#' @return Un data.frame avec, pour chaque taxon d'entrée : le code d'entrée, le libellé nettoyé,
#' le rang, les codes et noms correspondants dans TAXREF (ou référentiel choisi), et le code cd_ref mis à jour.
#'
#' @details
#' La fonction effectue des opérations de nettoyage sur les noms de taxons,
#' corrige certains formats (ex. ssp. → subsp.), supprime les parenthèses et mots parasites,
#' détermine le rang (espèce, sous-espèce, variété…), puis cherche la correspondance exacte dans le référentiel taxonomique choisi.
#'
#' @examples
#' \dontrun{
#' find_taxaref(
#'   lb_taxa_entree = c("Ranunculus aquatilis subsp. aquatilis", "Centaurea jacea"),
#'   code_taxa_entree = c(NA, NA),
#'   ref = "taxref",
#'   input_ref = "inaturalist"
#' )
#' }
#'
#' @export



find_taxaref <- function(
    lb_taxa_entree,
    code_taxa_entree = NA,
    ref='taxref',
    input_ref="inaturalist"
){
  entree = data.frame(code_taxa_entree = code_taxa_entree,lb_taxa_entree = lb_taxa_entree)

  if(ref == 'taxref'){
    taxref = dbGetQuery(con, "SELECT * FROM public.taxref_flore_fr_syn")
  }

  #Préparation du tableau entree
  entree <- entree %>%
    mutate(cd_nom = NA_character_)
  entree <- entree %>%
    mutate(lb_nom = NA_character_)
  entree <- entree %>%
    mutate(rang = NA_character_)

  # Correction des adjectifs de rangs
  entree$lb_taxa_entree = str_replace(entree$lb_taxa_entree,'ssp.','subsp.')

  # Supprimer les mots entre parenthèses
  entree$lb_taxa_entree = gsub("\\(.*?\\)", "", entree$lb_taxa_entree)
  entree$lb_taxa_entree = gsub("\\b(?!subsp\\.|var\\.)\\w+\\.", "", entree$lb_taxa_entree, perl=TRUE)
  entree$lb_taxa_entree = gsub("\\s+", " ", entree$lb_taxa_entree)
  entree$lb_taxa_entree = sapply(entree$lb_taxa_entree, majuscule_hors_premier_mot)# retirer les mot avec majuscule hor spremier mot ou hybrides
  entree$lb_taxa_entree = gsub("[^[:alnum:] ]", "", entree$lb_taxa_entree)
  entree$lb_taxa_entree = gsub("[[:space:]]+$", "", entree$lb_taxa_entree)
  entree$lb_taxa_entree <- sapply(entree$lb_taxa_entree, function(x) {
    # Si la chaîne contient " subsp", " var", ou " f", on ne modifie pas
    if (grepl(" subsp| var| f", x)) {
      return(x)
    } else if (grepl(" x ", x)) {
      # Si la chaîne contient " x ", on garde les trois premiers mots
      mots <- strsplit(x, " ")[[1]]
      return(paste(mots[1:min(3, length(mots))], collapse = " "))
    } else {
      # Sinon, on garde les deux premiers mots
      mots <- strsplit(x, " ")[[1]]
      return(paste(mots[1:min(2, length(mots))], collapse = " "))
    }
  }) #ENCORE A VERIFIER


  # Ajoute une colonne de RANG au tableau entree
  for(i in 1:nrow(entree)){
    cat('rang : ',i,'/',nrow(entree),'\n')
    if(str_detect(entree$lb_taxa_entree[i],'subsp.')){
      entree$rang[i] = 'SSES'
    }
    else if(str_detect(entree$lb_taxa_entree[i],'var.')){
      entree$rang[i] = 'VAR'
    }
    else{entree$rang[i] = 'ES'}
  }

  # Dénition des colonnes cd_nom et lb_nom par défaut
  entree$cd_nom = '_NOMATCH'
  entree$lb_nom = '_NOMATCH'

  # Retirer les marqueurs d'infrataxons pour la correspondance avec Inaturalist
  if(input_ref=="inaturalist"){
    taxref$lb_nom = taxref$lb_nom %>%
      str_replace(' subsp.','') %>%
      str_replace(' var.','') %>%
      str_replace(' f.','')
  }

  if(input_ref=="baseflor"){
    taxref$lb_nom = taxref$lb_nom %>%
      str_replace(' subsp.',' subsp') %>%
      str_replace(' var.',' var') %>%
      str_replace(' f.',' f')
  }

  for (i in 1:nrow(entree)) {
    cat('CORRESP : ',i,'/',nrow(entree),'\n')

    # Trouver les correspondances
    if(str_detect(entree$lb_taxa_entree[i]," x ")){

      taxref_rang = taxref[taxref$rang==entree$rang[i],]

    } else {
      taxref_rang = taxref[taxref$rang==entree$rang[i] & !str_detect(taxref$lb_nom," x "),]
    }
    matches <- str_detect(taxref_rang$lb_nom,fixed(entree$lb_taxa_entree[i]))

    # Pour Inaturalist tenter de faire une correpsondance des sous-espèces
    if(input_ref=="inaturalist" & !any(matches)){
      taxref_rang = taxref[taxref$rang=="SSES",]
      matches <- str_detect(taxref_rang$lb_nom,fixed(entree$lb_taxa_entree[i]))
    }

    # Vérifier les hybrides

    if(any(matches)){
      lb_nom = taxref_rang[matches==TRUE,]$lb_nom
      cd_nom = taxref_rang[matches==TRUE,]$cd_nom

      # Mettre à jour la colonne CD_NOM pour les correspondances trouvées
      entree$cd_nom[i] <- cd_nom
      entree$lb_nom[i] <- lb_nom
    }

  }
  #Mise à jour du code CD_REF
  entree$cd_ref = updatetaxa(entree$cd_nom)

  return(entree)
}
