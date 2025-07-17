#' @title trigrammisation
#' @description Génère un trigramme unique pour chaque taxon en se basant sur les règles de construction à partir du nom valide. Gère les cas de sous-espèces, variétés, formes, hybrides, et doublons. Permet également d'utiliser le nom complet pour les rangs supérieurs au genre.
#' @param nom_valide Vecteur de caractères contenant les noms scientifiques valides (par défaut : `taxref$nom_valide`)
#' @param lb_nom Vecteur de caractères contenant le libellé du nom (par défaut : `taxref$lb_nom`)
#' @param rang Vecteur de caractères indiquant le rang taxonomique (par défaut : `taxref$rang`)
#' @return Un vecteur de chaînes de caractères correspondant au trigramme attribué à chaque taxon
#' @importFrom stringr str_detect str_locate str_split
#' @importFrom dplyr rename
#' @examples
#' trigrammisation(nom_valide = c("Quercus ilex", "Quercus x pubescens"),
#'                 lb_nom = c("Chêne vert", "Chêne hybride"),
#'                 rang = c("ES", "ES"))
#' @export


trigrammisation = function(nom_valide = taxref$nom_valide,
                           lb_nom = taxref$lb_nom,
                           rang = taxref$rang )
{
  # Chargement des packages
  if (!require("tidyverse")) {install.packages("tidyverse")}+library("tidyverse")
  left <- function(x, n) substr(x, 1, n) # si tu veux vraiment conserver `left()`

  #Chargement de TAXREF
  TAXREF = data.frame( NOM_VALIDE = nom_valide,
                       LB_NOM = lb_nom,
                       RANG = rang)

  TAXREF$ESPBRUT = paste0(left(TAXREF$NOM_VALIDE,3),
                          substr(TAXREF$NOM_VALIDE, str_locate(TAXREF$NOM_VALIDE," ")+1,
                                 str_locate(TAXREF$NOM_VALIDE," ")+3))

  TAXREF$subsp = str_detect(TAXREF$NOM_VALIDE,"subsp. ")
  TAXREF$var = str_detect(TAXREF$NOM_VALIDE,"var. ")
  TAXREF$f = str_detect(TAXREF$NOM_VALIDE," f. ")



  for(i in 1:nrow(TAXREF)){
    if(TAXREF$subsp[i]){
      TAXREF$ESP[i] = paste0(TAXREF$ESPBRUT[i],
                             substr(TAXREF$NOM_VALIDE[i], str_locate(TAXREF$NOM_VALIDE[i],"subsp. ")+7,
                                    str_locate(TAXREF$NOM_VALIDE[i],"subsp. ")+9))
    }
    if(TAXREF$var[i]){
      TAXREF$ESP[i] = paste0(TAXREF$ESPBRUT[i],
                             substr(TAXREF$NOM_VALIDE[i], str_locate(TAXREF$NOM_VALIDE[i],"var. ")+5,
                                    str_locate(TAXREF$NOM_VALIDE[i],"var. ")+7))
    }
    if(TAXREF$f[i]){
      TAXREF$ESP[i] = paste0(TAXREF$ESPBRUT[i],
                             substr(TAXREF$NOM_VALIDE[i], str_locate(TAXREF$NOM_VALIDE[i]," f. ")+4,
                                    str_locate(TAXREF$NOM_VALIDE[i]," f. ")+6))
    }
    if((TAXREF$subsp[i]|TAXREF$var[i]|TAXREF$f[i])==FALSE){
      TAXREF$ESP[i] = TAXREF$ESPBRUT[i]
    }
    cat(i,"\n")
  }

  #Gestion des hybrides
  TAXREF$x = str_detect(TAXREF$NOM_VALIDE," x ")
  TAXREFX = TAXREF[TAXREF$x==TRUE,]
  xsplit = str_split(TAXREFX$NOM_VALIDE," ")


  #Application de la fonction ) la liste
  fun = function(x){
    if(x[3]=="x"){
      paste0(left(x[1],3),left(x[2],3),"x",left(x[4],3),left(x[5],3))
    } else {
      paste0(left(x[1],3),"x",left(x[3],3))
    }



  }


  TAXREFX$ESP = unlist(lapply(xsplit,FUN=fun))
  TAXREFX$ESP2 = TAXREFX$ESP

  # Gestion des duplicats x
  TAXREFDX = TAXREFX[duplicated(TAXREFX$ESP)==TRUE,]
  for(i in 1:nrow(TAXREFDX)){
    num = c(1:nrow(TAXREFDX[TAXREFDX$ESP == TAXREFDX$ESP[i],]))
    for(j in 1:length(num)){
      if((TAXREFDX$subsp[i]|TAXREFDX$var[i]|TAXREFDX$f[i])==FALSE){
        TAXREFDX[TAXREFDX$ESP ==TAXREFDX$ESP[i],]$ESP2[j] = paste0(TAXREFDX[TAXREFDX$ESP ==TAXREFDX$ESP[i],]$ESP[j],num[j]+1)
      } else {
        cat("test")
        TAXREFDX[TAXREFDX$ESP ==TAXREFDX$ESP[i],]$ESP2[j] = paste0(TAXREFDX[TAXREFDX$ESP ==TAXREFDX$ESP[i],]$ESP[j],"sub")
      }

    }
    cat(i,"\n")
  }

  #REMISE dans le tableau principale
  TAXREFX[duplicated(TAXREFX$ESP)==TRUE,] =TAXREFDX
  TAXREF[TAXREF$x==TRUE,] = TAXREFX

  # Dernier passage des doubons

  TAXREFD = TAXREF[duplicated(TAXREF$ESP),]
  TAXREFD$ESP2 = as.character(TAXREFD$ESP)

  for(i in 1:nrow(TAXREFD)){
    num = c(1:nrow(TAXREFD[TAXREFD$ESP ==TAXREFD$ESP[i],]))
    for(j in 1:length(num)){
      TAXREFD[TAXREFD$ESP ==TAXREFD$ESP[i],]$ESP2[j] = paste0(TAXREFD[TAXREFD$ESP ==TAXREFD$ESP[i],]$ESP[j],num[j]+1)
    }
    cat(i,"\n")
  }
  TAXREF$ESP2 = TAXREF$ESP
  TAXREF[duplicated(TAXREF$ESP),]=TAXREFD


  ### GENRE EN UN MOT :
  JOIN_GENRE_SAISIE = TAXREF


  for(i in 1:nrow(JOIN_GENRE_SAISIE)){
    if(JOIN_GENRE_SAISIE$RANG[i]=="GN"||JOIN_GENRE_SAISIE$RANG[i]=="FM"||JOIN_GENRE_SAISIE$RANG[i]=="TR"||JOIN_GENRE_SAISIE$RANG[i]=="SPTR"||JOIN_GENRE_SAISIE$RANG[i]=="SSTR"||JOIN_GENRE_SAISIE$RANG[i]=="SBFM"||JOIN_GENRE_SAISIE$RANG[i]=="SSGR"){
      JOIN_GENRE_SAISIE$ESP2[i]=JOIN_GENRE_SAISIE$LB_NOM[i]}
    cat(i,"\n")
  }
  # Retirer les colonnes de travail
  TAXREF_FLORE_FR = JOIN_GENRE_SAISIE
  TAXREF_FLORE_FR = TAXREF_FLORE_FR %>% rename(trigramme = ESP2)


  return(TAXREF_FLORE_FR$trigramme)

}
