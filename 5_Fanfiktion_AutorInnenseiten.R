################### Fanfiction Scraping: Autor*inneninformationen ##############
#------------------------------------------------------------------------------#
### In diesem Skript werden die Daten zu den Autor*innen gescrapet.

#------------------------------------------------------------------------------#
## Einfuehrung: Workspace leeren und  Funktionen laden  -----------------------#
## HIER AUF PFADE ACHTEN !!! --------------------------------------------------#
#------------------------------------------------------------------------------#
rm(list = ls())
source("INSERT_PATH_HERE\\Funktionen_Fanfiction.R")

## Arbeitsordner bestimmen: es muss der Ordner mit den gescrapeten URLs sein --#
wd <- "INSERT_PATH_HERE\\Scraping"
setwd(wd)

## Pfad zum Metadatenordner
meta <- paste0(wd, "\\Metadaten\\")

#------------------------------------------------------------------------------#
## CSVs einlesen
#------------------------------------------------------------------------------#
files <- list.files(path = meta, pattern = "_Metadaten.csv")

#------------------------------------------------------------------------------#
## Gibt es einen Ordner fuer Texte? Wenn nein => kreieren
#------------------------------------------------------------------------------#
if("AutorInneninfo" %in% dir() != T){
  
  dir.create("AutorInneninfo")
  
}

#------------------------------------------------------------------------------#
## Schritt 1: Einlesen der Metadatentabellen & Extraktion aller Autor*innenseiten
#------------------------------------------------------------------------------#
author_pages <- vector("list", length= length(files))

for( f in  1:length(files)){
  
  metadata_table <- read.csv2(paste0(meta, files[[f]]), 
                             stringsAsFactors =F)
  
  author_pages[[f]] <- metadata_table$author_url
  
}

author_pages <- unique(unlist(author_pages))


#------------------------------------------------------------------------------#
## Schritt 2: Scrapen der Autor*inneninformationen.
## Dieser Schritt kann einen Abgleich mit einer Gesamtautor*innentabelle 
## beinhalten. Dafür muss ein Pfad zur Gesamttabelle zusätzlich im Funktionen-
## Skript definiert werden. (s. Funktionen-Skript unter "AutorInneninformationen 
## Scrapen"). Nach 200 Einträgen wird eine Datei abgespeichert, es muss aber nur 
## die letzte behalten werden. 
# 
#------------------------------------------------------------------------------#

createAuthorInfoTable(author_pages, min.wait = 10, 
                      max.wait = 15, 
                      save.after = 200)
