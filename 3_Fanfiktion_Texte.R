################### Fanfiction Scraping: Texte der Fanfictions  ################
#------------------------------------------------------------------------------#
### In diesem Skript werden die Texte der Fanfictions gescrapet.

#------------------------------------------------------------------------------#
## Einfuehrung: Workspace leeren und  Funktionen laden ------------------------#
## HIER AUF PFADE ACHTEN! -----------------------------------------------------#
#------------------------------------------------------------------------------#
rm(list = ls())
source("INSERT_PATH_HERE\\Funktionen_Fanfiction.R")

## Arbeitsordner bestimmen: es muss der Ordner mit den gescrapeten URLs sein --#
wd <- "INSERT_PATH_HERE\\Scraping"
setwd(wd)

## Pfad zum Metadatenordner
meta <- paste0(wd, "\\Metadaten")

#------------------------------------------------------------------------------#
## CSVs einlesen
#------------------------------------------------------------------------------#
files <- list.files(path = meta, pattern = "Metadaten.csv")

#------------------------------------------------------------------------------#
## Gibt es einen Ordner fuer Texte? Wenn nein => kreieren
#------------------------------------------------------------------------------#
if("Texte" %in% dir() != T){
  
  dir.create("Texte")
  
}

#------------------------------------------------------------------------------#
## Metadatentabelle erstellen
#------------------------------------------------------------------------------#

for (f in 1: length(files)){
  
  # f <- 1       # diese Zeile wird beim Debugging verwendet! 
  
  urls <- read.csv2(file= paste0(meta,"\\", files[[f]]), stringsAsFactors = F)[,-1]
  #View(urls)
  createTextTable(urls, min.wait = 9, max.wait = 11)
  print(paste("fertig mit Datei ", f, "von", length(files)))
  Sys.sleep(sample(1:9)[1])
}




