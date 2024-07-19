################### Fanfiction Scraping: Datumsfilter ##########################
#------------------------------------------------------------------------------#
### Nachdem die URLs zu allen Unterseiten erstellt wurden, wird der 
### Datumsfilter eingebaut und gleichzeitig die Daten fuer die Metadatentabelle
### gescrapet.

### Eintraege, die geaendert werden muessen, sind mit *!!* markiert. Vor der 
### Anwendung bitte Skript nach *!!* durchsuchen.

#------------------------------------------------------------------------------#
## Einfuehrung: Workspace leeren und  Funktionen laden  -----------------------#
#------------------------------------------------------------------------------#
rm(list = ls())
source("INSERT_PATH_HERE\\Funktionen_Fanfiction.R")

## Arbeitsordner bestimmen (eigenen Pfad einfuegen)----------------------------#
wd <- "INSERT_PATH_HERE\\Scraping\\"
setwd(wd)

#------------------------------------------------------------------------------#
## Gibt es einen Ablageordner? Wenn nein => kreieren
#------------------------------------------------------------------------------#
if("Metadaten" %in% dir() != T){
  
  dir.create("Metadaten")
  
}

#------------------------------------------------------------------------------#
## URLs der Unterseiten einlesen 
#------------------------------------------------------------------------------#
urls <- read.csv2(list.files(pattern = "urlsMitUnterseiten.csv", recursive = T), #### recursive = T
                  stringsAsFactors = F)[,-1]

#------------------------------------------------------------------------------#
## Aus den URLs ein Dataframe mit Kategorientitel und Kategorien-ID -----------#
#------------------------------------------------------------------------------#
urls.df <- data.frame (urls, 
                       sapply(strsplit(sapply(strsplit(urls, "https://www.fanfiktion.de/"), "[[", 2), "/c/"), "[[", 1), 
                       sapply(strsplit(urls, "/"), "[[",6))

colnames(urls.df) <- c("url", "category_title", "category_id")

urls.df.list <- split(urls.df, urls.df$category_id)


#------------------------------------------------------------------------------#
## Datumsfilter (Datum). Ergebnis der Funktion: CSV-Dateien im "Metadaten"-Ordner
## Sollte ein ganzes Jahr gescrapet werden, gibt man den ersten Januar an, 
## z.b. bei 2020 - "01.01.2020"
## Format: TT.MM.JJJJ 
#------------------------------------------------------------------------------#
filterByDateMetadata(urls.df.list, min.wait = 10, max.wait = 20, date = "*!!*Datum einfuegen")
