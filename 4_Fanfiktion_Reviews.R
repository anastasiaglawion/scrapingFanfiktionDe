################### Fanfiction Scraping: Reviews ###############################
#------------------------------------------------------------------------------#
### In diesem Skript werden die Fanfictionreviews gescrapet.

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
meta <- paste0(wd, "\\Metadaten")

#------------------------------------------------------------------------------#
## CSVs einlesen
#------------------------------------------------------------------------------#
files <- list.files(path = meta, pattern = "_Metadaten.csv")

#------------------------------------------------------------------------------#
## Gibt es einen Ordner fuer Texte? Wenn nein => kreieren
#------------------------------------------------------------------------------#
if("Reviews" %in% dir() != T){
  
  dir.create("Reviews")
  
}

#------------------------------------------------------------------------------#
## Metadatentabellen werden eingelesen und Reviews gescrapet:
#------------------------------------------------------------------------------#
for( f in  1:length(files)){
  # f <- 80
  metadata_table <- read.csv2(paste0(wd, "\\Metadaten\\", 
                                    files[[f]]), 
                             stringsAsFactors =F)
  #View(metadata_table)
  
  if(nrow(subset.data.frame(metadata_table, review_amount != 0)) != 0){
    metadata_w_reviews <- subset.data.frame(metadata_table, review_amount != 0)
    createReviewTable(metadata_w_reviews, min.wait = 15, max.wait = 20)
    print(paste("!!!!!! fertig mit Datei ", 
                 files[[f]], 
                 "| Datei Nr.", 
                 f, "von", length(files)))
    
    }else{
      print( paste("!!!!!! fertig mit Datei ", 
                   files[[f]], "| Datei Nr.", 
                   f, "von", length(files), ". Keine Reviews!"))
      }
  

}
 