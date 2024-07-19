################### Fanfiction Scraping: URLs der Unterseiten ##################
#------------------------------------------------------------------------------#
### Erster Schritt des Fanfiction Scrapings: Erstellung der URLs zu
# allen Uebersichtsseiten.

#------------------------------------------------------------------------------#
## Einfuehrung: Workspace leeren und  Funktionen laden  -----------------------#
#------------------------------------------------------------------------------#
rm(list = ls())

# Funktionsskript einlesen: 
source("INSERT_PATH_HERE\\Funktionen_Fanfiction.R")

## Arbeitsordner bestimmen (eigenen Pfad einfuegen)----------------------------#
wd <- "INSERT_PATH_HERE\\Scraping"
setwd(wd)

## URL der Webseite -----------------------------------------------------------#
url <- "https://www.fanfiktion.de/Fanfiction/c/100000000"
session <- session("https://www.fanfiktion.de/")

#------------------------------------------------------------------------------#
## Variable fuer Datum einstellen ---------------------------------------------#
#------------------------------------------------------------------------------#
date <- paste(unlist(strsplit(as.character(Sys.Date()), "-")), collapse = "")

#------------------------------------------------------------------------------#
## Gibt es einen Ordner fuer die neuen Daten? Wenn nein 
##                                => kreieren & als Arbeitsordner festlegen!
#------------------------------------------------------------------------------#
if(date %in% dir() != T){
   
   dir.create(date)
   setwd(paste0(wd, "\\", date))
   
   }else{
   setwd(paste0(wd, "\\", date))
      }

#------------------------------------------------------------------------------#
## Scraping: URLs fuer jede Uebersichtsseite generieren 
## (Ende der URL: updatedate)
#------------------------------------------------------------------------------#
urls <- getCategoriesUrlNested(url)

#------------------------------------------------------------------------------#
## Scraping: Dataframe mit URL und Anzahl der Geschichten erstellen 
#------------------------------------------------------------------------------#
story.amount <- list()

for( i in 1: length(urls)){
   story.amount[[i]] <- getFanficAmountFromOverview (as.character(urls[[i]]))

   # Zur Uebersicht: 
   print(paste("fertig mit", urls[[i]], ",", i, "von", length(urls)))
   
   # Damit Fanfiktion.de nicht ueberlastet wird, bauen wir eine Pause ein
   Sys.sleep(print(sample(5:10)[1]))

}

#------------------------------------------------------------------------------#
## Einige Kategorien haben 0 Geschichten --------------------------------------#
#------------------------------------------------------------------------------#
story.amount[which(sapply(story.amount, length) == 0)] <- 0

urls.storyamount.df <- data.frame(urls, unlist(story.amount))
colnames(urls.storyamount.df) <- c("urls", "story_amount")

#------------------------------------------------------------------------------#
## Spalte "pageamount" beinhaltet die Anzahl der Unterseiten fuer 
## jede Uebersichtsseite
#------------------------------------------------------------------------------#
urls.storyamount.df$pageamount <- round_any(urls.storyamount.df$story_amount/20, 1, f = ceiling)

#------------------------------------------------------------------------------#
## Scraping: Tabelle mit URL und Anzahl der Geschichten erstellen und speichern
#------------------------------------------------------------------------------#
urls.with.subpages <- list()

for( i in 1: length(urls.storyamount.df$urls)){
   if(urls.storyamount.df$story_amount[[i]] != 0){
   urls.with.subpages[[i]] <- paste0(strsplit(as.character(urls.storyamount.df$urls[[i]]), "1/updatedate"), 1:urls.storyamount.df$pageamount[[i]], "/updatedate")  
   }
}

write.csv2(unlist(urls.with.subpages), paste0(date, "_urlsMitUnterseiten.csv"))
