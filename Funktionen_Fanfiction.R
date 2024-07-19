################ FUNKTIONEN FUER SCRAPING DES FANFICTION KORPUS ################
#------------------------------------------------------------------------------#

### Hier sind die Funktionen fuer die monatlichen Scrapings des Korpus von 
### Fanfiktion.de. 
### Skript wird ausschliesslich ueber source() aus anderen aufgerufen. 

#------------------------------------------------------------------------------#
## Benoetigte Bibliotheken ----------------------------------------------------#
#------------------------------------------------------------------------------#
require("rvest") 
require("dplyr")
require("plyr")
require("RCurl")
require("tm")
require("xml2")
require("lubridate")
require("httr")


#------------------------------------------------------------------------------#
## Organisationsvariablen -----------------------------------------------------#
#------------------------------------------------------------------------------#

## Session mit der Seite erstellen  -------------------------------------------#
url <- "https://www.fanfiktion.de/Fanfiction/c/100000000"
session <- session("https://www.fanfiktion.de/")

## Datum 
date <- paste(unlist(strsplit(as.character(Sys.Date()), "-")), collapse = "")

#------------------------------------------------------------------------------#
## Funktionen -----------------------------------------------------------------#
#------------------------------------------------------------------------------#

## Einfache Funktion zur Extraktion der Kategorienurls
## Anwendbar auf allen Uebersichtsseiten: 
## z.b. https://www.fanfiktion.de/Fanfiction/c/100000000    aber auch 
## https://www.fanfiktion.de/Buffy-Angel/c/101006000        oder
## https://www.fanfiktion.de/TV-Serien/c/101000000
#------------------------------------------------------------------------------#
getCategoriesUrlSimple <- function(url){  
  
  page <- session_jump_to(session, url)
  
  cat_title <- html_nodes(x = read_html(page), 
                          css = "#ffcbox-dirlist-layer-SL a") 
  
  df <- bind_rows(lapply(xml_attrs(cat_title), 
                         function(x) data.frame(as.list(x), 
                                                stringsAsFactors=FALSE)))
  
  return(as.character(paste0("https://www.fanfiktion.de",df$href)))
}

#------------------------------------------------------------------------------#
# Iterative Funktion zur Extraktion der Seiten, die zu den Fanfictions fuehren #
# Idealerweise anwendbar auf https://www.fanfiktion.de/Fanfiction/c/100000000
#------------------------------------------------------------------------------#

getCategoriesUrlNested <- function(url, min, max){  
  
  if (length(unlist(strsplit(url, "/"))) == 8){
    
    page <- session_jump_to(session, url)
    
    cat_title <- html_nodes(x = read_html(page), css = "#ffcbox-dirlist-layer-SL a") 
    
    df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    
    paste0("https://www.fanfiktion.de",df$href)
  }
  if (length(unlist(strsplit(url, "/"))) == 6){
    
    page <- session_jump_to(session, url)
    
    cat_title <- html_nodes(x = read_html(page), css = "#ffcbox-dirlist-layer-SL a") 
    
    df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    
    new_urls <- paste0("https://www.fanfiktion.de",df$href)
    
    fandomcategory_urls1 <- list()
    
    for( i in 1:length(new_urls)){
      
      page <- session_jump_to(session, new_urls[[i]])
      
      cat_title <- html_nodes(x = read_html(page), css = "#ffcbox-dirlist-layer-SL a") 
      
      df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
      
      fandomcategory_urls1[[i]] <- paste0("https://www.fanfiktion.de",df$href)
    }
    
    fandomcategory_urls1 <- unlist(fandomcategory_urls1)
    
    fandomcategory_urls2 <- list()
    
    for( i in 1:length(fandomcategory_urls1)){
      if (length(unlist(strsplit(fandomcategory_urls1[[i]], "/"))) == 6){
        page <- session_jump_to(session, fandomcategory_urls1[[i]])
        cat_title <- html_nodes(x = read_html(page), css = "#ffcbox-dirlist-layer-SL a") 
        df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
        fandomcategory_urls2[[i]] <- paste0("https://www.fanfiktion.de",df$href)
      }
    }
    fandomcategory_urls2 <- unlist(fandomcategory_urls2)
    fandomcategory_urls3 <- list()
    for( i in 1:length(fandomcategory_urls2)){
      if (length(unlist(strsplit(fandomcategory_urls2[[i]], "/"))) == 6){
        page <- session_jump_to(session, fandomcategory_urls2[[i]])
        cat_title <- html_nodes(x = read_html(page), css = "#ffcbox-dirlist-layer-SL a") 
        df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
        fandomcategory_urls3[[i]] <- paste0("https://www.fanfiktion.de",df$href)
      }
    }
    fandomcategory_urls3 <- unlist(fandomcategory_urls3)
    if (length(which(sapply(strsplit(fandomcategory_urls3, "/"), length) != 8)) != 0){
      print("!! Eine untergeordnete Ebene wurde nicht erfasst !!")
    }
    
    all_urls <- c(new_urls, fandomcategory_urls1, fandomcategory_urls2, fandomcategory_urls3)
    x <- sort(all_urls[which(sapply(strsplit(all_urls, "/"), length) == 8)], decreasing = T)
    return(x)
    
  }
  Sys.sleep(sample(min:max)[1])
  
}  


#------------------------------------------------------------------------------#
## Titel der Kategorie aus der Kategorienansicht, 
## bspw.: https://www.fanfiktion.de/Anime-Manga/c/102000000
#------------------------------------------------------------------------------#
getCategoriesTitle <- function(url){  
  page <- session_jump_to(session, url)
  cat_title <- html_nodes(x = read_html(page), css = "#ffcbox-dirlist-layer-SL a") 
  df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  return(as.character(sapply(strsplit(df$href, "/"), "[[", 2)))
}

#------------------------------------------------------------------------------#
## Anzahl der FFs aus der Kategorienansicht, 
## bspw. https://www.fanfiktion.de/Anime-Manga/c/102000000 
#------------------------------------------------------------------------------#
getFanficAmountPerCategory <- function(url){  
  page <- session_jump_to(session, url)
  return(as.numeric(html_text(html_nodes(x = read_html(page) , css = ".badge-spacer") , trim = TRUE)))
}

#------------------------------------------------------------------------------#
## Anzahl der FFs aus der Geschichtenuebersicht, 
## bspw. https://www.fanfiktion.de/One-Punch-Man/c/300000528/1/updatedate 
#------------------------------------------------------------------------------#
getFanficAmountFromOverview <- function(url){  
  page <- session_jump_to(session, url)
  return(as.numeric(html_text(html_nodes(x = read_html(page), 
                                         css = ".resultcount-page~ .resultcount-page+ .resultcount-page"), 
                              trim = TRUE)))
  
}

#------------------------------------------------------------------------------#
## Datum der letzten Updates aus der Geschichtenuebersicht, 
## bspw. https://www.fanfiktion.de/One-Punch-Man/c/300000528/1/updatedate 
#------------------------------------------------------------------------------#
getFanfictionUpdateDate <- function(url){  
  page <- session_jump_to(session, url)
  
  
  if(length(html_text(html_nodes(x = read_html(page), css = ".center:nth-child(4) .floatleft:nth-child(2)"), trim = TRUE)) == 0){
    print("NoEntry")
  }else{
    dates <- html_text(html_nodes(x = read_html(page), 
                                  css = ".center:nth-child(4) .floatleft:nth-child(2)"), 
                       trim = TRUE)
    dates <- dmy(dates)
    return(dates)
  }
}

#------------------------------------------------------------------------------#
## Datumsfilter: Jahr der letzten Updates aus der Geschichtenuebersicht, 
## bspw. https://www.fanfiktion.de/One-Punch-Man/c/300000528/1/updatedate 
#------------------------------------------------------------------------------#
getFanfictionUpdateYear <- function(url){  
  
  page <- session_jump_to(session, url)
  
  if(length(html_text(html_nodes(x = read_html(page) , css = ".center:nth-child(4) .floatleft:nth-child(2)"), trim = TRUE)) == 0){
    
    print("NoEntry")
    
  }else{
    
    return(sapply(strsplit(html_text(html_nodes(x = read_html(page), 
                                                css = ".center:nth-child(4) .floatleft:nth-child(2)"), 
                                     trim = TRUE), 
                           "\\."), 
                  "[[", 3))
    
  }
}

#------------------------------------------------------------------------------#
## Datumsfilter: Jahr der letzten Updates aus der Geschichtenuebersicht, 
## bspw. https://www.fanfiktion.de/One-Punch-Man/c/300000528/1/updatedate 
#------------------------------------------------------------------------------#

getFanfictionUpdateMonth <- function(url){  
  
  page <- session_jump_to(session, url)
  
  if(length(html_text(html_nodes(x = read_html(page) , css = ".center:nth-child(4) .floatleft:nth-child(2)"), trim = TRUE)) == 0){
    
    print("NoEntry")
    
  }else{
    
    return(sapply(strsplit(html_text(html_nodes(x = read_html(page), 
                                                css = ".center:nth-child(4) .floatleft:nth-child(2)"), 
                                     trim = TRUE), 
                           "\\."), 
                  "[[", 2))
    
  }
}


getFanfictionUpdateDay <- function(url){  
  
  page <- session_jump_to(session, url)
  
  if(length(html_text(html_nodes(x = read_html(page) , css = ".center:nth-child(4) .floatleft:nth-child(2)"), trim = TRUE)) == 0){
    
    print("NoEntry")
    
  }else{
    
    return(sapply(strsplit(html_text(html_nodes(x = read_html(page), 
                                                css = ".center:nth-child(4) .floatleft:nth-child(2)"), 
                                     trim = TRUE), 
                           "\\."), 
                  "[[", 1))
    
  }
}

#------------------------------------------------------------------------------#
## Komplettes Datum der Erstveroeffentlichung aus der Geschichtenuebersicht, 
## bspw. https://www.fanfiktion.de/One-Punch-Man/c/300000528/1/updatedate 
#------------------------------------------------------------------------------#

getFanfictionCreationDate <- function(url){  
  
  page <- session_jump_to(session, url)
  
  if(length(html_text(html_nodes(x = read_html(page) , css = ".center:nth-child(4) .floatleft:nth-child(1)") , trim = TRUE)) == 0){
    
    print("NoEntry")
    
  }else{
    return(html_text(html_nodes(x = read_html(page), 
                                css = ".center:nth-child(4) .floatleft:nth-child(1)"), 
                     trim = TRUE))
  }
}

#------------------------------------------------------------------------------#
## Datumsfilter: Jahr
#------------------------------------------------------------------------------#
# list.of.dataframes <- urls.df.list 
# which(names(list.of.dataframes) == "101001003")
# browseURL(as.character(list.of.dataframes[[i]]$url[[j]]))

filterByYearMetadata <- function(list.of.dataframes, min.wait, max.wait, year) {
  #year <- 2020 
  #min.wait <- 10
  #max.wait <- 20
  
  fanfiction.update.date <- vector("list", length= length(list.of.dataframes))
  fanfiction.creation.date <- vector("list", length= length(list.of.dataframes))
  fanfiction.id <- vector("list", length= length(list.of.dataframes))
  fanfiction.url <- vector ("list", length= length(list.of.dataframes))
  fanfiction.year <- vector("list", length= length(list.of.dataframes))
  
  author.name <- vector("list", length=  length(list.of.dataframes))
  author.url <- vector("list", length=  length(list.of.dataframes))
  fanfiction.title <- vector("list",  length= length(list.of.dataframes))
  medium <- vector("list", length=  length(list.of.dataframes))
  fandom.categories <- vector("list", length=  length(list.of.dataframes))
  chapter.amount <- vector("list", length=  length(list.of.dataframes))
  story.update.date <- vector("list", length=  length(list.of.dataframes))
  story.creation.date <- vector("list", length=  length(list.of.dataframes))
  wordcount <- vector("list", length=  length(list.of.dataframes))
  genre <- vector("list", length=  length(list.of.dataframes))
  age.restr <- vector("list", length=  length(list.of.dataframes))
  endorsement.amount <- vector("list", length=  length(list.of.dataframes))
  review.amount <- vector("list", length=  length(list.of.dataframes))
  
  
  for(i in 1:length(list.of.dataframes)){
    #i <- 3
    for( j in 1: length(list.of.dataframes[[i]]$url)){
      #j <- 1
      if(length(which(getFanfictionUpdateYear(as.character(list.of.dataframes[[i]]$url[[j]])) != year)) == 0 || length(which(getFanfictionUpdateYear(as.character(list.of.dataframes[[i]]$url[[j]])) != year)) < 20 ) {
        page <- session_jump_to(session, as.character(list.of.dataframes[[i]]$url[[j]])) #### as.character()
        page.html <- read_html(page)
        
        # Datum des letzten Updates
        fanfiction.update.date[[i]][[j]] <- if(length(html_text(html_nodes(x = page.html, css = ".center:nth-child(4) .floatleft:nth-child(2)"), trim = TRUE)) == 0){
          "NoEntry"
        }else{
          html_text(html_nodes(x = read_html(page), 
                               css = ".center:nth-child(4) .floatleft:nth-child(2)"),
                    trim = TRUE)
        }
        
        
        # Erstellungsdatum
        fanfiction.creation.date[[i]][[j]] <-  if(length(html_text(html_nodes(x = page.html, css = ".center:nth-child(4) .floatleft:nth-child(1)") , trim = TRUE)) == 0){
          "NoEntry"
        }else{
          html_text(html_nodes(x = read_html(page),
                               css = ".center:nth-child(4) .floatleft:nth-child(1)"),
                    trim = TRUE)
        }
        
        
        # Fanfiction-ID: 
        cat_title <- html_nodes(x = page.html, 
                                css = ".huge-font a")
        
        fanfiction.id[[i]][[j]] <- if(dim(bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE))))[[1]] ==0){
          "NoEntry"
          
        }else{
          
          df <- bind_rows(lapply(xml_attrs(cat_title), 
                                 function(x) data.frame(as.list(x), 
                                                        stringsAsFactors=FALSE)))
          sapply(strsplit(df$href, "/"), "[[", 3)
        }
        
        # Fanfiction URL   
        fanfiction.url[[i]][[j]] <- if(length(html_text(html_nodes(x = page.html, css = ".huge-font a") , trim = TRUE)) == 0){
          url
        }else{
          cat_title <- html_nodes(x = page.html, css = ".huge-font a")
          df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
          paste0("https://www.fanfiktion.de",df$href)
        }
        
        # Jahr der des letzten Updates 
        fanfiction.year[[i]][[j]] <- sapply(strsplit(fanfiction.update.date[[i]][[j]], "\\."), "[[", 3)
        
        # Autorname
        author.name[[i]][[j]] <- html_text(html_nodes(x =page.html , css = ".padded-vertical-small .no-wrap") , trim = TRUE)
        
        # Autor URL
        cat_title <- html_nodes(x = page.html, css = ".padded-vertical-small .no-wrap")
        df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
        author.url[[i]][[j]] <-  paste0("https://www.fanfiktion.de", df$href)
        
        # Titel
        fanfiction.title[[i]][[j]] <-   html_text(html_nodes(x =page.html , css = ".huge-font a") , trim = TRUE)
        
        # Medium
        medium[[i]][[j]] <- html_text(html_nodes(x = page.html , css = "#ffcbox-dirlist-topic-1 a") , trim = TRUE)[2]
        
        # Fandomkategorien
        categories <- html_text(html_nodes(x =page.html , css = "#ffcbox-dirlist-topic-1 a") , trim = TRUE)
        fandom.categories[[i]][[j]] <- paste(categories[-c(1,2)], collapse = " / ")
        
        # Kapitelanzahl
        chapter.amount[[i]][[j]] <- as.numeric(html_text(html_nodes(x = page.html , css = ".right .semibold") , trim = TRUE))
        
        # Wortanzahl
        wordcount[[i]][[j]] <- as.numeric(html_text(html_nodes(x =page.html, css = ".fa-keyboard+ .semibold") , trim = TRUE), "\\.")
        
        # Genre
        genre[[i]][[j]] <- sapply(strsplit(html_text(html_nodes(x = page.html, css = ".padded-vertical-small+ .center") , trim = TRUE), " / ") , "[[", 1)
        
        # Altersbeschraenkung
        age.restr[[i]][[j]] <- sapply(strsplit(html_text(html_nodes(x = page.html, css = ".padded-vertical-small+ .center") , trim = TRUE), " / ") , "[[", 2)
        
        # Anzahl der Empfehlungen
        endorsement.amount[[i]][[j]] <- as.numeric(html_text(html_nodes(x = page.html , css = ".left") , trim = TRUE))
        for( e in 1:length (endorsement.amount[[i]][[j]])){
          if(is.na(endorsement.amount[[i]][[j]][e]) == TRUE ){
            endorsement.amount[[i]][[j]][e] <- 0
          }
        }
        
        # Anzahl der Reviews
        review.amount[[i]][[j]] <- as.numeric(html_text(html_nodes(x = page.html , css = ".left~ .no-wrap") , trim = TRUE))
        
        print(paste("fertig mit", list.of.dataframes[[i]]$url[[j]], ",", j, "von", length(list.of.dataframes[[i]]$url)))
        
        # Damit Fanfiktion.de nicht ueberlastet wird, bauen wir eine Pause ein
        Sys.sleep(sample(min.wait:max.wait)[1])
        
        
      }else{
        
        break
      }
      
    }
    if(length(fanfiction.url[[i]])!= 0) {
      metadata <- data.frame(unlist(author.name[[i]]), 
                             unlist(author.url[[i]]), 
                             unlist(fanfiction.id[[i]]), 
                             unlist(fanfiction.url[[i]]),
                             unlist(fanfiction.title[[i]]), 
                             rep(unique(unlist(medium[[i]])), times = length(unlist(fanfiction.id[[i]]))),
                             rep(unique(unlist(fandom.categories[[i]])), times = length(unlist(fanfiction.id[[i]]))),
                             unlist(chapter.amount[[i]]),
                             unlist(fanfiction.creation.date[[i]]), 
                             unlist(fanfiction.update.date[[i]]), 
                             unlist(wordcount[[i]]), 
                             unlist(genre[[i]]),
                             unlist(age.restr[[i]]), 
                             unlist(endorsement.amount[[i]]),
                             unlist(review.amount[[i]]), 
                             unlist(fanfiction.year[[i]]),
                             stringsAsFactors = FALSE)
      
      colnames(metadata) <- c("author_username", 
                              "author_url", 
                              "text_id", 
                              "url",
                              "text_title", 
                              "medium_orig", 
                              "fandom_categories",
                              "chapter_amount", 
                              "creation_date", 
                              "update_date",
                              "wordcount", 
                              "genre", 
                              "age_restriction", 
                              "endorsement_amount", 
                              "review_amount", 
                              "year")
      
      result <- metadata[which(metadata$year == year),]
      if(nrow(result) != 0 ){
        write.csv2(result, paste0(wd, "\\Metadaten\\", unique(list.of.dataframes[[i]]$category_id),"_", unique(list.of.dataframes[[i]]$category_title), "_Metadaten.csv"))
      }
    }
    
    print(paste("!!! fertig mit Fandom", unique(list.of.dataframes[[i]]$category_title), ",", i, "von", length(list.of.dataframes), "!!!"))
    Sys.sleep(sample(min.wait:max.wait)[1])
    
  }
  
}


#------------------------------------------------------------------------------#
## Datums filter: Datum
#------------------------------------------------------------------------------#
# list.of.dataframes <- urls.df.list
filterByDateMetadata <- function(list.of.dataframes, min.wait, max.wait, date) {
  # date <- "01.04.2003"
  # min.wait <- 5
  # max.wait <- 8
  # year <- as.numeric(unlist(strsplit(date, "\\."))[[3]])
  # month<- as.numeric(unlist(strsplit(date, "\\."))[[2]])
  # day <- as.numeric(unlist(strsplit(date, "\\."))[[1]])
  date_format <- dmy(date)
  
  
  fanfiction.update.date <- vector("list", length= length(list.of.dataframes))
  fanfiction.creation.date <- vector("list", length= length(list.of.dataframes))
  fanfiction.id <- vector("list", length= length(list.of.dataframes))
  fanfiction.url <- vector ("list", length= length(list.of.dataframes))
  fanfiction.year <- vector("list", length= length(list.of.dataframes))
  fanfiction.month <- vector("list", length= length(list.of.dataframes))
  fanfiction.day <- vector("list", length= length(list.of.dataframes))
  
  
  author.name <- vector("list", length=  length(list.of.dataframes))
  author.url <- vector("list", length=  length(list.of.dataframes))
  fanfiction.title <- vector("list",  length= length(list.of.dataframes))
  medium <- vector("list", length=  length(list.of.dataframes))
  fandom.categories <- vector("list", length=  length(list.of.dataframes))
  chapter.amount <- vector("list", length=  length(list.of.dataframes))
  story.update.date <- vector("list", length=  length(list.of.dataframes))
  story.creation.date <- vector("list", length=  length(list.of.dataframes))
  wordcount <- vector("list", length=  length(list.of.dataframes))
  genre <- vector("list", length=  length(list.of.dataframes))
  age.restr <- vector("list", length=  length(list.of.dataframes))
  endorsement.amount <- vector("list", length=  length(list.of.dataframes))
  review.amount <- vector("list", length=  length(list.of.dataframes))
  
  
  for(i in 1:length(list.of.dataframes)){ #!!! 
    #i <- 1
    for( j in 1:length(list.of.dataframes[[i]]$url)){
      #print(j)
      if(length(which(getFanfictionUpdateDate(as.character(list.of.dataframes[[i]]$url[[j]])) >= date_format)) <= 20 && length(which(getFanfictionUpdateDate(as.character(list.of.dataframes[[i]]$url[[j]])) >= date_format)) > 0 ) {
        #print("page?")
        page <- session_jump_to(session, as.character(list.of.dataframes[[i]]$url[[j]])) 
        
        page.html <- read_html(page)
        
        # Datum des letzten Updates
        fanfiction.update.date[[i]][[j]] <- if(length(html_text(html_nodes(x = page.html, css = ".center:nth-child(4) .floatleft:nth-child(2)"), trim = TRUE)) == 0){
          "NoEntry"
        }else{
          dmy(html_text(html_nodes(x = read_html(page), css = ".center:nth-child(4) .floatleft:nth-child(2)"),
                        trim = TRUE))
        }
        
        # Erstellungsdatum
        fanfiction.creation.date[[i]][[j]] <-  if(length(html_text(html_nodes(x = page.html, css = ".center:nth-child(4) .floatleft:nth-child(1)") , trim = TRUE)) == 0){
          "NoEntry"
        }else{
          dmy(html_text(html_nodes(x = read_html(page),
                                   css = ".center:nth-child(4) .floatleft:nth-child(1)"),
                        trim = TRUE))
        }
        
        # Fanfiction-ID: 
        cat_title <- html_nodes(x = page.html, 
                                css = ".huge-font a")
        
        fanfiction.id[[i]][[j]] <- if(dim(bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE))))[[1]] ==0){
          "NoEntry"
          
        }else{
          
          df <- bind_rows(lapply(xml_attrs(cat_title), 
                                 function(x) data.frame(as.list(x), 
                                                        stringsAsFactors=FALSE)))
          sapply(strsplit(df$href, "/"), "[[", 3)
        }
        
        # Fanfiction URL   
        fanfiction.url[[i]][[j]] <- if(length(html_text(html_nodes(x = page.html, css = ".huge-font a") , trim = TRUE)) == 0){
          url
        }else{
          cat_title <- html_nodes(x = page.html, css = ".huge-font a")
          df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
          paste0("https://www.fanfiktion.de",df$href)
        }
        
        # Autorname
        author.name[[i]][[j]] <- html_text(html_nodes(x =page.html , css = ".padded-vertical-small .no-wrap") , trim = TRUE)
        
        # Autor URL
        cat_title <- html_nodes(x = page.html, css = ".padded-vertical-small .no-wrap")
        df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
        author.url[[i]][[j]] <-  paste0("https://www.fanfiktion.de", df$href)
        
        # Titel
        fanfiction.title[[i]][[j]] <-   html_text(html_nodes(x =page.html , css = ".huge-font a") , trim = TRUE)
        
        # Medium
        medium[[i]][[j]] <- html_text(html_nodes(x = page.html , css = "#ffcbox-dirlist-topic-1 a") , trim = TRUE)[2]
        
        # Fandomkategorien
        categories <- html_text(html_nodes(x =page.html , css = "#ffcbox-dirlist-topic-1 a") , trim = TRUE)
        fandom.categories[[i]][[j]] <- paste(categories[-c(1,2)], collapse = " / ")
        
        # Kapitelanzahl
        chapter.amount[[i]][[j]] <- as.numeric(html_text(html_nodes(x = page.html , css = ".right .semibold") , trim = TRUE))
        
        # Wortanzahl
        wordcount[[i]][[j]] <- as.numeric(html_text(html_nodes(x =page.html, css = ".fa-keyboard+ .semibold") , trim = TRUE), "\\.")
        
        # Genre
        genre[[i]][[j]] <- sapply(strsplit(html_text(html_nodes(x = page.html, css = ".padded-vertical-small+ .center") , trim = TRUE), " / ") , "[[", 1)
        
        # Altersbeschraenkung
        age.restr[[i]][[j]] <- sapply(strsplit(html_text(html_nodes(x = page.html, css = ".padded-vertical-small+ .center") , trim = TRUE), " / ") , "[[", 2)
        
        # Anzahl der Empfehlungen
        endorsement.amount[[i]][[j]] <- as.numeric(html_text(html_nodes(x = page.html , css = ".left") , trim = TRUE))
        for( e in 1:length (endorsement.amount[[i]][[j]])){
          if(is.na(endorsement.amount[[i]][[j]][e]) == TRUE ){
            endorsement.amount[[i]][[j]][e] <- 0
          }
        }
        
        # Anzahl der Reviews
        review.amount[[i]][[j]] <- as.numeric(html_text(html_nodes(x = page.html , css = ".left~ .no-wrap") , trim = TRUE))
        
        print(paste("fertig mit", list.of.dataframes[[i]]$url[[j]], ",", j, "von", length(list.of.dataframes[[i]]$url)))
        
        # Damit Fanfiktion.de nicht ueberlastet wird, bauen wir eine Pause ein
        Sys.sleep(sample(min.wait:max.wait)[1])
        
        
      }else{
        
        break
      }
    }
    
    #print("HERE")
    
    if(length(fanfiction.url[[i]])!= 0) {
      metadata <- data.frame(unlist(author.name[[i]]), 
                             unlist(author.url[[i]]), 
                             unlist(fanfiction.id[[i]]), 
                             unlist(fanfiction.url[[i]]),
                             unlist(fanfiction.title[[i]]), 
                             rep(unique(unlist(medium[[i]])), times = length(unlist(fanfiction.id[[i]]))),
                             rep(unique(unlist(fandom.categories[[i]])), times = length(unlist(fanfiction.id[[i]]))),
                             unlist(chapter.amount[[i]]),
                             do.call("c", fanfiction.creation.date[[i]]), 
                             do.call("c", fanfiction.update.date[[i]]), 
                             unlist(wordcount[[i]]), 
                             unlist(genre[[i]]),
                             unlist(age.restr[[i]]), 
                             unlist(endorsement.amount[[i]]),
                             unlist(review.amount[[i]]), 
                             stringsAsFactors = FALSE)
      
      #View(metadata)
      colnames(metadata) <- c("author_username", 
                              "author_url", 
                              "text_id", 
                              "url",
                              "text_title", 
                              "medium_orig", 
                              "fandom_categories",
                              "chapter_amount", 
                              "creation_date", 
                              "update_date",
                              "wordcount", 
                              "genre", 
                              "age_restriction", 
                              "endorsement_amount", 
                              "review_amount")
      
      metadata_filtered <- metadata[metadata$update_date >= date_format,  ]
      
      #View(metadata_filtered)
      if(nrow(metadata_filtered) != 0 ){
        # Falls mehrere verschiedene category_titles exisitieren (weil z.B. ein Fandom umbenannt wurde) wird der erste Titel verwendet
        write.csv2(metadata_filtered, paste0(wd, "\\Metadaten\\", unique(list.of.dataframes[[i]]$category_id),"_", unique(list.of.dataframes[[i]]$category_title)[1], "_Metadaten.csv"))
      }else{
        print(paste("!!! fertig mit Fandom", unique(list.of.dataframes[[i]]$category_title), ",", i, "von", length(list.of.dataframes), ". Keine Updates. !!!"))
      }
    }
    
    print(paste("!!!!!!! fertig mit Datei", unique(list.of.dataframes[[i]]$category_title), ",", i, "von", length(list.of.dataframes), "!!!"))
    Sys.sleep(sample(min.wait:max.wait)[1])
    
  }
  
}

#------------------------------------------------------------------------------#
## Metadatentabelle fuer einen textid-Vektor. Z.b. 
## textids
#------------------------------------------------------------------------------#

#charactervector_textids <- missing_metadata [1:10]

metadata_for_textidvector <- function(charactervector_textids, min.wait, max.wait) {
  # min.wait <- 5
  # max.wait <- 8
  author_username <- vector("list", length=  length(charactervector_textids))
  author_url <- vector("list", length=  length(charactervector_textids))
  text_title <- vector("list",  length= length(charactervector_textids))
  medium_orig <- vector("list", length=  length(charactervector_textids))
  fandom_categories <- vector("list", length=  length(charactervector_textids))
  chapter_amount <- vector("list", length=  length(charactervector_textids))
  creation_date <- vector("list", length=  length(charactervector_textids))
  update_date <- vector("list", length=  length(charactervector_textids))
  wordcount <- vector("list", length=  length(charactervector_textids))
  genre <- vector("list", length=  length(charactervector_textids))
  age_restriction <- vector("list", length=  length(charactervector_textids))
  endorsement_amount <- vector("list", length=  length(charactervector_textids))
  review_amount <- vector("list", length=  length(charactervector_textids))
  fandom_code <- vector("list", length=  length(charactervector_textids))
  
  urls <- paste0("https://www.fanfiktion.de/s/", charactervector_textids)
  
  
  for( u in 1:length(urls)){
    if(GET(urls[[u]])$status_code == 200){
    
    # u <- 2
    page <- tryCatch(session_jump_to(session, as.character(urls[[u]])))
    page.html <- read_html(page)
    
    # Autorname
    author_username[[u]] <- html_text(html_nodes(x =page.html , css = ".center.small-font .no-wrap") , trim = TRUE)
    
    # Autor URL
    cat_title <- html_nodes(x = page.html, css = ".center.small-font .no-wrap")
    df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    author_url[[u]] <-  paste0("https://www.fanfiktion.de", df$href)
    
    # Titel
    text_title[[u]] <-  html_text(html_nodes(x =page.html , css = ".huge-font") , trim = TRUE)
    
    # Medium
    medium_orig[[u]] <- html_text(html_nodes(x = page.html , css = "#ffcbox-story-topic-1 a") , trim = TRUE)[2]
    
    # Fandomkategorien
    categories <- html_text(html_nodes(x =page.html , css = "#ffcbox-story-topic-1 a") , trim = TRUE)
    fandom_categories[[u]] <- paste(categories[-c(1,2, length(categories))], collapse = " / ")
    
    # Kapitelanzahl
    chapter_amount[[u]] <- as.numeric(html_text(html_nodes(x = page.html , css = ".fa-file-alt+ .semibold") , trim = TRUE))
    
    
    # Erstellungsdatum
    creation_date[[u]] <-  if(length(html_text(html_nodes(x = page.html, css = ".block:nth-child(7) div:nth-child(1)") , trim = TRUE)) == 0){
      "NoEntry"
    }else{
      html_text(html_nodes(x = read_html(page),
                               css = ".block:nth-child(7) div:nth-child(1)"),
                    trim = TRUE)
    }
    
    # Datum des letzten Updates
    update_date[[u]] <- if(length(html_text(html_nodes(x = page.html, css = ".block:nth-child(7) div+ div"), trim = TRUE)) == 0){
      "NoEntry"
    }else{
      html_text(html_nodes(x = read_html(page), css = ".block:nth-child(7) div+ div"),
                    trim = TRUE)
    }
    
    
    # Wortanzahl
    wordcount[[u]] <- as.numeric(html_text(html_nodes(x =page.html, css = ".fa-keyboard+ .semibold") , trim = TRUE), "\\.")
    
    # Genre
    genre[[u]] <- sapply(strsplit(html_text(html_nodes(x = page.html, css = ".center.block") , trim = TRUE), " / ") , "[[", 1)
    
    # Altersbeschraenkung
    age_restriction[[u]] <- sapply(strsplit(html_text(html_nodes(x = page.html, css = ".center.block") , trim = TRUE), " / ") , "[[", 2)
    
    # Anzahl der Empfehlungen
    endorsement_amount[[u]] <- as.numeric(html_text(html_nodes(x = page.html , css = ".no-wrap~ .no-wrap+ .no-wrap") , trim = TRUE))
    if(is.na(endorsement_amount[[u]]) == TRUE ){
        endorsement_amount[[u]] <- 0
      }
    
    # Anzahl der Reviews
    review_amount[[u]] <-   as.numeric(unlist(strsplit(gsub(pattern="Alle Kapitel", replacement ="",x = html_text(html_nodes(x = page.html , css = ".base-line-height.block") , trim = TRUE)), " Reviews")))
    if(is.na(review_amount[[u]]) == TRUE ){
      review_amount[[u]] <- 0
    }
    

    # Anzahl der Reviews
    cat_title <- html_nodes(x = page.html , css = "#ffcbox-story-topic-1 a") [length(html_text(html_nodes(x = page.html , css = "#ffcbox-story-topic-1 a") , trim = TRUE))-1]
    df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    fandom_code[[u]] <-   sapply(strsplit(sapply(strsplit(df$href, "/c/"), "[[", 2), "/1/"), "[[", 1)

    Sys.sleep(sample(min.wait:max.wait)[1])
    }else{
      author_username[[u]] <- "information_deleted" 
      author_url[[u]] <-"information_deleted" 
      text_title[[u]] <-"information_deleted" 
      medium_orig[[u]] <-"information_deleted" 
      fandom_categories[[u]] <-"information_deleted" 
      chapter_amount[[u]] <-"information_deleted" 
      creation_date[[u]] <-"information_deleted" 
      update_date[[u]] <-"information_deleted" 
      wordcount[[u]] <-"information_deleted" 
      genre[[u]] <-"information_deleted" 
      age_restriction[[u]] <-"information_deleted" 
      endorsement_amount[[u]] <-"information_deleted" 
      review_amount[[u]] <-"information_deleted" 
      fandom_code[[u]] <-"information_deleted" 
      
    }
    
    print(paste("fertig mit URL No. ", u))
          
    
  }

  metadatatable <- data.frame(unlist(author_username), 
                             unlist(author_url), 
                             charactervector_textids, 
                             urls,
                             unlist(text_title), 
                             unlist(medium_orig), 
                             unlist(fandom_categories), 
                             unlist(chapter_amount),
                             unlist(creation_date), 
                             unlist(update_date), 
                             unlist(wordcount), 
                             unlist(genre),
                             unlist(age_restriction), 
                             unlist(endorsement_amount),
                             unlist(review_amount),
                             unlist(fandom_code),
                             stringsAsFactors = FALSE)
      
      #View(metadatatable)
      colnames(metadatatable) <- c("author_username", 
                              "author_url", 
                              "text_id", 
                              "url",
                              "text_title", 
                              "medium_orig", 
                              "fandom_categories",
                              "chapter_amount", 
                              "creation_date", 
                              "update_date",
                              "wordcount", 
                              "genre", 
                              "age_restriction", 
                              "endorsement_amount", 
                              "review_amount")
      
      return(metadatatable)

  }
  



#------------------------------------------------------------------------------#
## FanfictionID aus der Geschichtenuebersicht, 
## bspw. https://www.fanfiktion.de/One-Punch-Man/c/300000528/1/updatedate 
#------------------------------------------------------------------------------#

getFanfictionID <- function(url){  
  
  page <- session_jump_to(session, url)
  
  cat_title <- html_nodes(x = read_html(page), 
                          css = ".huge-font a") 
  
  if(dim(bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE))))[[1]] ==0){
    
    print("NoEntry")
    
  }else{
    
    df <- bind_rows(lapply(xml_attrs(cat_title), 
                           function(x) data.frame(as.list(x), 
                                                  stringsAsFactors=FALSE)))
    
    return(sapply(strsplit(df$href, 
                           "/"), 
                  "[[", 3))}
}


#------------------------------------------------------------------------------#
# Fanfictiontitel aus der Geschichtenuebersicht, 
## bspw. https://www.fanfiktion.de/Anime-Manga/c/102000000 
#------------------------------------------------------------------------------#

getFanfictionTitleFromOverview <- function(url){  
  page <- session_jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".huge-font a") , trim = TRUE)
  
}
#------------------------------------------------------------------------------#
# Fanfictiontitel aus der Geschichtenuebersicht extrahieren,
# bspw. https://www.fanfiktion.de/One-Punch-Man/c/300000528/1/updatedate
#------------------------------------------------------------------------------#

getFanfictionTitleFromStorypage <- function(url){  
  page <- session_jump_to(session, url)
  html_text(html_nodes(x = read_html(page) , css = ".huge-font") , trim = TRUE)
  
}


#------------------------------------------------------------------------------#
# FanfictionURL aus der Geschichtenuebersicht extrahieren, 
# bspw. https://www.fanfiktion.de/One-Punch-Man/c/300000528/1/updatedate 
#------------------------------------------------------------------------------#
getFanfictionURL <- function(url){  
  page <- session_jump_to(session, url)
  if(length(html_text(html_nodes(x = read_html(page) , css = ".huge-font a") , trim = TRUE)) == 0){
    print(url)
  }else{  
    cat_title <- html_nodes(x = read_html(page), css = ".huge-font a") 
    df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
    return(paste0("https://www.fanfiktion.de",df$href))
  }
}



#------------------------------------------------------------------------------#
# Autorname aus der Geschichtenansicht extrahieren, 
# bspw. https://www.fanfiktion.de/s/5e37603200025f05230e6a25/1/Sein-Leben-An-Ihrer-Seite
#------------------------------------------------------------------------------#
getAuthorNameFromStorypage <- function(url){  
  page <- session_jump_to(session, url)
  return(html_text(html_nodes(x = read_html(page) , css = ".center.small-font .no-wrap") , trim = TRUE))
}

getAuthorUrlFromStorypage <- function(url){  
  page <- session_jump_to(session, url)
  cat_title <- html_nodes(x = read_html(page), css = ".center.small-font .no-wrap") 
  df <- bind_rows(lapply(xml_attrs(cat_title), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
  return(paste0("https://www.fanfiktion.de", df$href))
  
}


#------------------------------------------------------------------------------#
# Kategorien aus dem Header in der Geschichtenansicht (Fandom und Medium) extrahieren
# bspw. https://www.fanfiktion.de/s/5e37603200025f05230e6a25/1/Sein-Leben-An-Ihrer-Seite
#------------------------------------------------------------------------------#
getMediumFromStorypage <- function(url){  
  page <- session_jump_to(session, url)
  categories <- html_text(html_nodes(x = read_html(page) , css = "#ffcbox-story-topic-1 a") , trim = TRUE)
  return(categories[2]) # 1 ist immer "Fanfiction", die letzte Kategorie ist der Titel 
}


getFandomCategoriesFromStorypage <- function(url){  
  page <- session_jump_to(session, url)
  categories <- html_text(html_nodes(x = read_html(page) , css = "#ffcbox-story-topic-1 a") , trim = TRUE)
  return(paste(categories[-c(1,2, length(categories))], collapse = " / ")) # 1 ist immer "Fanfiction", die letzte Kategorie ist der Titel 
}

# Kapitelanzahl aus dem Header in der Geschichtenansicht extrahieren
# bspw. https://www.fanfiktion.de/s/5e37603200025f05230e6a25/1/Sein-Leben-An-Ihrer-Seite
url <- "https://www.fanfiktion.de/s/5e37603200025f05230e6a25/1/Sein-Leben-An-Ihrer-Seite"

getChapterAmountFromStorypage <- function(url){  
  page <- session_jump_to(session, url)
  return(as.numeric(html_text(html_nodes(x = read_html(page) , css = ".fa-file-alt+ .semibold") , trim = TRUE)))
}


#------------------------------------------------------------------------------#
# Entstehungsdatum 
#------------------------------------------------------------------------------#
getStoryCreationDateFromStorypage <- function(url){  
  page <- session_jump_to(session,url)
  return(substr(html_text(html_nodes(x = read_html(page) , css = ".block") , trim = TRUE)[3], 1,10))
}

#------------------------------------------------------------------------------#
# Entstehungsdatum 
#------------------------------------------------------------------------------#
getStoryUpdateDateFromStorypage <- function(url){  
  page <- session_jump_to(session,url)
  x <- html_text(html_nodes(x = read_html(page) , css = ".block") , trim = TRUE)[3]
  return(substr(x, nchar(x)-9, nchar(x)))
}

#------------------------------------------------------------------------------#
# Wortanzahl  
#------------------------------------------------------------------------------#
getWordcountFromStorypage <- function(url){  
  page <- session_jump_to(session,url)
  return(as.numeric(paste(unlist(strsplit(html_text(html_nodes(x = read_html(page), 
                                                               css = ".fa-keyboard+ .semibold"), 
                                                    trim = TRUE), 
                                          "\\.")), 
                          collapse = "")))
}

#------------------------------------------------------------------------------#
# Typ (Geschichte, Aufz?hlung, )  
#------------------------------------------------------------------------------#
getStorytypeFromStorypage <- function(url){  
  page <- session_jump_to(session,url)
  return(unlist(strsplit(html_text(html_nodes(x = read_html(page), 
                                              css = ".center.block"), 
                                   trim = TRUE), 
                         " / "))[1])
}


#------------------------------------------------------------------------------#
# Genre  
#------------------------------------------------------------------------------#
getGenreFromStorypage <- function(url){  
  page <- session_jump_to(session,url)
  return(unlist(strsplit(html_text(html_nodes(x = read_html(page), 
                                              css = ".center.block"), 
                                   trim = TRUE), 
                         " / "))[1])
}

#------------------------------------------------------------------------------#
# Altersbeschraenkung
#------------------------------------------------------------------------------#
getAgeRestrFromStorypage <- function(url){  
  page <- session_jump_to(session,url)
  return(  unlist(strsplit(html_text(html_nodes(x = read_html(page), 
                                                css = ".center.block"), 
                                     trim = TRUE), 
                           " / "))[2])
}


#------------------------------------------------------------------------------#
# Anzahl der Empfehlungen
#------------------------------------------------------------------------------#

getEndorsementAmountFromStorypage <- function(url){
  page <- session_jump_to(session, url)
  if(is.na(as.numeric(html_text(html_nodes(x = read_html(page) , css = ".no-wrap~ .no-wrap+ .no-wrap") , trim = TRUE)))==F){
    return(as.numeric(html_text(html_nodes(x = read_html(page) , css = ".no-wrap~ .no-wrap+ .no-wrap") , trim = TRUE)))
  }else{
    return(0)
  }
}

#------------------------------------------------------------------------------#
# Reviewanzahl 
#------------------------------------------------------------------------------#

getReviewAmountFromStorypage <- function(url){  
  page <- session_jump_to(session,url)
  if( length( html_text(html_nodes(x = read_html(page) , css = ".block a") , trim = TRUE)) != 0){
    return(as.numeric(unlist(strsplit(html_text(html_nodes(x = read_html(page) , css = ".block a") , trim = TRUE), " "))[1]))  
  }else{
    return(0)
  }
}

#------------------------------------------------------------------------------#
# Ueberpruefung: ist eine Geschichte als entwicklungsbeeintraechtigend eingestuft? 
#------------------------------------------------------------------------------#

isRestricted <- function(url){ 
  if(length(html_text(html_nodes(x = read_html(tryCatch(session_jump_to(session, url))) , css = ".story-left") , trim = TRUE))!= 1){
    return(TRUE)
  }else{
    return(FALSE)
  }
  
}

#------------------------------------------------------------------------------#
getTextFromStorypage <- function(url){  
  page <- session_jump_to(session,url)
  return(html_text(html_nodes(x = read_html(page) , css = ".user-formatted-inner") , trim = FALSE))
}

#------------------------------------------------------------------------------#
# Gibt es die URL ? (https://stackoverflow.com/a/60627969)
#------------------------------------------------------------------------------#

valid_url <- function(url_in,t=2){
  con <- url(url_in)
  check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
  suppressWarnings(try(close.connection(con),silent=T))
  ifelse(is.null(check),TRUE,FALSE)
}
#------------------------------------------------------------------------------#
# Texte 
#------------------------------------------------------------------------------#

#dataframe <- urls

createTextTable <- function(dataframe, min.wait, max.wait){
  # min.wait <- 9 
  # max.wait <- 11
  df <- vector("list", length =nrow(dataframe))
  for(r in 1:nrow(dataframe)){
    # r <- 1
    existing_fanfictions <- sapply(strsplit(list.files (paste0(wd, "\\Texte"), pattern = ".csv"), "_"), "[[", 1)
    
    if( dataframe$text_id[[r]] %in% existing_fanfictions){
      print(paste0("Geschichte '",dataframe$text_title[[r]]  , "' mit Text-id ",dataframe$text_id[[r]], " wurde schon gescrapet."))
      next
    }else{
      if(valid_url(dataframe$url[[r]]) == FALSE){
        df[[r]] <- data.frame (1, "URL existiert nicht mehr")
        colnames(df[[r]])<- c("chapter","text")
        rownames(df[[r]])<- NULL
        enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(removePunctuation(dataframe$text_title[[r]]), " ")), collapse = "-"), ".csv") ,encoding="UTF-8")
        write.csv2(df[[r]], file =enc )
        print(paste("URL EXISTIERT NICHT, Geschichte Nr.", r, "von", nrow(dataframe), "abgespeichert"))
      }else{
        if(is.na(as.numeric(dataframe$chapter_amount[[r]])) == FALSE){
          if(dataframe$chapter_amount[[r]] != 1){
            z <-   strsplit(dataframe$url[[r]], "/1/")
            if(isRestricted(dataframe$url[[r]]) == FALSE){      # hier wird die Anzahl der Kapitel neu gescrapet, wenn die Geschichte zug?nglich ist.
              urls_ch <- paste0(z[[1]][1], "/", 1:getChapterAmountFromStorypage(dataframe$url[[r]]),"/", z[[1]][2]) # Wenn nicht, dann wird sie spaeter als nicht zugaenglich aussortiert.
            }else{
              urls_ch <- paste0(z[[1]][1], "/", 1:dataframe$chapter_amount[[r]],"/", z[[1]][2])
            }
          }else{
            urls_ch <- dataframe$url[[r]]
          }
          if(isRestricted(urls_ch[[1]]) == FALSE){
            if (length(urls_ch) == 1){
              text <- getTextFromStorypage(urls_ch[[1]])
            }else{
              text <- list()
              for( i in 1:length(urls_ch)){
                # i <- 111
                text [[i]] <- getTextFromStorypage(urls_ch[[i]])
                Sys.sleep(sample(min.wait:max.wait)[1])
                print(paste("Geschichte Nr.", r, "von", nrow(dataframe), ";  ", "Kapitel Nr.", i, "von", length(urls_ch), "gescrapet"))
              }
              text <- trimws(unlist(text))
            }
            df[[r]] <- data.frame (1:length(text), text, stringsAsFactors = FALSE)
            colnames(df[[r]])<- c("chapter","text")
            rownames(df[[r]])<- NULL
            #class(df[[r]]$text)
            if(length(unlist(strsplit(removePunctuation(as.character(dataframe$text_title[[r]])), " "))) > 7){
              if(validUTF8(dataframe$text_title[[r]]) == TRUE){
                # !!! NEU: as.character um dataframe$text_title[[r]]), damit Zahlen umgewandelt werden k?nnen
                enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(removePunctuation(as.character(dataframe$text_title[[r]])), " "))[1:7], collapse = "-"), ".csv") ,encoding="UTF-8")
                write.csv2(df[[r]], file =enc )
              }else{
                enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(unlist(strsplit(dataframe$url[[r]], "/"))[7], "-"))[1:7], collapse= "-"), ".csv") ,encoding="UTF-8")
                write.csv2(df[[r]], file =enc )
              }
            }else{
              # !!! NEU: as.character um dataframe$text_title[[r]]), damit Zahlen umgewandelt werden k?nnen
              enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(removePunctuation(as.character(dataframe$text_title[[r]])), " ")), collapse = "-"), ".csv") ,encoding="UTF-8")
              write.csv2(df[[r]], file =enc )
            }
            print(paste("Dataframe mit Geschichte Nr.", r, "von", nrow(dataframe), "abgespeichert"))
          }else{
            df[[r]] <- data.frame (1, "Geschichte ist nicht zugaenglich")
            colnames(df[[r]])<- c("chapter","text")
            rownames(df[[r]])<- NULL
            if(length(unlist(strsplit(removePunctuation(as.character(dataframe$text_title[[r]])), " "))) > 7){
              if(validUTF8(dataframe$text_title[[r]]) == TRUE){
                enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(removePunctuation(dataframe$text_title[[r]]), " "))[1:7], collapse = "-"), ".csv") ,encoding="UTF-8")
                write.csv2(df[[r]], file =enc )
              }else{
                enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(unlist(strsplit(dataframe$url[[r]], "/"))[7], "-"))[1:7], collapse= "-"), ".csv") ,encoding="UTF-8")
                write.csv2(df[[r]], file =enc )
              }
            }else{
              enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(removePunctuation(dataframe$text_title[[r]]), " ")), collapse = "-"), ".csv") ,encoding="UTF-8")
              write.csv2(df[[r]], file =enc )
            }
            print(paste("Geschichte Nr.", r, "von", nrow(dataframe), "ist gerade nicht zugaenglich!"))
          }
          if(valid_url(urls_ch[[1]]) == FALSE) {
            df[[r]] <- data.frame (1, "URL existiert nicht mehr")
            colnames(df[[r]])<- c("chapter","text")
            rownames(df[[r]])<- NULL
            
            if(length(unlist(strsplit(removePunctuation(as.character(dataframe$text_title[[r]])), " "))) > 7){
              if(validUTF8(dataframe$text_title[[r]]) == TRUE){
                enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(removePunctuation(dataframe$text_title[[r]]), " "))[1:7], collapse = "-"), ".csv") ,encoding="UTF-8")
                write.csv2(df[[r]], file =enc )
              }else{
                enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(unlist(strsplit(dataframe$url[[r]], "/"))[7], "-"))[1:7], collapse= "-"), ".csv") ,encoding="UTF-8")
                write.csv2(df[[r]], file =enc )
              }
            }else{
              enc <- file(paste0(wd, "\\Texte\\", dataframe$text_id[[r]], "_", paste(unlist(strsplit(removePunctuation(dataframe$text_title[[r]]), " ")), collapse = "-"), ".csv") ,encoding="UTF-8")
              write.csv2(df[[r]], file =enc )
            }
            print(paste("URL EXISTIERT NICHT, Geschichte Nr.", r, "von", nrow(dataframe), "abgespeichert"))
          }
        }
      }
    }
  }
}



#------------------------------------------------------------------------------#
### Create Review Table 
#------------------------------------------------------------------------------#
#table_with_rev <- metadata_w_reviews


createReviewTable <- function(table_with_rev, min.wait, max.wait){
  #min.wait <- 9
  #max.wait <- 12
  for( i in 1: nrow(table_with_rev)){
    #i <-1
    
    existing_reviewtables <- sapply(strsplit(list.files (paste0(wd, "\\Reviews"), pattern = ".csv"), "_"), "[[", 1)
    if( table_with_rev$text_id[[i]] %in% existing_reviewtables){
      print(paste0("Reviews zu Geschichte '",table_with_rev$text_title[[i]]  , "' mit Text-id ",table_with_rev$text_id[[i]], " wurden schon gescrapet."))
      next
    }else{
      textid <- table_with_rev$text_id[[i]]
      review_amount <- table_with_rev$review_amount[[i]]
      pageamount_reviews <- round_any(review_amount/25, 1, f = ceiling)
      review_urls <- paste0("https://www.fanfiktion.de/r/s/", textid, "/date/0/", 1:pageamount_reviews)
      
      reviewtext <- vector ("list", length = length(review_urls))
      review_reply_scraped <- vector ("list",length = length(review_urls))
      review_reply_final <- vector ("list",length = length(review_urls))
      reviewdate <- vector ("list", length = length(review_urls))
      zu_kapitel <- vector ("list", length = length(review_urls))
      reviewer_username <- vector ("list", length = length(review_urls))
      
      for( j in 1:length(review_urls)){
        # j <- 1
        # browseURL(review_urls[[j]])
        
        if(GET(review_urls[[j]])$status_code != 200){
        reviewtext [[j]] <- "url_non_existent"
        reviewer_username [[j]] <- "url_non_existent"
        review_reply_final [[j]] <- "url_non_existent"
        reviewdate [[j]] <- "url_non_existent"
        zu_kapitel [[j]] <- "url_non_existent"
        }else{
          page <- session_jump_to(session, as.character(review_urls[[j]])) #### as.character()
          page.html <- read_html(page)
          review_reply_scraped[[j]] <- if(length(html_text(html_nodes(x = page.html, css = ".review-reply-text") , trim = TRUE)) !=0){
            x <- html_text(html_nodes(x = page.html, css = ".review-reply-text") , trim = TRUE)
            reviewtext[[j]] <-   html_text(html_nodes(x = page.html, css = ".user-formatted-inner") , trim = TRUE)
            review_reply_final[[j]] <- rep(0, length.out = length (reviewtext[[j]]))
            story_author <- html_text(html_nodes(x = page.html, css = ".block > .no-wrap") , trim = TRUE)
            for(k in 1:length(which(grepl(pattern = paste0("\n\t\t\t\t\t\t\t\n\t\n\\W*((?i)Antwort von(?-i))\\W*", story_author), reviewtext[[j]]) == TRUE))) {
              # k <- 1
              review_reply_final[[j]][which(grepl(pattern = paste0("\n\t\t\t\t\t\t\t\n\t\n\\W*((?i)Antwort von(?-i))\\W*", story_author), reviewtext[[j]]) == TRUE)[k]] <- x[[k]]
              
            }
            reviewtext[[j]] <-   sapply(strsplit( reviewtext[[j]], "\n\t\t\t\t\t\t\t\n\t\n\\W*((?i)Antwort von(?-i))\\W*"), "[[", 1)
          }else{
            reviewtext[[j]] <-   html_text(html_nodes(x = page.html, css = ".user-formatted-inner") , trim = TRUE)
            review_reply_final[[j]] <- rep(0, length.out = length (reviewtext[[j]]))
          }
          reviewdate[[j]] <- html_text(html_nodes(x = page.html, css = ".block div:nth-child(2)") , trim = TRUE)
          zu_kapitel[[j]] <- html_text(html_nodes(x = page.html , css = "i") , trim = TRUE)
          reviewer_username[[j]] <- html_text(html_nodes(x = page.html, css = ".block div:nth-child(1)"), trim = TRUE)
          Sys.sleep(sample(min.wait:max.wait)[1])
          print(paste("fertig mit Review-URL:", review_urls[[j]], ", URL nr.", j, "von", length(review_urls) ))
        }
      }
      
      reviews_with_metadata <- data.frame(unlist(reviewer_username),
                                          unlist(zu_kapitel),
                                          unlist(reviewdate),
                                          unlist(reviewtext), 
                                          unlist(review_reply_final),
                                          stringsAsFactors = FALSE)
      colnames(reviews_with_metadata) <- c("reviewer_username", "chapter_reviewed", "review_date",
                                           "review_text", "review_reply")
      #View(reviews_with_metadata)
      if(length(unlist(strsplit(removePunctuation(as.character(table_with_rev$text_title[[i]])), " "))) > 7){
        if(validUTF8(table_with_rev$text_title[[i]]) == TRUE){
          write.csv2(reviews_with_metadata,
                     paste0(wd, "\\Reviews\\",
                            textid, "_",
                            paste(unlist(strsplit(removePunctuation(as.character(table_with_rev$text_title[[i]])), " "))[1:7], collapse = "-"),
                            "_Reviews.csv" ))
        }else{
          write.csv2(reviews_with_metadata,
                     paste0(wd, "\\Reviews\\",
                            textid, "_",
                            paste(unlist(strsplit(unlist(strsplit(table_with_rev$url[[i]], "/"))[7], "-"))[1:7], collapse= "-"),
                            "_Reviews.csv" ))
        }
        
      }else{
        write.csv2(reviews_with_metadata, paste0(wd, "\\Reviews\\", textid, "_", paste(unlist(strsplit(removePunctuation(as.character(table_with_rev$text_title[[i]])), " ")), collapse = "-"), "_Reviews.csv" ))
      }
      print(paste("!!! fertig mit Reviews zu Text:", table_with_rev$text_title[[i]], "; Nr.", i, "von", nrow(table_with_rev)))
    } 
  }
}


#------------------------------------------------------------------------------#
### AutorInneninformationen Scrapen
#------------------------------------------------------------------------------#
#table_with_rev <- metadata_w_reviews

# vector_of_urls <- author_pages

createAuthorInfoTable <- function(vector_of_urls, min.wait, max.wait, save.after){
  #min.wait <- 5
  #max.wait <- 6
  if(missing(save.after)) {
    save.after <- 500
  } 
  if(missing(min.wait)) {
    min.wait <- 10
  } 
  if(missing(max.wait)) {
    max.wait <- 15
  } 
  
  author_infos <- vector("list", length= length(vector_of_urls))
  for ( u in 1: length(vector_of_urls)){
    # u <- 1
    if(GET(vector_of_urls[[u]])$status_code != 200){
      author_username <- "url_non_existent"
      first_name <- "url_non_existent"
      last_name <- "url_non_existent"
      living_in <- "url_non_existent"
      homepage <- "url_non_existent"
      country <- "url_non_existent"
      gender <- "url_non_existent"
      about <- "url_non_existent"
      age <- "url_non_existent"
      
    }else{
      page <- session_jump_to(session, as.character(vector_of_urls[u]))
      page.html <- read_html(page)
      
      if(length(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)) == 0){
        author_username <- paste(unlist(strsplit(unlist(strsplit(vector_of_urls[u], "/"))[length(unlist(strsplit(vector_of_urls[u], "/")))], "-")), collapse = " ")
        if(length(unlist(strsplit(author_username, "\\+"))) != 1){
          author_username <- paste(unlist(strsplit(author_username, "\\+")), collapse = " ")
        }
        
        if(length(html_text(html_nodes(x = page.html, css = ".caption") , trim = TRUE)) !=0 && length(html_text(html_nodes(x = page.html, css = ".caption") , trim = TRUE)) ==  "Benutzer wurde gesperrt"){
          first_name <- "Benutzer wurde gesperrt"
          last_name <- "Benutzer wurde gesperrt"
          living_in <- "Benutzer wurde gesperrt"
          homepage <-"Benutzer wurde gesperrt"
          country <-"Benutzer wurde gesperrt"
          gender <- "Benutzer wurde gesperrt"
          about <- 0
          age <- "Benutzer wurde gesperrt"  
        }else{
          first_name <- 0
          last_name <- 0
          living_in <- 0
          homepage <- 0
          country <- 0
          gender <- 0
          about <- 0
          age <- 0
        }
        
      }else{
        #html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)
        author_username <- html_text(html_nodes(x = page.html, css = "h2") , trim = TRUE)
        
        ## Infos
        # Vorname
        first_name <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Vorname:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Vorname:") + 1]
        }else{
          0
        }
        
        # Nachname
        last_name <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Nachname:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Nachname:") + 1]
        }else{
          0
        }
        
        # Wohnort
        living_in <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Wohnort:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Wohnort:") + 1]
        }else{
          0
        }
        # Homepage
        homepage <- if(length(html_text(html_nodes(x = page.html, css = ".no-wrap") , trim = TRUE)) != 0){
          h <- html_nodes(x = page.html, css = ".no-wrap")
          df <- bind_rows(lapply(xml_attrs(h), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
          df$href
          
        }else{
          0
        }
        
        # Land
        country <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Land:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Land:") + 1]
        }else{
          0
        }
        
        # Gender
        gender <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Geschlecht:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Geschlecht:") + 1]
        }else{
          0
        }
        
        # Ueber mich
        about <- if(length(html_text(html_nodes(x = page.html, css = ".user-formatted") , trim = TRUE)) != 0){
          html_text(html_nodes(x = page.html, css = ".user-formatted") , trim = TRUE)
        }else{
          0
        }
        
        # Alter
        age <-if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Alter:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Alter:") + 1]
        }else{
          0
        }
      }
      
      
    }
    author_infos[[u]] <- data.frame (as.character(author_username),
                                     as.character(first_name),
                                     as.character(last_name),
                                     as.character(living_in),
                                     as.character(country),
                                     as.character(gender),
                                     as.character(age), 
                                     as.character(homepage),
                                     as.character(about), stringsAsFactors = FALSE)
    
    # View(author_infos[[u]])
    
    colnames(author_infos[[u]]) <- c("author_username", 
                                     "first_name",
                                     "last_name", 
                                     "living_in", 
                                     "country", 
                                     "gender", 
                                     "age", 
                                     "homepage",
                                     "about")
    
    Sys.sleep(sample(min.wait:max.wait)[1])
    
    print(paste("fertig mit url no. ", u, " von ", length(vector_of_urls) ))  
    
    if(u / save.after == round(u / save.after)){
      author_table_df <- bind_rows(author_infos)
      write.csv2(author_table_df, paste0(wd, "\\AutorInneninfo\\", u,"_AutorInneninfo.csv"))
      
    }
  }
  
  author_table_df <- bind_rows(author_infos)
  
  # View(author_table_df)
  
  ####--------------------------------------------------------------------------
  ####---------- Die Funktion kann einen Abgleich mit einer 
  #### Gesamtautor*innentabelle beinhalten. Dafür muss ein Pfad zur Gesamttabelle 
  #### definiert werden. 
  #################
  # print("Zuweisung der Userids...")
  # author_table_df$author_id <- 0
  # for(i in 1:nrow(author_table_df)){
  #   database_authorids <- read.csv("INSERT_PATH_HERE\\authorsReviewers-ids.csv", stringsAsFactors = FALSE)[,-1]
  #   
  #   #i <- 1
  #   if(author_table_df$author_username[[i]] %in% database_authorids$username){
  #     author_table_df$author_id[[i]] <- database_authorids$id[which(database_authorids$username == author_table_df$author_username[[i]])]
  #   }else{
  #     database_authorids_new <- data.frame( c(database_authorids$username,author_table_df$author_username[[i]] ), 
  #                                           c(database_authorids$id, (max(database_authorids$id)+1) ))
  #     colnames(database_authorids_new) <-  c("username", "id")
  #     
  #     author_table_df$author_id[[i]] <- database_authorids_new$id[which(database_authorids_new$username == author_table_df$author_username[[i]])]
  #     
  #     #View(database_authorids)
  #     write.csv(database_authorids_new,"INSERT_PATH_HERE\\authorsReviewers-ids.csv")
  #     
  #   }
  # }
  #
  write.csv2(author_table_df, paste0(wd, "\\AutorInneninfo\\AutorInneninfo.csv"))
  print("Autorinnentabelle ist fertig!")
  
}


#------------------------------------------------------------------------------#
# Fuer nicht zugaengliche Geschichten
#------------------------------------------------------------------------------#
createTextTableRestricted <- function(dataframe, min.wait, max.wait){
  # min.wait <- 9 
  # max.wait <- 11
  df <- vector("list", length =nrow(dataframe))
  if(valid_url(dataframe$url) == FALSE){
    df <- data.frame (1, "URL existiert nicht mehr")
    colnames(df)<- c("chapter","text")
    rownames(df)<- NULL
    
    enc <- file(paste0(wd,"\\", dataframe$text_id, "_", paste(unlist(strsplit(removePunctuation(dataframe$text_title), " ")), collapse = "-"), ".csv"), encoding="UTF-8")
    write.csv2(df, file =enc )
    print(paste("URL EXISTIERT NICHT, Geschichte Nr.", r, "von", nrow(dataframe), "abgespeichert"))
  }else{
    
    if(is.na(as.numeric(dataframe$chapter_amount)) == FALSE){
      if(dataframe$chapter_amount != 1){
        
        if(isRestricted(dataframe$url) == FALSE){      # hier wird die Anzahl der Kapitel neu gescrapet, wenn die Geschichte zugaenglich ist. 
          urls_ch <- paste0(dataframe$url, "/", 1:getChapterAmountFromStorypage(dataframe$url)) # Wenn nicht, dann wird sie spaeter als nicht zugaenglich aussortiert.
        }else{
          urls_ch <- paste0(z[[1]][1], "/", 1:dataframe$chapter_amount)
        }
      }else{
        urls_ch <- dataframe$url
      }
      if (length(urls_ch) == 1){
        text <- getTextFromStorypage(urls_ch[[1]])
      }else{
        text <- list()
        
        for( i in 1:length(urls_ch)){
          # i <- 111
          text [[i]] <- getTextFromStorypage(urls_ch[[i]])
          Sys.sleep(sample(min.wait:max.wait)[1])
          print(paste("Kapitel Nr.", i, "von", length(urls_ch), "gescrapet"))
        }
        
        text <- trimws(unlist(text))
      }
      df <- data.frame (1:length(text), text, stringsAsFactors = FALSE)
      colnames(df)<- c("chapter","text")
      rownames(df)<- NULL
      #class(df$text)
      
      enc <- file(paste0(wd,"\\", dataframe$text_id, "_", paste(unlist(strsplit(removePunctuation(dataframe$text_title), " ")), collapse = "-"), ".csv"), encoding="UTF-8")
      write.csv2(df, file =enc )
      print(paste("Dataframe mit Geschichte Nr.", "von", nrow(dataframe), "abgespeichert"))
      
      
    }
    
  }
}

#------------------------------------------------------------------------------#
### AutorInneninformationen Scrapen
#------------------------------------------------------------------------------#
#table_with_rev <- metadata_w_reviews

# vector_of_urls <- author_pages [1:10]

createAuthorInfoTableComplete <- function(vector_of_urls, min.wait, max.wait, save.after){
  #min.wait <- 5
  #max.wait <- 6
  if(missing(save.after)) {
    save.after <- 500
  } 
  if(missing(min.wait)) {
    min.wait <- 10
  } 
  if(missing(max.wait)) {
    max.wait <- 15
  } 
  
  author_infos <- vector("list", length= length(vector_of_urls))
  for ( u in 1: length(vector_of_urls)){
    # u <- 17551
    if(GET(vector_of_urls[[u]])$status_code != 200){
      author_username <- unlist(strsplit(vector_of_urls[[u]], "https://www.fanfiktion.de/u/"))[[2]]
      if(length(unlist(strsplit(author_username, "\\+"))) != 1){
        author_username <- paste(unlist(strsplit(author_username, "\\+")), collapse = " ")
      }
      first_name <- "url_non_existent"
      last_name <- "url_non_existent"
      living_in <- "url_non_existent"
      homepage <- "url_non_existent"
      country <- "url_non_existent"
      gender <- "url_non_existent"
      about <- "url_non_existent"
      age <- "url_non_existent"
      
    }else{
      page <- session_jump_to(session, as.character(vector_of_urls[u]))
      page.html <- read_html(page)
      
      if(length(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)) == 0){
        author_username <- paste(unlist(strsplit(unlist(strsplit(vector_of_urls[u], "/"))[length(unlist(strsplit(vector_of_urls[u], "/")))], "-")), collapse = " ")
        if(length(html_text(html_nodes(x = page.html, css = ".caption") , trim = TRUE)) !=0 && length(html_text(html_nodes(x = page.html, css = ".caption") , trim = TRUE)) ==  "Benutzer wurde gesperrt"){
          first_name <- "Benutzer wurde gesperrt"
          last_name <- "Benutzer wurde gesperrt"
          living_in <- "Benutzer wurde gesperrt"
          homepage <-"Benutzer wurde gesperrt"
          country <-"Benutzer wurde gesperrt"
          gender <- "Benutzer wurde gesperrt"
          about <- 0
          age <- "Benutzer wurde gesperrt"  
        }else{
          first_name <- 0
          last_name <- 0
          living_in <- 0
          homepage <- 0
          country <- 0
          gender <- 0
          about <- 0
          age <- 0
        }
        
      }else{
        html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)
        author_username <- html_text(html_nodes(x = page.html, css = "h2") , trim = TRUE)
        
        ## Infos
        # Vorname
        first_name <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Vorname:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Vorname:") + 1]
        }else{
          0
        }
        
        # Nachname
        last_name <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Nachname:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Nachname:") + 1]
        }else{
          0
        }
        
        # Wohnort
        living_in <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Wohnort:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Wohnort:") + 1]
        }else{
          0
        }
        # Homepage
        homepage <- if(length(html_text(html_nodes(x = page.html, css = ".no-wrap") , trim = TRUE)) != 0){
          h <- html_nodes(x = page.html, css = ".no-wrap")
          df <- bind_rows(lapply(xml_attrs(h), function(x) data.frame(as.list(x), stringsAsFactors=FALSE)))
          df$href
          
        }else{
          0
        }
        
        # Land
        country <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Land:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Land:") + 1]
        }else{
          0
        }
        
        # Gender
        gender <- if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Geschlecht:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Geschlecht:") + 1]
        }else{
          0
        }
        
        # Ueber mich
        about <- if(length(html_text(html_nodes(x = page.html, css = ".user-formatted") , trim = TRUE)) != 0){
          html_text(html_nodes(x = page.html, css = ".user-formatted") , trim = TRUE)
        }else{
          0
        }
        
        # Alter
        age <-if(length(which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Alter:")) != 0){
          html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE) [which(html_text(html_nodes(x = page.html, css = ".cell") , trim = TRUE)  == "Alter:") + 1]
        }else{
          0
        }
      }
      
      
    }
    
    if(length(unlist(strsplit(author_username, "\\+"))) != 1){
      author_username <- paste(unlist(strsplit(author_username, "\\+")), collapse = " ")
    }
    
    author_infos[[u]] <- data.frame (as.character(author_username),
                                     as.character(first_name),
                                     as.character(last_name),
                                     as.character(living_in),
                                     as.character(country),
                                     as.character(gender),
                                     as.character(age), 
                                     as.character(homepage),
                                     as.character(about), stringsAsFactors = FALSE)
    
    # View(author_infos[[u]])
    
    colnames(author_infos[[u]]) <- c("author_username", 
                                     "first_name",
                                     "last_name", 
                                     "living_in", 
                                     "country", 
                                     "gender", 
                                     "age", 
                                     "homepage",
                                     "about")
    
    Sys.sleep(sample(min.wait:max.wait)[1])
    
    print(paste("fertig mit url no. ", u, " von ", length(vector_of_urls) ))  
    
    if(u / save.after == round(u / save.after)){
      author_table_df <- bind_rows(author_infos)
      write.csv2(author_table_df, paste0(wd, u,"_AutorInnen_ReviewerInneninfo.csv"))
      
    }
  }
  
  author_table_df <- bind_rows(author_infos)
  
  write.csv2(author_table_df, paste0(wd,"Autorinnen_ReviewerinnenInfo\\", date, "_AutorInnen_ReviewerInneninfo.csv"))
  print("Autorinnentabelle ist fertig!")
  
}

