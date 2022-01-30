#' Download Current Versions of German Laws and Regulations




extract_links <- function(x,
                          NA.omit = FALSE){
    
    links <- tryCatch({
        
        html <- xml2::read_html(x)
        
        nodes <- rvest::html_elements(html,
                                      "a")
        
        rvest::html_attr(nodes,
                         'href')},
        
        error = function(cond) {
            
            return(NA)}
        
        )

    if(NA.omit == TRUE){

        links <- links[!is.na(links)]
            
    }

    return(links)
    
}


download_xml <- function(x,
                         dir){

    filename <- gsub(pattern = "http://www.gesetze-im-internet.de/(.*)/xml\\.zip",
                     replacement = "\\1\\.zip",
                     x = x)

    destination <- file.path(dir,
                             filename)
    
    utils::download.file(url = x,
                         destfile = destination)
    
}



download_laws <- function(filetype = "xml"){

    ## Extract Links to ZIP Archives from XML Table of Contents
    XML <- xml2::read_xml("https://www.gesetze-im-internet.de/gii-toc.xml")

    links <- rvest::html_elements(XML,
                                  "link")

    links.xml <- xml2::xml_text(links)

    ## Define Download Date
    download.date <- Sys.Date()

    ## Define Name of Target Directory
    dir <- paste0("Gesetze-im-Internet_",
                  Sys.Date())

    ## Create Target Directory
    invisible(dir.create(dir))

    
    ## === XML DOWNLOAD ===
    
    if ((filetype == "all") || (filetype == "xml")){

        ## Store Start Time
        begin <- Sys.time()
        
        ## Define Folders
        dir.xml <- file.path(dir,
                             c("xml_zip",
                               "xml_text",
                               "xml_attachments"))

        ## Delete Old Folders from Previous Runs
        unlink(dir.xml,
               recursive = TRUE)

        ## Create Folders
        invisible(lapply(dir.xml,
                         dir.create))

        
        ## Download DTD
        download.file(url = "https://www.gesetze-im-internet.de/dtd/1.01/gii-norm.dtd",
                      destfile = file.path(dir,
                                          "GII_XML_Document-Type-Definition_v1-01.dtd"))
        
        ## Download XML Files
        download.result.xml <- future.apply::future_lapply(X = links.xml,
                                                           FUN = download_xml,
                                                           dir = dir
                                                           )

        ## Return Download Success
        message(paste(sum(unlist(download.result.xml) == 0),
                      "of",
                      length(links.xml),
                      "XML files successfully downloaded."))


        ## Unzip XML
        files.zip <- list.files(dir,
                                pattern = "\\.zip",
                                full.names = TRUE)
        
        invisible(lapply(X = files.zip,
                         FUN = zip::unzip,
                         exdir = dir))
        

        
        ## Store ZIP Files

        invisible(file.rename(files.zip,
                              file.path(dir,
                                        "xml_zip",
                                        basename(files.zip)
                                        )
                              )
                  )


        
        ## Store XML Files

        files.xml <- list.files(dir,
                                pattern = "\\.xml",
                                full.names = TRUE)

        invisible(file.rename(files.xml,
                              file.path(dir,
                                        "xml_text",
                                        basename(files.xml)
                                        )
                              )
                  )


        
        ## Store Attachments

        files.attachments <- list.files(dir,
                                        pattern = "\\.jpg|\\.gif|\\.png|\\.pdf",
                                        ignore.case = TRUE,
                                        full.names = TRUE)

        invisible(file.rename(files.attachments,
                              file.path(dir,
                                        "xml_attachments",
                                        basename(files.attachments)
                                        )
                              )
                  )
        
    }



    ## === PDF und EPUB DOWNLOAD VORBEREITEN ===
    
    if ((filetype == "all") || (filetype == "pdf") || (filetype == "epub")){


        ## Extract HTML Links

        links.html <- gsub("/xml.zip",
                           "/index.html",
                           links.xml)
        
        links.list <- future.apply::future_lapply(links.html,
                                                  extract_links)


        links.raw <- unlist(links.list)


        ## Define PDF and EPUB Filenames
        
        filenames.pdf <- grep (".pdf$",
                               links.raw,
                               ignore.case = TRUE,
                               value = TRUE)

        filenames.epub <- grep (".epub$",
                                links.raw,
                                ignore.case = TRUE,
                                value = TRUE)


        ## --- Vektor der Langtitel erstellen
        ## Hinweis: Es gibt zwei Rechtsakte mit dem Namen "Allgemeine Eisenbahngesetz", obwohl es sich um zwei unterschiedliche Rechtsakte handelt. Die beiden Rechtsakte werden daher um ihr jeweiliges Ausfertigungsjahr ergänzt um die Dateinamen einzigartig zu machen.

        longtitle.raw <- rvest::html_elements(XML, "title")
        longtitle.raw <- rvest::html_text(longtitle.raw)

        ## Namen bereinigen und kürzen

        longtitle <- gsub(" ", "", longtitle.raw)
        longtitle <- gsub("[[:punct:]]", "", longtitle)


        ## Indizes der AEG bestimmen
        AEGindex <- grep("AllgemeinesEisenbahngesetz", longtitle)


        ## AEGs umbenennen
        longtitle[AEGindex] <- c("AllgemeinesEisenbahngesetz1993",
                                 "AllgemeinesEisenbahngesetz1951")


        ## Vektor der Kurztitel erstellen

        shorttitle <- filenames.pdf

        shorttitle <- gsub(".pdf",
                           "",
                           shorttitle)

        shorttitle <- gsub("_",
                           "",
                           shorttitle)



        ## Vektoren der Titel vereinigen
        ## Die Kurz- und Langtitel werden zu einem Vektor zusammengefügt. Dieser wird dann auf maximal 200 Zeichen gekürzt, damit keine Probleme für Windows-User entstehen. 

        title <- paste(shorttitle,
                       longtitle,
                       sep="_")

        title <- strtrim(title,
                         200)



### Prüfung auf Namens-Kollisionen
        ##  Kollidierende Namen anzeigen. Wenn Namens-Kollisionen bestehen (wie oben beim AEG) müssen diese unbedingt bereinigt werden, weil ansonsten beim Herunterladen eine Datei alle anderen mit dem gleichen Namen überschreibt.
                                        #
        title[duplicated(title)]


        ## --- Bereinigung von Namens-Kollisionen
        ## Eine manuelle Bereinigung von Kollisionen ist bevorzugt. Falls keine manuelle Bereinigung stattgefunden hat wird in diesem Schritt eine automatische Bereinigung durchgeführt.

        title <- make.unique(title,
                             sep = "-")



        ## Dateierweiterungen hinzufügen

        title.xml <- paste0(title, ".zip")
        title.epub <- paste0(title, ".epub")
        title.pdf <- paste0(title, ".pdf")



        ## Links zu EPUB-Dateien erstellen

        prelinks.epub <- gsub("xml.zip",
                              "",
                              links.xml)

        links.epub <- paste0(prelinks.epub,
                             filenames.epub)


        ## Links zu  PDF-Dateien erstellen

        prelinks.pdf <- gsub("xml.zip",
                             "",
                             links.xml)

        links.pdf <- paste0(prelinks.pdf,
                            filenames.pdf)



        ## Data Table für Download vorbereiten

        download <- data.table(title.epub,
                               links.epub,
                               title.pdf,
                               links.pdf)

    }
    

    ## === PDF DOWNLOAD ===
    
    if ((filetype == "all") || (filetype == "pdf")){

        future.apply::future_mapply(utils::download.file,
                                    download$links.pdf,
                                    download$title.pdf)

    }

    
    ## === EPUB DOWNLOAD ===
    
    if ((filetype == "all") || (filetype == "epub")){

        future.apply::future_mapply(utils::download.file,
                                    download$links.epub,
                                    download$title.epub)

    }


    ## Store End Time
    end <- Sys.time()

    ## Message Download Duration
    message(paste("Download completed after", round(end-begin), "seconds."))
    
}

