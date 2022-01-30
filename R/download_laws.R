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

    ## Start Time
    begin <- Sys.time()
    message(paste0("Begin at ", begin, "."))

    ## Define Download Date
    download.date <- Sys.Date()

    ## Extract Links to ZIP Archives from XML Table of Contents
    XML <- xml2::read_xml("https://www.gesetze-im-internet.de/gii-toc.xml")

    links <- rvest::html_elements(XML,
                                  "link")

    links.xml <- xml2::xml_text(links)
    

    ## Define Name of Target Directory
    dir <- paste0("Gesetze-im-Internet_",
                  Sys.Date())

    ## Delete Target Directory from Previous Run
    unlink(dir)

    ## Create Target Directory
    invisible(dir.create(dir))

    
    ## === XML DOWNLOAD ===
    
    if ((filetype == "all") || (filetype == "xml")){
        
        ## Define Folders
        dir.xml <- file.path(dir,
                             c("xml_zip",
                               "xml_text",
                               "xml_attachments"))

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


        ## Extract Long Titles

        longtitle.raw <- rvest::html_elements(XML, "title")
        longtitle.raw <- rvest::html_text(longtitle.raw)

        ## Clean and Shorten Names

        longtitle <- gsub(" ", "", longtitle.raw)
        longtitle <- gsub("[[:punct:]]", "", longtitle)


        ## Disambiguate "Allgemeines Eisenbahngesetz"
        AEGindex <- grep("AllgemeinesEisenbahngesetz", longtitle)

        longtitle[AEGindex] <- c("AllgemeinesEisenbahngesetz1993",
                                 "AllgemeinesEisenbahngesetz1951")


        ## Extract Short Titles

        shorttitle <- filenames.pdf

        shorttitle <- gsub(".pdf",
                           "",
                           shorttitle)

        shorttitle <- gsub("_",
                           "",
                           shorttitle)



        ## Create Full Title (Short Title + Long Title) and Truncate

        title <- paste(shorttitle,
                       longtitle,
                       sep="_")

        title <- strtrim(title,
                         200)

        
        ## Make Long Title Unique

        title <- make.unique(title,
                             sep = "-")



        ## Add File Extension

        title.epub <- paste0(title, ".epub")
        title.pdf <- paste0(title, ".pdf")



        ## Define EPUB Links

        prelinks.epub <- gsub("xml.zip",
                              "",
                              links.xml)

        links.epub <- paste0(prelinks.epub,
                             filenames.epub)


        ## Define PDF Links

        prelinks.pdf <- gsub("xml.zip",
                             "",
                             links.xml)

        links.pdf <- paste0(prelinks.pdf,
                            filenames.pdf)



        ## Prepare Data Table for Download

        download <- data.table::data.table(title.epub,
                                           links.epub,
                                           title.pdf,
                                           links.pdf)
        
    }
    

    ## === PDF DOWNLOAD ===
    
    if ((filetype == "all") || (filetype == "pdf")){

        ## Create PDF Directory
        dir.create(file.path(dir, "pdf"))

        ## Execute Download
        download.result.pdf <- future.apply::future_mapply(utils::download.file,
                                                           url = download$links.pdf,
                                                           destfile = file.path(dir,
                                                                                "pdf",
                                                                                download$title.pdf))

        ## Return Download Success
        message(paste(sum(unlist(download.result.pdf) == 0),
                      "of",
                      length(links.xml),
                      "PDF files successfully downloaded."))

    }

    
    ## === EPUB DOWNLOAD ===
    
    if ((filetype == "all") || (filetype == "epub")){


        ## Create EPUB Directory
        dir.create(file.path(dir, "epub"))

        ## Execute Download
        download.result.epub <- future.apply::future_mapply(utils::download.file,
                                                            url = download$links.epub,
                                                            destfile = file.path(dir,
                                                                                 "epub",
                                                                                 download$title.epub))


        ## Return Download Success
        message(paste(sum(unlist(download.result.epub) == 0),
                      "of",
                      length(links.xml),
                      "EPUB files successfully downloaded."))

    }


    ## End Time and Duration
    end <- Sys.time()
    duration <- end - begin

    ## Message End
    message(paste0("Ended at ", end, "."))

    ## Message Duration
    message(paste0("Download completed after ",
                   round(duration,
                         digits = 2),
                   " ",
                   attributes(duration)$units,
                   "."))
            

    
}

