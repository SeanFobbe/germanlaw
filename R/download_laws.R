#' Download Current Versions of German Laws and Regulations



download_xml <- function(x,
                         dir){

    filename <- gsub("http://www.gesetze-im-internet.de/(.*)/xml\\.zip",
                           "\\1\\.zip",
                           x)

    destination <- file.path(dir, filename)
    
    utils::download.file(x,
                         destination)
    
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
    dir.create(dir)


    if ((filetype == "all") || (filetype == "xml")){

        ## Create Folders
        dir.create(paste0(dir, "/xml_zip"))
        dir.create(paste0(dir, "/xml"))
        dir.create(paste0(dir, "/attachments"))


        
        ## Download XML Files
        download.result.xml <- future.apply::future_lapply(X = links.xml,
                                              FUN = download_xml,
                                              dir = dir
                                              )

        message(paste(sum(unlist(download.result.xml) == 0),
                      "of",
                      length(links.xml),
                      "XML files successfully downloaded."))


        ## Unzip XML
        files.zip <- list.files(dir,
                                pattern = "\\.zip",
                                full.names = TRUE)
        
        invisible(lapply(files.zip,
                         zip::unzip,
                         exdir = dir))
        

        
        ## Move ZIP Files

        invisible(file.rename(files.zip,
                              file.path(dir,
                                        "xml_zip",
                                        basename(files.zip)
                                        )
                              )
                  )


        
        ## Move XML Files

        files.xml <- list.files(dir,
                                pattern = "\\.xml",
                                full.names = TRUE)

        invisible(file.rename(files.xml,
                              file.path(dir,
                                        "xml",
                                        basename(files.xml)
                                        )
                              )
                  )


        
        ## Move Attachments


        
        files.attachments <- list.files(dir,
                                pattern = "\\.jpg|\\.gif|\\.png",
                                full.names = TRUE)

        invisible(file.rename(files.attachments,
                              paste0(dir, "/attachments/", basename(files.attachments))))
        
    }

    
}




x <- "http://www.gesetze-im-internet.de/begdv_3/xml.zip"
