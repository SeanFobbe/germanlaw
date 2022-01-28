#' Download Current Versions of German Laws and Regulations



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


    if ((filetype == "all") || (filetype == "xml")){

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
                                        pattern = "\\.jpg|\\.gif|\\.png",
                                        full.names = TRUE)

        invisible(file.rename(files.attachments,
                              file.path(dir,
                                        "xml_attachments",
                                        basename(files.attachments)
                                        )
                              )
                  )
        
    }

    
}

