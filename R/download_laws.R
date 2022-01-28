#' Download Current Versions of German Laws and Regulations



download_xml <- function(x,
                         dir){

    filename <- base::gsub("http://www.gesetze-im-internet.de/(.*)/xml\\.zip",
                           "\\1\\.zip",
                           x)

    destination <- base::file.path(dir, filename)
    
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
    download.date <- base::Sys.Date()

    ## Define Name of Target Directory
    dir <- base::paste0("Gesetze-im-Internet_",
                       base::Sys.Date())

    ## Create Target Directory
    base::dir.create(dir)


    if ((filetype == "all") || (filetype == "xml")){
        
        ## Download XML Files
        download.result.xml <- future.apply::future_lapply(X = links.xml,
                                              FUN = download_xml,
                                              dir = dir
                                              )

        message(paste(base::sum(base::unlist(download.result.xml) == 0),
                      "of",
                      base::length(links.xml),
                      "XML files successfully downloaded."))


        ## Unzip XML
        files.zip <- list.files(dir,
                                pattern = "\\.zip",
                                full.names = TRUE)
        
        base::lapply(files.zip,
                     zip::unzip,
                     exdir = dir)
        

        
        ## Move ZIP Files
        dir.create(paste0(dir, "/xml_zip"))

        invisible(file.rename(files.zip,
                    paste0(dir, "/xml_zip/", basename(files.zip))))


        
        ## Move XML Files

        dir.create(paste0(dir, "/xml"))
        
        files.xml <- list.files(dir,
                                pattern = "\\.xml",
                                full.names = TRUE)

        invisible(file.rename(files.xml,
                              paste0(dir, "/xml/", basename(files.xml))))


        
        ## Move Attachments

        dir.create(paste0(dir, "/attachments"))
        
        files.attachments <- list.files(dir,
                                pattern = "\\.jpg|\\.gif|\\.png",
                                full.names = TRUE)

        invisible(file.rename(files.attachments,
                              paste0(dir, "/attachments/", basename(files.attachments))))
        
    }

    
}




x <- "http://www.gesetze-im-internet.de/begdv_3/xml.zip"
