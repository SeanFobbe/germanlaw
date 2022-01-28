#' Download Current Versions of German Laws and Regulations



download_xml <- function(x,
                         download.date,
                         filepath){

    filename <- gsub("http://www.gesetze-im-internet.de/(.*)/xml\\.zip",
                     "\\1\\.zip",
                     x)

    destination <- paste0(filepath,
                          "/",
                          filename)
    
    download.file(x,
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

    ## Set Filepath and Create Dir
    dir <- paste0("Gesetze-im-Internet_XML_",
                       Sys.Date())
    
    dir.create(dir)


    if ((filetype == "all") || (filetype == "xml")){
        
        ## Download XML Files
        invisible(future.apply::future_lapply(links.xml,
                                              download_xml,
                                              download.date = download.date,
                                              filepath = filepath))


        ## Unzip XML
        files.zip <- list.files(filepath,
                                pattern = "\\.zip",
                                full.names = TRUE)
        
        lapply(files.zip,
               unzip,
               exdir = filepath)


        
        ## Move ZIP Files
        dir.create(paste0(filepath, "/xml_zip"))

        invisible(file.rename(files.zip,
                    paste0(filepath, "/xml_zip/", basename(files.zip))))


        
        ## Move XML Files

        dir.create(paste0(filepath, "/xml"))
        
        files.xml <- list.files(filepath,
                                pattern = "\\.xml",
                                full.names = TRUE)

        invisible(file.rename(files.xml,
                              paste0(filepath, "/xml/", basename(files.xml))))


        
        ## Move Attachments

        dir.create(paste0(filepath, "/attachments"))
        
        files.attachments <- list.files(filepath,
                                pattern = "\\.jpg|\\.gif|\\.png",
                                full.names = TRUE)

        invisible(file.rename(files.attachments,
                              paste0(filepath, "/attachments/", basename(files.attachments))))
        
    }

    
}




x <- "http://www.gesetze-im-internet.de/begdv_3/xml.zip"
