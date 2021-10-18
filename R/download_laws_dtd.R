#' Download Document Type Definition for gesetze-im-internet



download_laws_dtd <- function(){

    download.file("https://www.gesetze-im-internet.de/dtd/1.01/gii-norm.dtd",
                  paste0("GII_",
                         Sys.Date(),
                         "_XML_Document-Type-Definition_v1-01.dtd"))

    }

