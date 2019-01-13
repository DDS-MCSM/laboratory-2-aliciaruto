#******************************************************************************#
#                                                                              #
#                          Lab 2 - CPE Standard                                #
#                                                                              #
#              Arnau Sangra Rocamora - Data Driven Securty                     #
#                                                                              #
#******************************************************************************#

GetCPEFile <- function() {
  compressed_cpes_url <- "https://nvd.nist.gov/feeds/xml/cpe/dictionary/official-cpe-dictionary_v2.3.xml.zip"
  cpes_filename <- "cpes.zip"
  download.file(compressed_cpes_url, cpes_filename)
  unzip(zipfile = cpes_filename)
  cpe.file <- "./official-cpe-dictionary_v2.3.xml"
  cpes.xml <- xml2::read_xml(cpe.file)
  return(cpes.xml)
}

GetCPEItems <- function() {
  cpe.title <- xml2::xml_find_all(cpes.xml, "//*[cpe-23:cpe23-item]/*[1]")
  cpe.name <- xml2::xml_find_all(cpes.xml, "//*[cpe-23:cpe23-item]/*/@name")

  cpe <- data.frame(title = xml2::xml_text(cpe.title),
                     cpe.23 = xml2::xml_text(cpe.name),
                     stringsAsFactors = F)
  return(cpe)
}

CleanCPEs <- function(){

  col.names <- c("cpe",
                 "v",
                 "part",
                 "vendor",
                 "product",
                 "version",
                 "update",
                 "edition",
                 "language",
                 "sw_edition",
                 "target_sw",
                 "target_hw",
                 "other")

  cpe$cpe.23 <- stringr::str_replace_all(cpe$cpe.23, "\\\\:", ";")
  cpe.clear <- tidyr::separate(data = cpe, col = cpe.23, into = col.names, sep = ":", remove = F)
  cpe.clear$cpe <- as.factor(cpe.clear$cpe)
  cpe.clear$v <- as.factor(cpe.clear$v)
  cpe.clear$part <- as.factor(cpe.clear$part)
  cpe.clear$vendor <- as.factor(cpe.clear$vendor)
  cpe.clear$product <- as.factor(cpe.clear$product)
  cpe.clear$language <- as.factor(cpe.clear$language)
  cpe.clear$sw_edition <- as.factor(cpe.clear$sw_edition)
  cpe.clear$target_sw <- as.factor(cpe.clear$target_sw)
  cpe.clear$target_hw <- as.factor(cpe.clear$target_hw)
  cpe.clear$other <- as.factor(cpe.clear$other)

  return(cpe.clear)
}

ParseCPEData <- function(cpe.file) {

  # load cpes as xml file
  cpes <- xml2::read_xml(x = cpe.file)

  # get CPEs
  cpes <- GetCPEItems(cpes)

  # transform, clean, arrange parsed cpes as data frame
  df <- CleanCPEs(cpes)

  # return data frame
  return(df)
}

##############

NewCPEItem <- function(){
  return(data.frame(cpe.22 = character(),
                    cpe.23 = character(),
                    cpe.ref = character(),
                    stringsAsFactors = FALSE)
  )
}
