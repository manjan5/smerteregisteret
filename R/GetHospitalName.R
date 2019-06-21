#' Get hospital name from registry data
#'
#' Based on the hospital id (reshID) this function will return the name of
#' the corresponding hospital as provided in the registry data
#'
#' @param reshID string defining the resh ID
#'
#' @return string of hospital name
#' @export
#'
#' @examples
#' \dontrun{
#' getHospitalName("123456")
#' }

getHospitalName <- function(reshID) {

  regName <- "smerte"
  dbType <- "mysql"
  query <- paste("SELECT SykehusNavn FROM AlleVarNum WHERE AvdRESH ='",
                 reshID, "' LIMIT 1")

  rapbase::LoadRegData(regName, dbType = dbType, query = query)[1,1]

}
