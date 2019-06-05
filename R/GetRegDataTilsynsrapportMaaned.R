#' Provide dataframe of registry data
#'
#' Provides a dataframe containing data from a registry
#'
#' @param registryName String providing the current registryName
#' @param reshId String providing reshId for filtering in query
#' @return regData data frame
#' @export

getRegDataTilsynsrapportMaaned <- function(registryName, reshId) {

  dbType <- "mysql"

  query <- "
SELECT
  var.AntTilsLege,
  var.AntTilsSykPleier,
  var.AntTilsFysioT,
  var.AntTilsPsyk,
  var.AntTilsSosio,
  var.AntPasTils,
  var.Tilsett,
  var.RegDato11,
  var.InnlAvd,
  var.PasientID,
  var.ForlopsID,
  avd.DEPARTMENT_ID,
  avd.DEPARTMENT_NAME
FROM
  AlleVarNum var
LEFT JOIN
  Avdelingsoversikt avd
ON
  avd.DEPARTMENT_ID = var.InnlAvd
WHERE
  var.AvdRESH ="

  query <- paste(query, reshId)

  regData <- rapbase::LoadRegData(registryName, query, dbType)

  return(regData)
}
