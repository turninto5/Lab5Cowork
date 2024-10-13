#' rkolada
#'
#' @description This class uses the Kolada API
#' @return An object that interacts with the Kolada API
#' @examples
#' instance <- rkolada()
#' @import httr jsonlite
#' @importFrom methods new
#' @export rkolada
#' @exportClass rkolada


rkolada <- setRefClass(
  "rkolada",
  fields = list(
    api = "character",
    kpiData = "data.frame",
    manicipalityData = "data.frame"
  ),
  methods = list(
    # constructor
    initialize = function() {
      .self$api <- "http://api.kolada.se/v2"
    },

  #' @description Function GetAndSetKpiSearchData(searchStr) returns the residuals
  #' @param searchStr The string search in the database of all KPI. Defaults to `NA`.
  #' @return A data.frame of the reponse from the kolada API
  GetAndSetKpiSearchData = function(searchStr = NA) {
    .self$kpiData <- GetKpiData(searchStr)
    return(.self$kpiData)
  },

  #' @description Function GetAllData(kpi_id, municipality, year) returns the residuals
  #' @param kpi_id Unique identifier of the KPI in rkolada. Defaults to `NA`
  #' @param municipality Unique identifier of the municipality in rkolada. Defaults to `NA`
  #' @param year The year to only filter data form this parameter. Defaults to `NA`.
  #' @return A data.frame of the reponse from the kolada API
  GetAllData = function(kpi_id = NA, municipality = NA, year = NA) {
    municipalityData <- GetManicipalityData(municipality)
    queryUrl <- paste0(.self$api, "/data/kpi/", URLencode(kpi_id), "/municipality/", URLencode(municipalityData[["id"]]), "/year/", URLencode(year))

    response <- .self$apiRequest(queryUrl)
    if(!is.data.frame(response)){
      response <- as.data.frame(response)
    }
    return(response)
  },

  #' @description Function GetKpiData(kpiString)returns the residuals
  #' @param searchStr The string search in the database of all KPI. Defaults to `NA`.
  #' @return A data.frame of the data from the kolada API
  GetKpiData = function(kpiString = NA){
    queryUrl <- paste0(.self$api, "/kpi?title=", URLencode(kpiString))
    return(.self$apiRequest(queryUrl))
  },

  #' @description Function GetManicipalityData(municipalityString)returns the residuals
  #' @param searchStr The string search in the database of all KPI. Defaults to `NA`.
  #' @return A data.frame of the data from the kolada API
  GetManicipalityData = function(municipalityString = NA){
  queryUrl <- paste0(.self$api, "/municipality?title=", URLencode(municipalityString))
  return(.self$apiRequest(queryUrl))
  },

  #' @description Private function apiRequest(queryUrl), only for internal use. Makes an API request given a query
  #' @param queryUrl A string of a URL to interact with the API.
  #' @return A data.frame of the data from the kolada API
  apiRequest = function(queryUrl){
  # Make the GET request to the Kolada API
    response <- GET(queryUrl)
    # Check if the request was successful
    if (status_code(response) != 200) {
      sprintf("Error: Unable to fetch data. Status code: ", status_code(response))
      return(NULL)
    }
    data <- .self$parseJson(response)
    # Return the structured KPI data as a data frame
    return(as.data.frame(data))
  },

  #' @description Private function parseJson(response), only for internal use. Parses the JSON response from the API
  #' @param response The raw JSON from the API.
  #' @return A data.frame of the data from the kolada API
  parseJson = function(response){
    # Parse the JSON response into an R list
    dataJson <- content(response, as = "text", encoding = "UTF-8")
    dataList <- fromJSON(dataJson)
    # Extract the relevant KPI data from the JSON structure
    if (length(dataList$values) == 0) {
      return("No data available for the specified query.")
    }
    # Convert the KPI data into a data frame for easier analysis
    return(dataList$values)
  }
  )
)





