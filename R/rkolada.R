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

  GetKpiSearchData = function(kpi_id = NA) {
    .self$kpiData <- getKpiData(kpi_id)
    return(.self$kpiData)
  },

  GetAllData = function(kpi_id = NA, municipality = NA, year = NA) {
    # /v2/data/kpi/<KPI>/municipality/<MUNICIPALITY_ID>/year/<PERIOD> 
    municipalityData <- getManicipalityData(municipality)
    queryUrl <- paste0(.self$api, "/data/kpi/", URLencode(kpi_id), "/municipality/", URLencode(municipalityData[["id"]]), "/year/", URLencode(year))
    
    response <- .self$apiRequest(queryUrl)
    print(response)
    if(!is.data.frame(response)){
      response <- as.data.frame(response)
    }
    return(response)
  },

  getKpiData = function(kpiString){
    if(!is.character(kpiString)){
      print("Error: Shitty Kpi name")
    }
    queryUrl <- paste0(.self$api, "/kpi?title=", URLencode(kpiString))
    .self$kpiData <- (.self$apiRequest(queryUrl))
  },

  getManicipalityData = function(municipalityString){
  queryUrl <- paste0(.self$api, "/municipality?title=", URLencode(municipalityString))
  return(.self$apiRequest(queryUrl))
  },


  apiRequest = function(queryUrl){
  # Make the GET request to the Kolada API
    response <- GET(queryUrl)
    # Check if the request was successful
    if (status_code(response) != 200) {
      sprintf("Error: Unable to fetch data. Status code: ", status_code(response))
      print(response)
    }
    data <- .self$parseJson(response)
    # Return the structured KPI data as a data frame
    return(as.data.frame(data))
  },

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





