#' rkolada
#'
#' @description This class uses the Kolada API
#' @return An object that interacts with the Kolada API
#' @examples
#' tbd
#' @import httr jsonlite
#' @importFrom methods new
#' @export
#' 

rkolada <- setRefClass(
  "rkolada",
  fields = list(
    api = "character",
    kpiData = "character"

  ),
  methods = list(
    # constructor
    initialize = function() {
      cat("rkolada object created.\n")
      .self$api <- "http://api.kolada.se/v2"
      .self$kpiData <- "data/kpi"

    },  

    GetKpiData = function(kpi_id = NA, municipality_id = NA, year = NA) {
      
    # Construct the full API URL using provided KPI ID, Municipality ID, and Year
    queryUrl <- paste0(.self$api, "/", .self$kpiData, "/", kpi_id, "/municipality/", municipality_id, "/year/", year)
    
    # Make the GET request to the Kolada API
    response <- GET(queryUrl)
    
    # Check if the request was successful
    if (status_code(response) != 200) {
      stop("Error: Unable to fetch data. Status code: ", status_code(response))
    }
    
    data <- parseJson(response)
    
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





