#context("rkolada")

test_that("class is correct", {
  instance <- rkolada$new()
  expect_true(class(instance)[1] == "rkolada")
  expect_true(instance$api == "http://api.kolada.se/v2")
})

test_that("GetAndSetKpiSearchData will obtain data", {
  instance <- rkolada$new() 
  searchStr <- "Skatt"

  expect_silent(instance$GetAndSetKpiSearchData(searchStr))
  expect_true(is.data.frame(instance$kpiData))
  expect_true(all(dim(instance$kpiData)>c(1,1)))
})

test_that("GetAndSetKpiSearchData bad request", {
  instance <- rkolada$new() 
  searchStr <- "This string will generate nothing"

  expect_silent(instance$GetAndSetKpiSearchData(searchStr))
  expect_true(is.data.frame(instance$kpiData))
  expect_true(all(dim(instance$kpiData)==c(1,1)))
  expect_equal(instance$kpiData$data, "No data available for the specified query.")
})


test_that("GetAndSetKpiSearchData bad characters", {
  instance <- rkolada$new() 
  searchStr <- "&*@%"

  expect_silent(instance$GetAndSetKpiSearchData(searchStr))
  expect_equal(instance$kpiData$data, NULL)
})


test_that("GetKpiData ", {
  # tested above 
  instance <- rkolada$new() 
  searchStr <- "Skatt"

  expect_silent(instance$GetKpiData(searchStr))
})

test_that("GetManicipalityData finds the object and the data is correct", {
  instance <- rkolada$new() 
  searchStr <- "Oxelösund"

  data <- (instance$GetManicipalityData(searchStr))
  expect_equal(data$id,"0481")
  expect_equal(data$type, "K")
})

test_that("GetAllData ", {
  kpi_id = "N00041"
  municipality = "Oxelösund"
  year = "2009"
  instance <- rkolada$new() 

  data <- instance$GetAllData(kpi_id,municipality,year)
  print(dim(data))
  expect_true(is.data.frame(data))
  expect_true(all(dim(data)==c(1,4)))
})


# test_that("resid() method works", {
#   linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
#   expect_equal(round(unname(linreg_mod$resid()[c(7,13,27)]),2), c(0.31, -0.58, -0.20))
# })

# test_that("coef() method works", {
#   linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)

#   expect_true(all(round(unname(linreg_mod$coef()),2) %in% c(-2.52, -1.34, 1.78)))
# })


# test_that("summary() method works", {
#   linreg_mod <- linreg$new(Petal.Length~Sepal.Width+Sepal.Length, data=iris)
  
#   expect_output(linreg_mod$summary(), "\\(Intercept\\)( )*-2.5[0-9]*( )*0.5[0-9]*( )*-4.4[0-9]*( )*.*( )*\\*\\*\\*")  
#   expect_output(linreg_mod$summary(), "Sepal.Width( )*-1.3[0-9]*( )*0.1[0-9]*( )*-10.9[0-9]*( )*.*( )*\\*\\*\\*")
#   expect_output(linreg_mod$summary(), "Sepal.Length( )*1.7[0-9]*( )*0.0[0-9]*( )*27.5[0-9]*( )*.*( )*\\*\\*\\*")
#   expect_output(linreg_mod$summary(), "Residual standard error: 0.6[0-9]* on 147 degrees of freedom")
# })

