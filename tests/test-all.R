library(testthat)
library(geonapi)

#test environment
gnUrl <- "http://localhost:8080/geonetwork"
gnUsr <- "admin"
gnPwd <- "admin"
gnVersion <- Sys.getenv("GN_VERSION")
gnLogger <- "DEBUG"
GN <- try(GNManager$new(gnUrl, gnUsr, gnPwd, gnVersion, gnLogger))

if(inherits(GN, "GNAbstractManager")){
  cat(sprintf("GeoNetwork test instance started at %s. Running integration tests...\n", gnUrl))
  test_check("geonapi")
}else{
  cat("GeoNetwork test instance is not started. Skipping integration tests...\n")
}
