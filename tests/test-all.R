library(testthat)
library(geonapi)

#test environment
gnUrl <- "http://localhost:8080/geonetwork"
gnUsr <- "admin"
gnPwd <- "geonetwork"
gnLogger <- "DEBUG"
gnman <- try(GNManager$new(gnUrl, gnUsr, gnPwd, gnLogger))


if(is(gnman, "GNManager")){
  cat(sprintf("GeoNetwork test instance started at %s. Running integration tests...\n", gnUrl))
  test_check("geonapi")
}else{
  cat("GeoNetwork test instance is not started. Skipping integration tests...\n")
}
