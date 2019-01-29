library(testthat)
library(geonapi)

#test environment
#gnUrl <- "https://georchestra-mshe.univ-fcomte.fr/geonetwork"
gnUrl <- "http://collec-za.org/geonetwork"
gnUsr <- "eblondel"
gnPwd <- "eblondel007"
gnVersion <- "3.4"
gnLogger <- "DEBUG"
GN <- try(GNManager$new(gnUrl, gnUsr, gnPwd, gnVersion, gnLogger))

if(is(GN, "GNManager")){
  cat(sprintf("GeoNetwork test instance started at %s. Running integration tests...\n", gnUrl))
  test_check("geonapi")
}else{
  cat("GeoNetwork test instance is not started. Skipping integration tests...\n")
}
