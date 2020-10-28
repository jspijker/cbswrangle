# cbswrangle

R package with helper functions for cbsodataR package. This package
evolved around some ideas to make accessing data from Statistics
Netherlands and data management of downloaded data somewhat easier.

Although this is an R package, it does not follow the R packaging
standards fully. To install this package one must first:

1. install the datafile package from GitHub: `remotes::install\_github("jspijker/datafile")`
2. install the cbsodataR package from CRAN
3. then install this package: `remotes:install\_github("jspijker/cbswrangle")`

Also before one loads the cbswrangle package, the packages datafile
and cbsodataR must be loaded first.
 
