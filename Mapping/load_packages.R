## PURPOSE: load all packages related to Spatial Coding

spatial.packages <- function(){
  # get all packages
  if(!require("caret")) install.packages("caret"); library("caret")
  if(!require("e1071")) install.packages("e1071"); library("e1071")
  if(!require("dplyr")) install.packages("dplyr"); library("dplyr")
  if(!require("ggmap")) install.packages("ggmap"); library("ggmap")
  if(!require("RCurl")) install.packages("RCurl"); library("RCurl")
  if(!require("xlsx")) install.packages("xlsx"); library("xlsx")
  if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
  if(!require("rgdal")) install.packages("rgdal"); library("rgdal")
  if(!require("sp")) install.packages("sp"); library("sp")
  if(!require("maptools")) install.packages("maptools"); library("maptools")
  if(!require("scales")) install.packages("scales"); library("scales")
  if(!require("Cairo")) install.packages("Cairo"); library("Cairo")
  if(!require("rgeos")) install.packages("rgeos"); library("rgeos")
  if(!require("raster")) install.packages("raster"); library("raster")
}