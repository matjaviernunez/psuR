#' Boundary
#'
#' A sf polygon object, that limits the area where polygons will be extended.
#'
#' \itemize{
#'   \item geom. sfc_POLYGON information
#' }
#'
#' @docType data
#' @keywords datasets
#' @name boundary
#' @usage data(boundary)
#' @format A sf polygon object with 1 row and 1 variable.
NULL


#' Gap
#'
#' A multi-polygon sf object, geographic data that represents rivers or
#' boundaries, used to avoid undesired connections between blocks, could be
#' contained within the boundary.
#'
#' \itemize{
#'   \item geom. sfc_POLYGON information
#' }
#'
#' @docType data
#' @keywords datasets
#' @name gap
#' @usage data(gap)
#' @format A sf multi-polygon object with 1 row and 1 variable.
NULL


#' Non-adyacent polygons
#'
#' A sf multi-polygon object, geographic data of non-adjacent polygons generally
#' related to blocks.
#'
#' \itemize{
#'   \item id. a character string, blocks ID related to poly argument, preferably unique by block.
#'   \item geom. sfc_MULTIPOLYGON information.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name poly
#' @usage data(poly)
#' @format A sf multi-polygon object with 249 rows and 2 variables.
NULL


#' Block weights
#'
#' A two column data.frame, contains the id and weights of the adyacent polygons
#'  (generally the id and the number of occupied dwellings in each block).
#'
#' \itemize{
#'   \item idp. a character string, blocks ID, preferably unique by block.
#'   \item weight. a integer, weights of adyacent polygons.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name weights
#' @usage data(weights)
#' @format A data.frame object with 249 rows and 2 variables.
NULL


#' Extended polygons
#'
#' A sf multi-polygon object, geographic data of adjacent polygons generally
#' related to blocks. ext_pol is created by running the polygon_ext function
#' with the parameters set in the example.
#'
#' \itemize{
#'   \item id. a character string, blocks ID related to poly argument, preferably unique by block.
#'   \item geom. sfc_MULTIPOLYGON information.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name ext_pol
#' @usage data(ext_pol)
#' @format A sf multi-polygon object with 249 rows and 2 variables.
NULL


#' Incidence matrix
#'
#' A matrix object that represents the binary relation 'polygon i shares a
#' common boundary with polygon j'. im is created by running the inc_matrix
#' function with the parameters set in the example.
#'
#' \itemize{
#'   \item the ith row represents the ith polygon of ext_pol.
#'   \item the jth column represents the jth polygon of ext_pol.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name im
#' @usage data(im)
#' @format A matrix object with 249 rows and 249 columns.
NULL





