#' @import dplyr
#' @import sf
#' @import magrittr
#' @title
#' Function that builds the incidence matrix (IM)!
#' @description
#' This function returns a binary matrix that represents the binary relation 'polygon i shares a common boundary
#' with polygon j'.
#' @details
#' Incidence matrix is computed by the geographical relations between a set of adyacent polygons. The binary
#' relation is derived by intersecting all the polygons with each other. As a result, only line types polygons
#' are expected.
#'
#' If the intersection length between polygon i and polygon j is greater or equal than tol, then the IM[i, j] = IM[j,i] = 1.
#'
#' Its important to mention that diag(IM) = 0.
#' @author Angel Gaibor <mat.angel.gaibor at gmail.com>
#' @author Javier Núñez <mat.javier.nunez at gmail.com>
#' @param poly a sf polygon object, extended polygons that share a common boundary.
#' @param id a character string, the column name of the ID related to extended polygons, preferably unique by polygon.
#' @param tol an integer, tolerance in meters.

#'
#' @references
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas.
#' Valliant, R, et. al. (2013), \emph{Practical tools for Design and Weighting Survey Samples}. Springer
#' @return IM
#' @export
#'
#' @examples
#' inc_matrix(poly, tol=10, id=id)

inc_matrix <- function(poly, tol=10, id = NULL){
    aux <- poly %>%
        rename(id = {{id}})
    aux1 <- st_intersection(aux, aux) %>%
        group_by(id) %>%
        mutate(n = n()) %>%
        ungroup() %>%
        filter(!(id==id.1 & n > 1)) %>%
        mutate(length = as.numeric(st_length(.)),
               freq = 1) %>%
        filter(length > tol | length == 0) %>%
        as.data.frame() %>%
        select(id, id.1, freq)

    o <- aux1 %>%
        arrange(id.1) %>%
        pivot_wider(names_from = id.1, values_from = freq) %>%
        arrange(id) %>%
        select(id, .$id) %>%
        as.data.frame()

    IM <- data.matrix(select(o, -id)) %>%
        replace(is.na(.), 0)
    rownames(IM) <- colnames(IM)

    return(IM)
}
