#' @import dplyr
#' @title
#' Function that creates a numbering for adjacent polygons.
#' @description
#' This function generates a variable in a geographic object that ranges from 1
#' to n that follows the shape of a snake.
#' @details
#' The polygon_enumerating function takes a geographic object as input, the centroids
#'  of the polygons are calculated and the numbering begins with the polygon whose
#' centroid is located most to the northeast, the numbering continues with the polygon
#' adjacent to the numbered polygon whose centroid is located most to the north.
#'
#' The function returns the geographic object with a variable numbered from 1 to n.
#' @author Angel Gaibor <mat.angel.gaibor at gmail.com>
#' @author Javier Núñez <mat.javier.nunez at gmail.com>
#' @param pol a sf polygon object, extended polygons that share a common boundary.
#' @param idp a character, name of id column in W.
#'
#' @references
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas.
#' Valliant, R, et. al. (2013), \emph{Practical tools for Design and Weighting Survey Samples}. Springer
#' @return pol_num
#' @export
#'
#' @examples
#' # polnum is created by running the psuR::polygon_enumarating function with the parameters set in its example.
#' polygon_enumerating(poly = ext_pol, idp = "idp")




polygon_enumerating <- function(pol, idp = NULL){

    pol <- pol %>%
        rename(id = {{idp}}) |>
        arrange(id)

    centroids <- pol %>%
        st_centroid(.)

    x <- as.numeric(st_coordinates(centroids)[,1])
    y <- as.numeric(st_coordinates(centroids)[,2])
    n <- length(x)

    if(n>1){
        A <- inc_matrix(pol, tol = 10, id = "id")

        diag(A) <- 0
        dis <- matrix(0,n,n)
        for (i in 1:n) {
            dis[i,] <- sqrt((x-x[i])^2+(y-y[i])^2)
            print(i)
        }

        h <- rep(0,n)

        pol_0 <- match(max(y),y)

        i=1
        h[pol_0]=i
        y[pol_0]=min(y)-1

        while (min(h)==0){
            i=i+1
            aux <- (1:n)[A[pol_0,] > 0 & h == 0]
            if(length(aux) == 1){
                aux1 <- sum(A[aux, h != 0])
            }else{
                aux1 <- rowSums(as.matrix(cbind(A[aux, h != 0], rep(0, length(aux)))))
            }

            boun <- aux
            if(length(boun)!=0){
                apo <- rowSums(matrix(A[boun, h == 0], nrow = length(boun), ncol = sum(h == 0)))
                boun <- boun[match(min(apo), apo)]
                pol_1 <- boun[match(max(y[boun]),y[boun])]
            }else{
                boun <- (1:n)[colSums(rbind(A[h > 0, ]), rep(0, n)) > 0]
                pol_1 <- boun[match(min(dis[pol_0, boun]), dis[pol_0, boun])]
            }
            y[pol_1]=min(y)-1

            pol_0=pol_1
            h[pol_0]=i
            A[h!=0,h!=0]=0
        }
        polnum <- cbind(pol |>  arrange(id),order=h) %>%
            rename({{idp}} := id)
    }
    if(n==1){
        polnum <- cbind(pol |>  arrange(id),order=1) %>%
            rename({{idp}} := id)
    }
    return(polnum)
}
