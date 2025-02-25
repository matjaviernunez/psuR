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
        rename(id = {{idp}})

    centroids <- pol %>%
        st_centroid(.)

    x <- as.numeric(st_coordinates(centroids)[,1])
    y <- as.numeric(st_coordinates(centroids)[,2])

    data <- as.data.frame(cbind(x,y)) %>%
        mutate(x=as.numeric(x),
               y=as.numeric(y))

    n <- length(x)

    if(n>1){
        A <- inc_matrix(por, tol = 0, id = "id")
        diag(A) <- 0
        com_x <- matrix(0,n,n)
        com_y <- matrix(0,n,n)
        dis <- matrix(0,n,n)
        for (i in 1:n) {
            com_x[i,] <- x-x[i]
            com_y[i,] <- y-y[i]
            dis[i,] <- sqrt((x-x[i])^2+(y-y[i])^2)
        }

        h <- rep(0,n)

        pol_0 <- match(max(y),y)

        i=1
        h[pol_0]=i
        y[pol_0]=min(y)-1

        while (min(h)==0){
            i=i+1

            boun <- c(1:dim(A)[2])[A[pol_0,]==1]

            if(length(boun)!=0){
                pol_1 <- boun[match(max(com_y[pol_0,boun]),pol_y[man_0,boun])]
            }
            else{
                pol_1 <- match(max(y),y)
            }
            y[pol_1]=min(y)-1
            A[pol_0,]=0
            A[,pol_0]=0
            pol_0=pol_1
            h[pol_0]=i
            print(pol_0)
        }
        polnum <- cbind(pol,order=h) %>%
            rename({{idp}} := id)
    }
    if(n==1){
        polnum <- cbind(pol,order=1) %>%
            rename({{idp}} := id)
    }
    return(polnum)
}
