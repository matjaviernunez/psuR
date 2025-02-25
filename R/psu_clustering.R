#' @import dplyr
#' @import sf
#' @import magrittr
#' @title
#' Function that creates Primary Sampling Units from adjacent polygons.
#' @description
#' This function returns a data.frame object that identifies the adyacent polygons
#' along with the PSU to which they belong.
#' @details
#' The psu_clustering functions takes as inputs an A_{n_xn} incidence matrix and a w_n
#' weight vector (subset of W) related to the n adyacent polygons in A. While creating Primary
#' Sampling Units (PSU), generally w_n takes the number of occupied dwellings in the n
#' block (adyacent polygon) as weight. A_{n_xn} and w_n must be ordered in the
#' same way.
#'
#' The function returns a vector of identifiers of size n that relates the polygons to
#' the PSU which it belongs.
#' @author Angel Gaibor <mat.angel.gaibor at gmail.com>
#' @author Javier Núñez <mat.javier.nunez at gmail.com>
#' @param A a n_n integer matrix, incidence matrix.
#' @param W a two column data.frame, contains the id and weights of the adyacent polygons (generally the id and the number of occupied dwellings in each block).
#' @param lowerLimit an integer, defines the lower limit of the PSU size to be created.
#' @param idp a character, name of id column in W.
#' @param weight a character, name of weights column in W.
#'
#' @references
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas.
#' Valliant, R, et. al. (2013), \emph{Practical tools for Design and Weighting Survey Samples}. Springer
#' @return psu
#' @export
#'
#' @examples
#' # im is created by running the psuR::inc_matrix function with the parameters set in its example.
#' psu_clustering(A = im, W=weights, lowerLimit=60, idp = "idp", weight = "weight")

psu_clustering <- function(A, W, lowerLimit, idp=NULL, weight){

    W <- W %>%
        rename(id = {{idp}})

    B <- A
    s <- W

    n <- dim(s)[1]
    H <- rep(0,n)

    a <- (1:n)[s$weight>=lowerLimit]
    H[a] <- a
    B[a,] <- 0
    B[,a] <- 0

    while(sum(B)>=1){
        d <- rowSums(B)
        d[d==0] <- n
        b <- (1:n)[d==min(d)]
        e <- B[,b]*(1:n)
        e <- e[e>0]
        join <- cbind(node1 = rep(b, each=min(d)), node2=e) %>%
            as.data.frame() %>%
            mutate(size1 = s[node1,2],
                   size2 = s[node2,2],
                   ninc1 = d[node1]+size1/1e4+sample(1:1e6,length(e))/1e11,
                   ninc2 = d[node2]+size2/1e4+sample(1:1e6,length(e))/1e11) %>%
            group_by(node1) %>%
            filter(ninc2 == min(ninc2)) %>%
            ungroup() %>%
            group_by(node2) %>%
            filter(ninc1 == min(ninc1)) %>%
            ungroup() %>%
            mutate(sumninc = ninc1+ninc2,
                   eliminar = "no",
                   size = size1 + size2)

        repeated <- join$node1[join$node1 %in% join$node2]

        if(length(repeated!=0)){
            join_01 <- join %>%
                filter(node1 %in% repeated | node2 %in% repeated)

            join_02 <- join %>%
                filter(!(node1 %in% repeated | node2 %in% repeated))

            for (i in 1:length(repeated)){
                supp <- join_01 %>%
                    filter(node1 == repeated[i] | node2 == repeated[i]) %>%
                    filter(sumninc == min(sumninc))
                if(i==1){
                    join_03 <- supp
                }
                else{
                    join_03 <- rbind(join_03, supp)
                }
            }
            join_03 <- join_03 %>%
                filter(!duplicated(.))

            join <- rbind(join_02, join_03)
        }

        join <- join %>%
            mutate(node = ifelse(node1 %in% H,node1,node2),
                   nodeb = ifelse(node1==node,node2,node1),
                   former = ifelse(nodeb %in% H,1,0))

        H[join$node] <- join$node
        H[join$nodeb] <- join$node

        if(sum(join$former) > 0){
            index1 <- join$nodeb[join$former==1]
            for(i in 1:length(index1)){
                H[H == index1[i]] <- join$node[join$nodeb==index1[i]]
            }
        }

        B[join$node,] <- B[join$node1,] + B[join$node2,]
        B[,join$node] <- B[,join$node1] + B[,join$node2]
        B[join$nodeb,] <- 0
        B[,join$nodeb] <- 0

        s[join$node,2] <- s[join$node1,2] + s[join$node2,2]
        s[join$nodeb,2] <- 0

        psu <- join$node[s[join$node,2] >= lowerLimit]
        B[psu,] <- 0
        B[,psu] <- 0
        B[B>=2] <- 1
        diag(B) <- 0
        print(sum(B))

    }

    r1 <- W %>%
        cbind(psu=H) %>%
        group_by(psu) %>%
        summarise(vivpsu = sum(weight),
                  nma = n())

    isolated <- W %>%
        cbind(psu = H) %>%
        group_by(psu) %>%
        mutate(vivpsu = sum(weight),
               nma = n()) %>%
        ungroup() %>%
        mutate(psuf = ifelse(vivpsu<lowerLimit, 0, psu),
               npol = 1:n)

    B <- A

    B[isolated$psuf==0, isolated$psuf==0] <- 0

    index <- unique(isolated$psuf[isolated$psuf>0])

    for(i in 1:length(index)){
        if(!is.na(sum(isolated$psuf == index[i]))){
            if(sum(isolated$psuf == index[i])>1){
                B[index[i],] <- colSums(B[isolated$psuf == index[i],])
                B[,index[i]] <- rowSums(B[,isolated$psuf == index[i]])
                B[isolated$psuf==index[i] & isolated$npol != index[i],] <- 0
                B[,isolated$psuf==index[i] & isolated$npol != index[i]] <- 0
            }
        }
    }
    B[B>1] <- 1
    diag(B) <- 0

    C <- A
    C[isolated$psuf==0, isolated$psuf!=0] <- 0
    C[isolated$psuf!=0, isolated$psuf==0] <- 0
    C[isolated$psuf!=0, isolated$psuf!=0] <- 0

    while(min(isolated$psuf) == 0 & sum(rowSums(rbind(B[isolated$npol[isolated$psuf == 0],],
                                                      B[isolated$npol[isolated$psuf == 0],]))) > 0){
        if(sum(isolated$psuf == 0) > 1){
            v <- rowSums(B[isolated$npol[isolated$psuf == 0],])
            v <- names(v[v == min(v[v>0])])
        }else{
            v <- isolated$id[isolated$psuf == 0]
        }
        v <- isolated$npol[isolated$id %in% v]
        w <- isolated$weight[isolated$npol %in% v]
        ais <- min(v[w == min(w)])
        candidates <- (1:n)[B[ais,]>0]

        p_can <- min(isolated$vivpsu[candidates])
        ncon <- candidates[isolated$vivpsu[candidates] == p_can]

        isolated <- isolated %>%
            mutate(psuf = ifelse(npol == ais, ncon, psuf)) %>%
            group_by(psuf) %>%
            mutate(vivpsu = sum(weight)) %>%
            ungroup()

        B[ncon, ] <- B[ncon, ] + C[ais, ]
        B[, ncon] <- B[, ncon] + C[, ais]
        C[ais, ] <- 0
        C[, ais] <- 0
        B[B>0] <- 1
        diag(B) <- 0

        print(sum(isolated$psuf == 0))
    }

    psu <- isolated %>%
        mutate(psuf = case_when(psuf == 0 & psu == 0 ~ npol,
                                 psuf == 0 & psu != 0 ~ psu,
                                 T ~ psuf)) %>%
        select(id, weight, psuf) %>%
        rename({{idp}} := id)

    return(psu)
}
