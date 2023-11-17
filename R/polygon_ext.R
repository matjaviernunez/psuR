#' @import dplyr
#' @import sf
#' @import magrittr
#' @title
#' Function that extends the polygons based on geographic data related to blocks.
#' @description
#' This function allows for extending the polygons prior to the formation of Primary Sampling Units.
#' @details
#' polygon_ext is a function that extends non-adjacent polygons until they share a common boundary.
#'
#' The objective of the extension is to identify which polygons share a common boundary, facilitating
#' the delimitation of Primary Sampling Units, which are generally composed of adjacent polygons sets.
#'
#' The extension is carried out by generating Voronoi Polygons from a inner points mesh constructed
#' from the buff and density parameters. Higher values require greater computational processing capacity,
#' however they can drastically improve the quality of the result.
#'
#' Using the gap parameter, unwanted incidences between extended polygons can be eliminated (for example,
#' large avenues or rivers that run through the city).
#' @author Angel Gaibor <mat.angel.gaibor at gmail.com>
#' @author Javier Núñez <mat.javier.nunez at gmail.com>
#' @param poly an sf polygon object, geographic data of non-adjacent polygons generally related to blocks.
#' @param boundary an sf polygon object, that limits the area where polygons will be extended.
#' @param id a character string, the column name of the ID related to blocks in poly argument, preferably unique by block.
#' @param gap an multi-polygon sf object, geographic data that represents rivers or boundaries, used to avoid undesired connections between blocks, could be contained within the boundary.
#' @param buff an integer (buffer width) that determine the quality of the polygon extension. Higher quality requires more processing power.
#' @param density an integer (number of points by 1 meter) that determine the quality of the polygon extension. Higher quality requires more processing power.
#'
#' @references
#' Gutierrez, H. A. (2009), \emph{Estrategias de muestreo: Diseno de encuestas y estimacion de parametros}. Editorial Universidad Santo Tomas.
#' Valliant, R, et. al. (2013), \emph{Practical tools for Design and Weighting Survey Samples}. Springer
#' @return
#' @export
#'
#' @examples

polygon_ext <- function(poly, boundary, id = NULL, gap = NULL, buff = 5, density = 0.1){

    box <- function(points){
        bb <- st_bbox(points)
        p <- matrix(
            c(bb["xmin"],bb["ymin"],
              bb["xmin"],bb["ymax"],
              bb["xmax"],bb["ymax"],
              bb["xmax"],bb["ymin"],
              bb["xmin"],bb["ymin"]),
            ncol=2,byrow = T
        )
        box <- st_polygon(list(p))
        return(box)
    }

    print(paste0("Polygon extension has begun: ", Sys.time()))

    poly <- poly %>%
        rename(id = {{ id }})

    index <- unique(poly$id)
    quntosi <- vector("list", 0)

    for(i in 1:length(index)){

        pol <- poly %>%
            filter(id == index[i])

        pol1 <- st_buffer(pol, -gap) %>%
            summarise()

        pol2 <- st_difference(pol, pol1)

        quntos1 <- pol2 %>%
            st_simplify(dTolerance = 0.9) %>%
            mutate(area = as.numeric(st_area(.)),
                   npoints = ceiling(area/(1/density))) %>%
            st_cast("MULTIPOLYGON") %>%
            st_sample(size = sum(.$npoints), type = "hexagonal") %>%
            st_as_sf() %>%
            st_cast("POINT") %>%
            st_coordinates() %>%
            as.data.frame() %>%
            st_as_sf(coords = c("X","Y"))

        st_crs(quntos1) <- st_crs(pol)

        quntos_iden <- quntos1 %>%
            st_join(pol %>% select(id), join = st_within) %>%
            filter(!is.na(id))

        names(quntos_iden)[names(quntos_iden) == attr(quntos_iden, "sf_column")] = attr(pol, "sf_column")
        st_geometry(quntos_iden) <- attr(pol, "sf_column")

        quntosi[[i]] <- pol %>%
            st_buffer(-0.2) %>%
            st_simplify(dTolerance = 0.9) %>%
            st_as_sf() %>%
            st_cast("MULTIPOINT") %>%
            st_cast("POINT") %>%
            filter(!is.na(id)) %>%
            select(id) %>%
            rbind(quntos_iden)

        print(length(index) - i)
    }

    quntos <- do.call(rbind, quntosi)
    points <- st_union(quntos)
    st_crs(points) <- st_crs(poly)
    st_crs(quntos) <- st_crs(poly)
    print(paste0("Point mesh have been created: ", Sys.time()))

    caj <- box(points)

    voronoi <- st_voronoi(points,caj) %>%
        st_cast() %>%
        st_as_sf() %>%
        st_join(.,quntos, join = st_contains) %>%
        mutate(area = as.numeric(st_area(.)))
    print(paste0("Voronoi polygons have been created: ", Sys.time()))

    vorpun <- voronoi
    dissolve <- voronoi %>%
        st_make_valid() %>%
        group_by(id) %>%
        summarise(np=n()) %>%
        st_cast() %>%
        filter(!is.na(id))
    st_crs(dissolve) <- st_crs(pol)
    print(paste0("Voronoi polygons have been dissolved: ", Sys.time()))

    polext <- st_intersection(dissolve, boundary) %>%
        group_by(id) %>%
        summarise() %>%
        st_cast()
    print(paste0("Extended polygons haven been cut off by boundary: ", Sys.time()))

    if(!is.null(gap)){
        polext <- polext %>%
            st_difference(gap) %>%
            st_buffer(0) %>%
            st_make_valid() %>%
            st_cast("MULTIPOLYGON") %>%
            st_cast("POLYGON") %>%
            st_join(poly) %>%
            filter(id.x == id.y) %>%
            group_by(id = id.x) %>%
            summarise() %>%
            st_make_valid() %>%
            st_cast("MULTIPOLYGON")
    }

    polext <- polext %>%
        rename({{ id }} := id) %>%
        st_as_sf()
    names(polext)[names(polext)==attr(polext, "sf_column")] = "geom"
    st_geometry(polext)="geom"

    rm(quntosi, pol, pol1, pol2, quntos1, quntos_iden, points, voronoi, vorpun, dissolve, quntos)

    return(polext)
}
