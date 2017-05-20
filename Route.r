#########################################################################################################################
#                                                  Routing with R                                                       #
#                                                                                                                       #
# Version: 0.1                                                                                                          #
# Author: Michalis Pavlis                                                                                               #
# Licence: MIT                                                                                                          #
#                                                                                                                       #
# road_to_graph: Function to create a graph representation (directed, undirected) of the road network                   #
#                                                                                                                       #
# is_connected: Function to clean the road network by identifying the self-connected line segments                      #
#                                                                                                                       #
# shortest_route_cost: Function to calculate the shortest route cost either in kilometres or minutes                    #
#                                                                                                                       #
#########################################################################################################################

library(igraph)
library(FNN)
library(data.table)
library(sf)
library(parallel)

#########################################################################################################################
#                                           Check functions' arguments                                                  #
#########################################################################################################################

check_sf <- function(sf_df, geom_column){
  
  if (! is(sf_df, "sf")) stop("object class should be 'sf'")
  
  if (! geom_column %in% names(sf_df)) stop("provide geometry column")
  
  p4str <- st_crs(sf_df)$proj4string
  if (is.na(p4str) || !nzchar(p4str)) stop("specify coordinate reference system")
  
  res <- grep("longlat", p4str, fixed = TRUE)
  if (length(res) != 0) stop("use projected reference system")
  
}

check_line <- function(sf_df, geom_column){
  
  check_sf(sf_df, geom_column)
  
  if (!is(sf_df[[geom_column]], "sfc_LINESTRING")){
    stop("the geometry column class should be 'sfc_LINESTRING'")
  } 
}

check_shortest_route <- function(origins_sf, destinations_sf, geom_column, road_graph, lookup_table, join_by){
  
  check_sf(origins_sf, geom_column)
  if (! is(origins_sf$geometry, "sfc_POINT")) stop("the class of the geometry column in origins_sf should be 'sfc_POINT'")
  
  check_sf(destinations_sf, geom_column)
  if (! is(destinations_sf$geometry, "sfc_POINT")) stop("the class of the geometry column in destinations_sf should be 'sfc_POINT'")
  
  if (! identical(st_crs(origins_sf)$proj4string, st_crs(destinations_sf)$proj4string)){
    stop("origin and destination points are not in the same reference system")
  }
  
  if (! identical(st_crs(origins_sf)$proj4string, attributes(road_graph)$proj4string)){
    stop("the road network is not in the same reference system with the point data")
  }
  
  if (! "id" %in% names(origins_sf)) stop("provide id field for origins_sf")
  
  if (! "id" %in% names(destinations_sf)) stop("provide id field for destinations_sf")
  
  if (!is.null(lookup_table)){
    if (ncol(lookup_table) != 2){
      stop("the lookup table should have two columns")
    }
    if (!any(unique(origins_sf$id) %in% unique(lookup_table[,1]))){
      stop("there are no matching values between the id field in origins_sf and the first column in lookup_table")
    }
    if (!any(unique(destinations_sf$id) %in% unique(lookup_table[,2]))){
      stop("there are no matching values between the id field in destinations_sf and the second column in lookup_table")
    }
    if (!is.null(join_by)){
      warning("both lookup_table and join_by were provided, ignoring join_by")
    }
  }
  
  if (!is.null(join_by)){
    if (! join_by %in% names(origins_sf)){
      stop(paste("the field", join_by, "was not found in origins_sf"))
    }
    if (! join_by %in% names(destinations_sf)){
      stop(paste("the field", join_by, "was not found in destinations_sf"))
    }
    if (! any(origins_sf[[join_by]] %in% destinations_sf[[join_by]])){
      stop(paste("you are trying to join origins_sf and destinations_sf by", join_by, "but there are no common values"))
    }
  }
}

#########################################################################################################################
#                                      Create graph from road network                                                   #
#########################################################################################################################

road_to_graph <- function(road_sf, geom_column, allpoints = T,  area_id = NULL,
                          weighted_graph = F, speed_limit_column = NULL, direction_column = NULL,
                          direction_lookup = list(c("forward","opposite", "both"), c("F","T","B"))){
  
  ##### 1. Check function arguments #####################################################################################
  
  check_line(road_sf, geom_column)
  
  if (! allpoints %in% c(T, F)) stop("allpoints should either be True or False")
  
  ##### 2. Edge list functions ###########################################################################################
  
  edge_list_endpoints <- function(coords){
    n <- nrow(coords)
    c(paste(coords[1, ], collapse = " "), paste(coords[n, ], collapse = " "))
  }
  
  edge_list_allpoints <- function(coords){
    n <- nrow(coords)
    cbind(coords[-n, 1], coords[-n, 2], coords[-1, 1], coords[-1, 2])
  }
  
  edge_list_allpoints_back <- function(coords){
    n <- nrow(coords)
    coords <- coords[n:1, ]
    cbind(coords[-n, 1], coords[-n, 2], coords[-1, 1], coords[-1, 2])
  }
  
  ##### 3. Build edge list ###############################################################################################
  
  if (is.null(direction_column)){
    if (! allpoints){
	  edgelist <- do.call(rbind, lapply(1:nrow(road_sf), function(x) edge_list_endpoints(unclass(road_sf[[geom_column]][[x]]))))
    } else {
      edgelist <- do.call(rbind, lapply(1:nrow(road_sf), function(x) cbind(x, edge_list_allpoints(unclass(road_sf[[geom_column]][[x]])))))
    }
  } else {
    forward <- direction_lookup[[2]][which(direction_lookup[[1]] == "forward" | direction_lookup[[1]] == "both")]
    id_forward <- which(road_sf[[direction_column]] %in% forward)
    
    opposite <- direction_lookup[[2]][which(direction_lookup[[1]] == "opposite" | direction_lookup[[1]] == "both")]
    id_opposite <- which(road_sf[[direction_column]] %in% opposite)
    edgelist <- rbind(do.call(rbind, lapply(id_forward,
                                            function(x) cbind(x, edge_list_allpoints(unclass(road_sf[[geom_column]][[x]]))))),
                      do.call(rbind, lapply(id_opposite,
                                            function(x) cbind(x, edge_list_allpoints_back(unclass(road_sf[[geom_column]][[x]]))))))
  }
  
  ##### 4. Build graph ###################################################################################################
  
  if (is.null(direction_column)){
    road_graph <- graph.edgelist(cbind(paste(edgelist[, 2], edgelist[, 3]), paste(edgelist[, 4], edgelist[, 5])), directed = F)
  } else {
    road_graph <- graph.edgelist(cbind(paste(edgelist[, 2], edgelist[, 3]), paste(edgelist[, 4], edgelist[, 5])), directed = T)
  }
  E(road_graph)$id <- edgelist[,1]
  if (weighted_graph){
    edge_length <- sqrt((edgelist[, 2] - edgelist[, 4]) ^ 2 + (edgelist[, 3] - edgelist[, 5]) ^ 2) / 1000
    if (!is.null(speed_limit_column)){
      edge_length <- ifelse(edge_length < 0.00001, 0.00001, edge_length)
      E(road_graph)$weight <- 60 * edge_length / road_sf[[speed_limit_column]][edgelist[,1]]
    } else {
      E(road_graph)$weight <- edge_length
    }
  }
  
  # set area id as graph attribute
  if (!is.null(area_id)){
    E(road_graph)$area_id <- road_sf[[area_id]][edgelist[, 1]]
  }
  
  # V(road_graph)$name <- 1:length(V(road_graph))
  
  attr(road_graph, "proj4string") <- st_crs(road_sf)$proj4string
  
  road_graph
}

#########################################################################################################################
#                                      Get connected parts of the road network                                          #
#########################################################################################################################

is_connected <- function(road_sf, geom_column, allpoints = T, area_id = NULL, cores_nr = 1){
  
  ##### 1. Functions ####################################################################################################
  
  connected_edges <- function(G){
    graph_parts <- decompose.graph(G, min.vertices = 2)
    if (length(graph_parts) > 1){
      # Find road part with the highest number of vertices, extract id instead of name
      edges_id <- unique(E(graph_parts[[which.max(sapply(1:length(graph_parts), function(x) length(V(graph_parts[[x]]))))]])$id)
      return(edges_id)
    }
    return(unique(E(G)$id))
  }
  
  ##### 2. Main Function ################################################################################################
  
  if (!is.null(area_id)){
    road_graph <- road_to_graph(road_sf = road_sf, geom_column = geom_column, allpoints = allpoints, area_id = area_id)
    edges_id <- unlist(mclapply(unique(road_sf[[area_id]]), 
                                       function(x) connected_edges(subgraph.edges(road_graph, which(E(road_graph)$area_id == x))),
                                       mc.cores = cores_nr))
  } else {
    road_graph <- road_to_graph(road_sf = road_sf, geom_column = geom_column, allpoints = allpoints)
    edges_id <- connected_edges(road_graph)
  }
  
  connected <- 1:nrow(road_sf) %in% edges_id
  
  connected
  
}


#########################################################################################################################
#                                                Shortest Route Cost                                                    #
#########################################################################################################################

shortest_route_cost <- function(origins_sf, destinations_sf, road_graph, geom_column, join_by = NULL, lookup_table = NULL, cores_nr = 1){
  
  ##### 1. Function #####################################################################################################
  
  calc_path <- function(origins_id, destinations_id){
    
    o_node_ids <- origin_points[.(unique(origins_id)), .(id, origins_node_id), on = "id", allow.cartesian = T]
    d_node_ids <- destination_points[.(unique(destinations_id)), .(id, destinations_node_id), on = "id", allow.cartesian = T]
    
    o_unique_node_ids <- unique(o_node_ids$origins_node_id)
    d_unique_node_ids <- unique(d_node_ids$destinations_node_id)
    
    path_DT <- rbindlist(mclapply(o_unique_node_ids, function(o_id) list(rep(o_id, length(d_unique_node_ids)),
                                                                         d_unique_node_ids,
                                                                         as.numeric(shortest.paths(road_graph, v = o_id, to = d_unique_node_ids))),
                                  mc.cores = cores_nr))
    setnames(path_DT, names(path_DT), c("origins_node_id", "destinations_node_id", "cost"))
    path_DT <- path_DT[o_node_ids, on = "origins_node_id", allow.cartesian = T]
    path_DT <- path_DT[d_node_ids, on = "destinations_node_id", allow.cartesian = T]
    setkeyv(path_DT, c("id", "i.id"))
    path_DT <- path_DT[, min(cost), by=c("id", "i.id")]
    
    path_DT$V1
  }
  
  ##### 2. Check inputs #################################################################################################
  
  check_shortest_route(origins_sf, destinations_sf, geom_column, road_graph, lookup_table, join_by)
  
  if (nrow(origins_sf) > nrow(destinations_sf) & !is_directed(road_graph) & !is.null(lookup_table)){
    origin_points <- destinations_sf
    destination_points <- origins_sf
  } else {
    origin_points <- origins_sf
    destination_points <- destinations_sf
  }
  
  ##### 3. Map origins and destinations to road network ######################################################################
  
  road_vertices <- do.call(cbind, tstrsplit(V(road_graph)$name, " "))
  storage.mode(road_vertices) <- "numeric"
  # Map origin and destination nodes to closest nodes
  setDT(origin_points)
  setDT(destination_points)
  origin_points[, origins_node_id := get.knnx(road_vertices, do.call(rbind, unclass(origin_points[[geom_column]])), 1)$nn.index[,1]]
  destination_points[, destinations_node_id := get.knnx(road_vertices, do.call(rbind, unclass(destination_points[[geom_column]])), 1)$nn.index[,1]]
  
  V(road_graph)$name <- seq_along(V(road_graph))
  
  ##### 4. Calculate shortest route cost #####################################################################################
  
  setkey(origin_points, id)
  setkey(destination_points, id)
  
  if (!is.null(lookup_table)){
    out_DT <- as.data.table(lookup_table)
  } else  if (!is.null(join_by)){
    out_DT <- origin_points[, unique(id), keyby = join_by][destination_points[, unique(id), keyby = join_by], on = join_by, allow.cartesian = T]
  } else {
    unique_origins <- unique(origin_points$id)
    unique_destinations <- unique(destination_points$id)
    out_DT <- data.table(origins_id = rep(unique_origins, length(unique_destinations)),
                         destinations_id = do.call(rbind, lapply(unique_destinations, 
                                                                 function(x) cbind(rep(x, length(unique_origins))))))
  }
  
  setnames(out_DT, 1:2, c("origins_id", "destinations_id"))
  setkeyv(out_DT, c("origins_id", "destinations_id"))
  out_DT[, cost := calc_path(origins_id, destinations_id), by = origins_id]
  
  if (nrow(origins_sf) > nrow(destinations_sf) & !is_directed(road_graph) & !is.null(lookup_table)){
    setnames(out_DT, c("origins_id", "destinations_id"), c("destinations_id", "origins_id"))
  }
  
  out_DT
  
}
