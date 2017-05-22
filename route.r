#########################################################################################################################
#                                                  Routing with R                                                       #
#                                                                                                                       #
# Version: 0.1                                                                                                          #
# Author: Michalis Pavlis                                                                                               #
# Licence: MIT                                                                                                          #
#                                                                                                                       #
# lines_to_graph: Function to create a graph representation (directed, undirected) of the lines network                 #
#                                                                                                                       #
# is_connected: Function to clean the line network by identifying the self-connected line segments                      #
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

.check_values <- function(sf_df, column_name){
  if (any(is.na(sf_df[[column_name]]))) stop(paste("NA values were found in column", column_name))
  if (any(is.null(sf_df[[column_name]]))) stop(paste("NULL values were found in column", column_name))
}

.check_sf <- function(sf_df, geom_column){
  
  if (! is(sf_df, "sf")) stop("object class should be 'sf'")
  
  if (! geom_column %in% names(sf_df)) stop("provide geometry column")
  
  .check_values(sf_df, geom_column)
  
  p4str <- st_crs(sf_df)$proj4string
  if (is.na(p4str) || !nzchar(p4str)) stop("specify coordinate reference system")
  
  res <- grep("longlat", p4str, fixed = TRUE)
  if (length(res) != 0) stop("use projected reference system")
  
}

.check_line <- function(sf_df, geom_column){
  
  .check_sf(sf_df, geom_column)
  
  if (!is(sf_df[[geom_column]], "sfc_LINESTRING")){
    stop("the geometry column class should be 'sfc_LINESTRING'")
  }
  
}

.check_is_connected <- function(lines_sf, geom_column, allpoints, area_id){
  
  .check_line(lines_sf, geom_column)
  
  if (! allpoints %in% c(T, F)) stop("allpoints should be either True or False")
  
  if (!is.null(area_id)){
    if (! area_id %in% names(lines_sf)) stop(paste("the column", area_id, "was not found in lines_sf"))
    check_values(lines_sf, area_id)
  } 
  
}

.check_lines_to_graph <- function(lines_sf, geom_column, allpoints, area_id, weighted_graph,
                                speed_limit_column, direction_column, direction_lookup) {
  
  .check_is_connected(lines_sf, geom_column, allpoints, area_id)
  
  if (! allpoints %in% c(T, F)) stop("allpoints should either be True or False")
  
  if (! weighted_graph %in% c(T, F)) stop("weighted_graph should either be True or False")
  
  if (!is.null(speed_limit_column)){
    if (! speed_limit_column %in% names(lines_sf)) stop(paste("the column", speed_limit_column, "was not found in lines_sf"))
    if (! is(lines_sf[[speed_limit_column]], "numeric")) stop("the class of the speed limit column should be numeric")
    .check_values(lines_sf, speed_limit_column)
  }
  
  if (!is.null(direction_column)){
    if (! direction_column %in% names(lines_sf)) stop(paste("the column", direction_column, "was not found in lines_sf"))
    .check_values(lines_sf, direction_column)
    if (length(unique(lines_sf[[direction_column]])) != 3) stop(paste("only three values are expected in column", direction_column, "that represent back, forward and both traffic direction"))
    if (! length(direction_lookup[[1]]) == 3) stop("provide exactly three values for the first vector in the direction_lookup list: forward, opposite and both")
    if (! length(direction_lookup[[2]]) == 3) stop("provide exactly three values for the second vector in the direction_lookup list")
    if (! all(c("forward","opposite", "both") %in% direction_lookup[[1]])) stop("the only values expected in the first vector of direction_lookup are: forward, opposite and both")
    if (! all(direction_lookup[[2]] %in% unique(lines_sf[[direction_column]]))) stop(paste("the values provided in the second column of the direction_lookup table do not match with the values in", direction_column))
  }
}


.check_shortest_route <- function(origins_sf, destinations_sf, geom_column, id_column, lines_graph, lookup_table, join_by){
  
  .check_sf(origins_sf, geom_column)
  if (! is(origins_sf$geometry, "sfc_POINT")) stop("the class of the geometry column in origins_sf should be 'sfc_POINT'")
  
  .check_sf(destinations_sf, geom_column)
  if (! is(destinations_sf$geometry, "sfc_POINT")) stop("the class of the geometry column in destinations_sf should be 'sfc_POINT'")
  
  if (! identical(st_crs(origins_sf)$proj4string, st_crs(destinations_sf)$proj4string)){
    stop("origin and destination points are not in the same reference system")
  }
  
  if (! identical(st_crs(origins_sf)$proj4string, attributes(lines_graph)$proj4string)){
    stop("the road network is not in the same reference system with the point data")
  }
  
  if (! id_column %in% names(origins_sf)) stop("provide id field for origins_sf")
  .check_values(origins_sf, id_column)
  
  if (! id_column %in% names(destinations_sf)) stop("provide id field for destinations_sf")
  .check_values(destinations_sf, id_column)
  
  if (!is.null(lookup_table)){
    if (ncol(lookup_table) != 2){
      stop("the lookup table should have two columns")
    }
    if (!any(unique(origins_sf[[id_column]]) %in% unique(lookup_table[,1]))){
      stop(paste("there are no matching values between the field", id_column, "in origins_sf and the first column in the lookup_table"))
    }
    if (!any(unique(destinations_sf[[id_column]]) %in% unique(lookup_table[,2]))){
      stop(paste("there are no matching values between the", id_column, "field in destinations_sf and the second column in the lookup_table"))
    }
    .check_values(lookup_table, colnames(lookup_table)[1])
    .check_values(lookup_table, colnames(lookup_table)[2])
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
    .check_values(origins_sf, join_by)
    .check_values(destinations_sf, join_by)
  }
}

#########################################################################################################################
#                                      Create graph from lines network                                                  #
#########################################################################################################################

lines_to_graph <- function(lines_sf, geom_column, allpoints = T,  area_id = NULL,
                           weighted_graph = F, speed_limit_column = NULL, direction_column = NULL,
                           direction_lookup = list(c("forward","opposite", "both"), c("F","T","B"))){
  
  ##### 1. Edge list functions ##########################################################################################
  
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
  
  ##### 2. Check inputs #################################################################################################
  
  .check_lines_to_graph(lines_sf, geom_column, allpoints, area_id, weighted_graph, speed_limit_column, direction_column, direction_lookup)
  
  ##### 3. Build edge list ##############################################################################################
  
  if (is.null(direction_column)){
    if (! allpoints){
	  edgelist <- do.call(rbind, lapply(1:nrow(lines_sf), function(x) edge_list_endpoints(unclass(lines_sf[[geom_column]][[x]]))))
    } else {
      edgelist <- do.call(rbind, lapply(1:nrow(lines_sf), function(x) cbind(x, edge_list_allpoints(unclass(lines_sf[[geom_column]][[x]])))))
    }
  } else {
    forward <- direction_lookup[[2]][which(direction_lookup[[1]] == "forward" | direction_lookup[[1]] == "both")]
    id_forward <- which(lines_sf[[direction_column]] %in% forward)
    
    opposite <- direction_lookup[[2]][which(direction_lookup[[1]] == "opposite" | direction_lookup[[1]] == "both")]
    id_opposite <- which(lines_sf[[direction_column]] %in% opposite)
    edgelist <- rbind(do.call(rbind, lapply(id_forward,
                                            function(x) cbind(x, edge_list_allpoints(unclass(lines_sf[[geom_column]][[x]]))))),
                      do.call(rbind, lapply(id_opposite,
                                            function(x) cbind(x, edge_list_allpoints_back(unclass(lines_sf[[geom_column]][[x]]))))))
  }
  
  ##### 4. Build graph ###################################################################################################
  
  if (is.null(direction_column)){
    lines_graph <- graph.edgelist(cbind(paste(edgelist[, 2], edgelist[, 3]), paste(edgelist[, 4], edgelist[, 5])), directed = F)
  } else {
    lines_graph <- graph.edgelist(cbind(paste(edgelist[, 2], edgelist[, 3]), paste(edgelist[, 4], edgelist[, 5])), directed = T)
  }
  E(lines_graph)$id <- edgelist[,1]
  if (weighted_graph){
    edge_length <- sqrt((edgelist[, 2] - edgelist[, 4]) ^ 2 + (edgelist[, 3] - edgelist[, 5]) ^ 2) / 1000
    if (!is.null(speed_limit_column)){
      edge_length <- ifelse(edge_length < 0.00001, 0.00001, edge_length)
      E(lines_graph)$weight <- 60 * edge_length / lines_sf[[speed_limit_column]][edgelist[,1]]
    } else {
      E(lines_graph)$weight <- edge_length
    }
  }
  
  # set area id as graph attribute
  if (!is.null(area_id)){
    E(lines_graph)$area_id <- lines_sf[[area_id]][edgelist[, 1]]
  }
  
  # V(lines_graph)$name <- 1:length(V(lines_graph))
  
  attr(lines_graph, "proj4string") <- st_crs(lines_sf)$proj4string
  
  lines_graph
}

#########################################################################################################################
#                                      Get connected parts of the road network                                          #
#########################################################################################################################

is_connected <- function(lines_sf, geom_column, allpoints = T, area_id = NULL, cores_nr = 1){
  
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
  
  ##### 2. Check inputs #################################################################################################
  
  .check_is_connected(lines_sf, geom_column, allpoints, area_id)
  
  ##### 3. Main Function ################################################################################################
  
  if (!is.null(area_id)){
    lines_graph <- lines_to_graph(lines_sf = lines_sf, geom_column = geom_column, allpoints = allpoints, area_id = area_id)
    edges_id <- unlist(mclapply(unique(lines_sf[[area_id]]), 
                                       function(x) connected_edges(subgraph.edges(lines_graph, which(E(lines_graph)$area_id == x))),
                                       mc.cores = cores_nr))
  } else {
    lines_graph <- lines_to_graph(lines_sf = lines_sf, geom_column = geom_column, allpoints = allpoints)
    edges_id <- connected_edges(lines_graph)
  }
  
  connected <- 1:nrow(lines_sf) %in% edges_id
  
  connected
  
}


#########################################################################################################################
#                                                Shortest Route Cost                                                    #
#########################################################################################################################

shortest_route_cost <- function(origins_sf, destinations_sf, lines_graph, geom_column, id_column, join_by = NULL, lookup_table = NULL, cores_nr = 1){
  
  ##### 1. Function #####################################################################################################
  
  calc_cost <- function(origins_id, destinations_id){
    
    o_node_ids <- origin_points[.(unique(origins_id)), .(get(id_column), origins_node_id), on = id_column, allow.cartesian = T]
    setnames(o_node_ids, 1, id_column)
    d_node_ids <- destination_points[.(unique(destinations_id)), .(get(id_column), destinations_node_id), on = id_column, allow.cartesian = T]
    setnames(d_node_ids, 1, id_column)
    
    o_unique_node_ids <- unique(o_node_ids$origins_node_id)
    d_unique_node_ids <- unique(d_node_ids$destinations_node_id)
    
    path_DT <- rbindlist(mclapply(o_unique_node_ids, function(o_id) list(rep(o_id, length(d_unique_node_ids)),
                                                                         d_unique_node_ids,
                                                                         as.numeric(shortest.paths(lines_graph, v = o_id, to = d_unique_node_ids))),
                                  mc.cores = cores_nr))
    setnames(path_DT, names(path_DT), c("origins_node_id", "destinations_node_id", "cost"))
    path_DT <- path_DT[o_node_ids, on = "origins_node_id", allow.cartesian = T]
    path_DT <- path_DT[d_node_ids, on = "destinations_node_id", allow.cartesian = T]
    setkeyv(path_DT, c(id_column, paste0("i.", id_column)))
    path_DT <- path_DT[, min(cost), by=c(id_column, paste0("i.", id_column))]
    
    path_DT$V1
  }
  
  ##### 2. Check inputs #################################################################################################
  
  .check_shortest_route(origins_sf, destinations_sf, geom_column, id_column, lines_graph, lookup_table, join_by)
  
  if (nrow(origins_sf) > nrow(destinations_sf) & !is_directed(lines_graph) & is.null(lookup_table)){
    origin_points <- destinations_sf
    destination_points <- origins_sf
  } else {
    origin_points <- origins_sf
    destination_points <- destinations_sf
  }
  
  ##### 3. Map origins and destinations to road network #################################################################
  
  line_vertices <- do.call(cbind, tstrsplit(V(lines_graph)$name, " "))
  storage.mode(line_vertices) <- "numeric"
  # Map origin and destination nodes to closest nodes
  setDT(origin_points)
  setDT(destination_points)
  origin_points[, origins_node_id := get.knnx(line_vertices, do.call(rbind, unclass(origin_points[[geom_column]])), 1)$nn.index[,1]]
  destination_points[, destinations_node_id := get.knnx(line_vertices, do.call(rbind, unclass(destination_points[[geom_column]])), 1)$nn.index[,1]]
  
  V(lines_graph)$name <- seq_along(V(lines_graph))
  
  ##### 4. Calculate shortest route cost ################################################################################
  
  setkeyv(origin_points, id_column)
  setkeyv(destination_points, id_column)
  
  if (!is.null(lookup_table)){
    out_DT <- as.data.table(lookup_table)
  } else  if (!is.null(join_by)){
    out_DT <- origin_points[, unique(id), keyby = join_by][destination_points[, unique(id), keyby = join_by], on = join_by, allow.cartesian = T]
  } else {
    unique_origins <- unique(origin_points[[id_column]])
    unique_destinations <- unique(destination_points[[id_column]])
    out_DT <- data.table(origins_id = rep(unique_origins, length(unique_destinations)),
                         destinations_id = do.call(rbind, lapply(unique_destinations, 
                                                                 function(x) cbind(rep(x, length(unique_origins))))))
  }
  
  setnames(out_DT, 1:2, c("origins_id", "destinations_id"))
  setkeyv(out_DT, c("origins_id", "destinations_id"))
  out_DT[, cost := calc_cost(origins_id, destinations_id), by = origins_id]
  
  if (nrow(origins_sf) > nrow(destinations_sf) & !is_directed(lines_graph) & is.null(lookup_table)){
    setnames(out_DT, c("origins_id", "destinations_id"), c("destinations_id", "origins_id"))
  }
  
  out_DT
  
}
