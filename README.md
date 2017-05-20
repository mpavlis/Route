# Route.r

The script provides functions to create a graph representation of the road network, to clean the road network by identifying the self-connected line segments and to calculate the shortest path cost either in kilometres or minutes.

### Function 1: road_to_graph

Description: This function is used to create a graph representation of the road network. The graph can be directed or undirected, weighted by the length of the linestrings (in kilometres) or weighted by the travel time (in minutes). Optionally the graph can have a character attribute that identifies the polygon boundaries that intersect or contain the linestrings of the road network.

8 arguments: road_sf (required), geom_column (required), allpoints (not required), area_id (not required), weighted_graph (not required), speed_limit_column (not required), direction_column (not required), direction_lookup (not required).

- road_sf: The road network, it should be an object of class 'sf'.
- geom_column: Character, the name of the geometry column, the column itself should be of class 'sfc_linestring' and in a projected reference system.
- allpoints: Boolean (default value is True), if True all the points of the linestrings are used to create the graph representation of the road network, if False only the endpoints are used.
- area_id: Character (default NULL), column name of the road_sf dataset, the values of the column can be either numeric or character and will be used as graph attribute. In the case of a study area consisting of unconnected subareas the column values should identify the subareas that the linestrings of the road netwrok intersect with. It can be used by the is_connected function.
- weighted_graph: Boolean (default is False), if True the length (in kilometres) of the linestrings of the road network is used as graph attribute.
- speed_limit_column: Character (default NULL), column name of the road_sf dataset. The column should provide the speed limits of the road network in kilometers/hour.
- direction_column: Character (default NULL), column name of the road_sf dataset. The column should provide the direction of traffic as character.
- direction_lookup: list object, (default list(c("forward","opposite", "both"), c("F","T","B")) ). The characters of the first vector of the list should always be  "forward", "opposite" and "both", the characters of the second vector represent the corresponding values of the direction_column that indicate traffic direction. The default values "F","T","B" are used by the OpenStreetMap road network.

Returns: Object of class 'igraph'

### Function 2: is_connected

Description: Function to identify the self-connected linestrings of the road network. It is advised to use this function in order to remove the linestrings that are not connected to the main part of the road network. If the area_id argument is used the function will be executed for each unique area. This can be useful if for example there are islands that are unconnected with the mainland and with each other.

5 arguments: road_sf (required), geom_column (required), allpoints (not required), area_id (not required), cores_nr (not required).

- road_sf: The road network, it should be an object of class 'sf'.
- geom_column: Character, the name of the geometry column, the column itself should be of class 'sfc_linestring' and in a projected reference system.
- allpoints: Boolean (default value is True), if True all the points of the linestrings are used to create the graph representation of the road network, if False only the endpoints are used. Set allpoints to False if the road network is topologically correct and the linestrings are connected at the endpoints. Otherwise (if you are using the OSM road network for example) use the default value (True). If the graph will be used as input for the shortest_route_cost function allways set allpoints equal to True.
- area_id: Character (default NULL), column name of the road_sf dataset, the values of the column can be either numeric or character and will be used as graph attribute. In the case of a study area consisting of unconnected subareas the column values should identify the subareas that the linestrings of the road netwrok intersect with.
- cores_nr: Integer (default 1). The number of processor cores to use. The mclapply function of the parallel library is used to process the data in parallel, therefore it can only be used in Unix systems. The more cores are used the more memory is required.

Returns: A boolean vector, True denotes connected, False denotes not connected.

### Function 3: shortest_route_cost

Description: Function to calculate the cost of the shortest path between an origin point or group of origin points and a destination point or group of destination points. The cost can be either distance (kilometres) or time (minutes).

7 arguments: origins_sf (required), destinations_sf (required), road_graph (required), geometry_column (not required), join_by (not required), lookup_table (not required), cores_nr (not required).

- origins_sf: Origin points object of class 'sf'. The dataset should have a column named 'id', if the values of the 'id' column are not unique the minimum shortest path cost will be calculated per unique id value.
- destinations_sf: Destination points object of class 'sf'. The dataset should have a column named 'id', if the values of the 'id' column are not unique the minimum shortest path cost will be calculated per unique id value.
- road_graph: Graph object of class 'igraph' created using the road_to_graph function.
- geometry_column: Character (default value 'geometry'), column name that is present in both origins_sf and destinations_sf. The column should be of class 'sfc_Point', in a projected reference system. Obviously the same reference system should be used by the road network, origins_sf and destinations_sf.
- join_by: Character (defualt NULL), name of column that is present in both origins_sf and destinations_sf. If a join_by column is not provided all possible (cartesian product) shortest routes will be created between each unique id in origins_sf and each unique id in destinations_sf. The join_by argument can be useful if for example you want to calculate the shortest route cost between origins and destinations that are within the same administrative boundaries. The id values will represent those administrative boundaries. The advantages of using the join_by argument are shorter running times and smaller output object.
- lookup_table: object of class dataframe with two columns, the first column should have common values with the id field in the origins_sf dataset while the second column with the destinations_sf dataset. The rationale of providing a lookup table between the id values of origins_sf and destinations_sf is to calculate the shortest route cost between specific origins and destinations that meet some criteria. For example you could first identify the n-nearest neighbours between origins and destinations and calculate the shortest route cost among those.
- cores_nr: Integer (default 1). The number of processor cores to use. The mclapply function of the parallel library is used to process the data in parallel, therefore it can only be used in Unix systems. The more cores are used the more memory is required.

Returns: object of class 'data.table'. If 'area_id' is not provided there will be three columns 'origins_id', 'destinations_id', 'cost', otherwise one additional column for 'area_id'.

