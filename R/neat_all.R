require(spatstat)
require(igraph)
require(intervals)
library(sf)
library(maptools)
library(aoos)
library(roxygen2)

setOldClass(c("igraph"))


#' @author Pol Llagostera Blasco
#' 
#-----------------------------OBJECT CLASSES------------------------------
#' Reference class that encapsulates different function to provide extra tools to 
#' work with igraph objects
#' 
#' @field .nodes_coords (Private) Contains a matrix with the coordinates 
#' of the nodes/vertices
#' @field .g (Private) igraph object
#' @field .distances_mtx (Private) Matrix containing all the distances 
#' between each pair of nodes
#' 
Neat <- setRefClass(
  Class = "Neat",
  contains = "Private",
  fields = list(.nodes_coords = "matrix", 
                .g = "igraph", 
                .distances_mtx = "matrix",
                .linnet_map = "linnet",
                .pixel_mask = "owin",
                .event_mtx = "matrix"),
  methods = list(
    
    #' Create an igraph object using the given map object. If the coordinates 
    #' of the vertices are given, then they're added to the graph as attributes.
    #' 
    #' @name Neat_createGraph
    #' @param map_obj  Can be a path to the file, SpatialLinesDataFrame, linnet, 
    #' adjacency matrix or directly an igraph object. 
    #' @param graph_type (character) Graph mode used by igraph.
    #' @param events_file_path File containing the events of the network
    #' @param long Longitudes of the nodes.
    #' @param lat Latitudes of the nodes.
    #' @return igraph object
    #' 
    # TODO: Check if necessary to give long lat and different map_obj
    createGraph = function(map_obj, events_file_path, graph_type, long = NULL, lat = NULL){
      
      cl_ob <- class(map_obj)
      
      extract_coords <- FALSE
      
      # Set coordinates, if not provided, try to extract from the 'map_obj' provided
      if(!is.null(long) & !is.null(lat)){
        .setCoordinatesMatrix(long, lat)
        .setDistancesMatrix()
      }
      else{
        extract_coords <- TRUE
      }
      
      if(class(map_obj)[1] == "SpatialLinesDataFrame"){
        .g <<- .createGraphFromShapelines(map_obj, graph_type, extract_coords)
      }
      else if (class(map_obj)[1] == "linnet"){
        .g <<- .createGraphFromLinnet(map_obj, graph_type, extract_coords)
      }
      else if (class(map_obj) == "character"){
        .g <<- .createGraphFromShp(map_obj, graph_type, extract_coords)
      }
      # TODO: Handle linnet
      else if (class(map_obj) == "matrix"){
        .g <<- .createGraphFromAdjacencyMatrix(map_obj, graph_type)
      }
      # TODO: Handle linnet
      else if (class(map_obj) == "igraph"){
        .g <<- map_obj
      }
      
      # Add coordinates to the graph as attributes 'long', 'lat'
      # TODO: Check that the condition works if the matrix is not initialized
      if(!is.null(.nodes_coords)){
        .setGraphCoords(.nodes_coords[,"long"], .nodes_coords[,"lat"])
      }
      
      .setEvents(events_file_path)
    },
    
    
    #' Private function. Create an igraph object using the given an adgacency Matrix.
    #' 
    #' @name Neat_.createGraphFromAdjacencyMatrix
    #' @param adjacency_mtx  Adjacency Matrix of the graph.
    #' @param graph_type (character) Graph mode used by igraph.
    #' @return igraph object
    #' 
    .createGraphFromAdjacencyMatrix = function(adjacency_mtx, graph_type){
      if(!is.null(.distances_mtx) & identical(dim(adjacency_mtx), dim(.distances_mtx))){
        wheighted_adj_mtx <- adjacency_mtx * .distances_mtx
        graph_from_adjacency_matrix(wheighted_adj_mtx, mode=graph_type, weighted = TRUE)
      }
      else{
        graph_from_adjacency_matrix(adjacency_mtx, mode=graph_type, weighted = NULL)
      }
    },
    
    
    #' Private function. Create an igraph object using the given a linnet object.
    #' 
    #' @name Neat_.createGraphFromLinnet
    #' @param linnet_obj  Linnet object.
    #' @param graph_type (character) Graph mode used by igraph.
    #' @return igraph object
    #' 
    .createGraphFromLinnet = function(linnet_obj, graph_type, extract_coords){
      # TODO: Make a try catch structure
      if(extract_coords){
        .setCoordinatesMatrix(linnet_obj[["vertices"]][["x"]], linnet_obj[["vertices"]][["y"]])
        .setDistancesMatrix()
      }
      .linnet_map <<- linnet_obj
      .createGraphFromAdjacencyMatrix(linnet_obj$m, graph_type)
    },
    
    
    #' Private function. Create an igraph object using the given ShapeLines (shp) object.
    #' 
    #' @name Neat_.createGraphFromShapelines
    #' @param spatial_lines_obj  ShapeLines object.
    #' @param graph_type (character) Graph mode used by igraph.
    #' @return igraph object
    #' 
    .createGraphFromShapelines = function(spatial_lines_obj, graph_type, extract_coords){
      .createGraphFromLinnet(as.linnet.SpatialLines(spatial_lines_obj, fuse=TRUE), graph_type, extract_coords)
    },
    
    
    #' Private function. Create an igraph object using the given the path to the .shp file.
    #' 
    #' @name Neat_.createGraphFromShp
    #' @param shp_file_route  Path to .shp file.
    #' @param graph_type (character) Graph mode used by igraph.
    #' @return igraph object
    #' 
    .createGraphFromShp = function(shp_file_route, graph_type, extract_coords){
      .createGraphFromShapelines(readShapeLines(shp_file_route), graph_type, extract_coords)
    },

    
    #' Private function. Set the coordinates into the graph nodes as attributes.
    #' 
    #' @name Neat_.setGraphCoords
    #' @param long Longitudes of the nodes.
    #' @param lat Latitudes of the nodes.
    #' 
    .setGraphCoords = function(long, lat){
      .g <<- .g %>% set_vertex_attr(name = "long", value = long) %>% 
                    set_vertex_attr(name = "lat", value = lat)
    },
    
    #' Private function. Set the coordinates into the graph nodes as attributes.
    #' 
    #' @name Neat_.setCoordinatesMatrix
    #' @param long Longitudes of the nodes.
    #' @param lat Latitudes of the nodes.
    #' 
    .setCoordinatesMatrix = function(long, lat){
      .nodes_coords <<- cbind(long, lat)
      colnames(.nodes_coords) <<- c("long", "lat")
    },
    
    
    #' Private function. Set the distances matrix of all pair of nodes.
    #' Uses WGS84 coordinate system.
    #' 
    #' @name Neat_.setDistancesMatrix
    #' 
    .setDistancesMatrix = function(){
      .distances_mtx <<-  as.matrix(spDists(.nodes_coords, .nodes_coords, longlat=TRUE))
    },
    
    
    #' Get the linear distance between two nodes (without going through the graph).
    #' Only if the graph is georeferenced.
    #' 
    #' @name Neat_getLinearDistance
    #' @param v1 Source node.
    #' @param v2 Ending node.
    #' @return distance (int) or -1 if the graph doesn't have coordinates.
    #' 
    getLinearDistance = function(v1, v2){
      if(!is.null(.distances_mtx)){
        .distances_mtx[v1, v2]
      } else{
        -1
      }
    },
    
    
    #' Given a point, projects this point to the graph and gives the neighbor crossings
    #' to it.
    #'
    #' @name Neat_pointNeighborCrossings
    #'
    #' @param long Longitude coordinates of the point
    #' @param lat Latitude coordinates of the point
    #' @return a list containing lists with information of the path and weight to each 
    #' neighbor crossing.
    #'
    pointNeighborCrossings = function(long, lat){
      nearest_info <- pointNearestVertice(long, lat)
      projection_info <- .projectToLine(nearest_info, cbind(long = long, lat  = lat))
      
      # If it's projected to a node then return the distance from that node
      if(is.null(projection_info$to)){
        return(list(closestCrossing(projection_info$from)))
      }
      
      crossings <- neighborCrossings(nearest_info$node_id)
      
      crossings_with_point <- list()
      for(crossing in crossings){
        cross_path <- crossing$path
        cross_weight <- crossing$weight
        
        x1_long <- projection_info$projection_coords[1]
        x1_lat <- projection_info$projection_coords[2]
        x2_long <- nearest_info$node_coords[1]
        x2_lat <- nearest_info$node_coords[2]
        
        distance <- spDistsN1(matrix(c(x1_long, x1_lat), ncol = 2), matrix(c(x2_long, x2_lat), ncol=2), longlat=TRUE)
        
        cross_weight <- if(projection_info$to %in% cross_path) cross_weight - distance else cross_weight + distance
        
        crossings_with_point <- append(crossings_with_point, list(list(path = cross_path, weight = cross_weight)))
      }
      return(crossings_with_point)
    },
    
    
    #' Given a point, projects this point to the graph and gives the clossest crossing
    #' to it.
    #' 
    #' @name Neat_pointClosestCrossing
    #' 
    #' @param long Longitude coordinates of the point
    #' @param lat Latitude coordinates of the point
    #' @return list(path, weight) path = path to the closest crossing, weight = km to the
    #' closest crossing
    #' 
    pointClosestCrossing = function(long, lat){
      crossings <- pointNeighborCrossings(long, lat)
      
      lowest_weight <- NULL
      shortest_path <- NULL
      for(crossing in crossings){
        path <- crossing$path
        weight <- crossing$weight
        
        if(weight < lowest_weight || is.null(lowest_weight)){
          lowest_weight <- weight
          shortest_path <- path
        }
      }
      
      return(list(path = shortest_path, weight = lowest_weight))
    },

    
    #' Given a point coordinates, gives the nearest vertice of the igraph object contained 
    #' in the class.
    #'
    #' @name Neat_.pointNearestVertice
    #' 
    #' @param long Longitude coordinates of the point
    #' @param lat Latitude coordinates of the point
    #' @return list(node_id, node_coords, distance) node_id = id of the closest node to 
    #' the given point, node_coords = coordinates of the closest node, distance = the 
    #' distance from the given point to its closest node
    #' 
    pointNearestVertice = function(long, lat){
      tmp <- .nodes_coords
      point <- rbind(c(long, lat))
      distances <- as.matrix(spDistsN1(tmp, point, longlat=TRUE))
      min_idx <- which.min(distances)
      node_coords <- cbind(long = vertex_attr(.g, "long", min_idx),
                           lat  = vertex_attr(.g, "lat", min_idx))
      
      return(list(node_id = min_idx, node_coords = node_coords, distance = distances[min_idx]))
    },
    
    
    #' Private function. Project the center of a point to the closest line of the igraph object.
    #' If there is a node more close than a line, then project to the node.
    #'
    #' @name Neat_.projectToLine
    #' 
    #' @param near_node_info pointNearestVertice return list (It's a list containing information 
    #' about the node of the graph most close to the point)
    #' @param point_coords = c(longitude, latitude) Coordinates of the point to be projected
    #' @return list(from, to, projection_coords) from = id of the closest node of the igraph object 
    #' to the point, to = id of the second closest node neighbor of 'from', projection_coords = 
    #' an array containing the longitude and latitude of the projection
    #' 
    .projectToLine = function(near_node_info, point_coords){
      nei <- neighbors(.g, near_node_info$node_id)
      
      neighbor_coords <- NULL
      for(node in nei){
        neighbor_coords <- rbind(neighbor_coords,
                                 c(vertex_attr(.g, "long", node), 
                                   vertex_attr(.g, "lat", node)))
      }
      rownames(neighbor_coords) <- nei
      colnames(neighbor_coords) <- c("long", "lat")
      
      dist_near_neis <- as.matrix(spDistsN1(neighbor_coords, 
                                            near_node_info$node_coords, 
                                            longlat=TRUE))
      rownames(dist_near_neis) <- nei
      colnames(dist_near_neis) <- near_node_info$node_id
      
      dist_vir_neis <- as.matrix(spDistsN1(neighbor_coords, 
                                           point_coords, 
                                           longlat=TRUE))
      rownames(dist_vir_neis) <- nei
      colnames(dist_vir_neis) <- "virtual"
      
      
      # Calculate projection to the line using Heron's formula
      triangles_info <- NULL
      min_h <- NULL
      min_id <- NULL
      for(i in 1:nrow(neighbor_coords)){
        nei_id <- rownames(neighbor_coords)[i]
        a <- dist_vir_neis[nei_id,]
        b <- near_node_info$distance
        c <- dist_near_neis[nei_id,]
        
        # If the angle between the point and the ends of the segment 
        # is not obtuse, then pass
        if(c < a || c < b){
          next
        }
          
        s <- (a+b+c)/2
        h <- (2/c)*sqrt(s*(s-a)*(s-b)*(s-c))
        
        triangles_info[[nei_id]] <- list(a=a, b=b, c=c, h=h)
        if(is.null(min_h) || h < min_h){
          min_h <- h
          min_id <- nei_id
        }
      }
      
      # If all angles were obtuse, then project to the closest node
      if(is.null(triangles_info)){
        return(list(from = near_node_info$node_id, 
                    to = NULL, 
                    projection_coords = near_node_info$node_coords))
      }
      
      # Else, project to the line (segment)
      min_segment <- triangles_info[[min_id]]
      nei_id <- as.numeric(names(triangles_info[min_id]))
      
      nei_coords <- c(long = vertex_attr(.g, "long", nei_id),
                      lat  = vertex_attr(.g, "lat", nei_id))
      
      projection_coords <- projectedPointCoordinates(unname(nei_coords), 
                                                     as.array(near_node_info$node_coords), 
                                                     as.array(point_coords))
      return(list(from = near_node_info$node_id, 
                  to = nei_id, 
                  projection_coords = projection_coords))
    },
    
    
    #' Project coordinates to the line of the segment formed by va vb endpoints
    #' 
    #' @name Neat_projectedPointCoordinates
    #' 
    #' @param va Segment endpoint va = c(longitude, latitude)
    #' @param vb Segment endpoint vb = c(longitude, latitude)
    #' @param point Coordinates of the point to be projected point = c(longitude, latitude)
    #' @return an array containing the longitude and latitude of the projection
    #' 
    projectedPointCoordinates = function(va, vb, point){
      va_long <- va[1]
      va_lat <- va[2]
      vb_long <- vb[1]
      vb_lat <- vb[2]
      point_long <- point[1]
      point_lat <- point[2]
      
      p_long <-  vb_long - va_long
      p_lat <-  vb_lat - va_lat
      dab <- p_long * p_long + p_lat * p_lat
      u <- ((point_long - va_long) * p_long + (point_lat - va_lat) * p_lat) / dab
      long <- va_long + u * p_long 
      lat <- va_lat + u * p_lat
      
      return (c(long = long, lat = lat))
    },
    
    
    #' Creates a pixel mask of the map previously loaded into the class.
    #'
    #' @name Neat_getPixelMask
    #' 
    #' @param size_px The size of the pixel
    #' @return a list containing an image (class "im") and a matrix representing the pixel mask
    #' 
    getPixelMask = function(size_px){
      
      # Check if the pixel mask requested was previously created, if not create a new one.
      if(is.null(.pixel_mask) || round(.pixel_mask$xstep,digits = 4) != size_px ){
        rangex<-.linnet_map$window$xrange[2]-.linnet_map$window$xrange[1]
        rangey<-.linnet_map$window$yrange[2]-.linnet_map$window$yrange[1]
        
        gridx<-as.integer(rangex/size_px)
        gridy<-as.integer(rangey/size_px)
        
        dens_graf<-matrix(rep(1,gridx*gridy),nrow=gridy,ncol=gridx,byrow=T)
        dens_graf<-as.im(dens_graf) 
        
        num_px<-max(gridx,gridy)
        maxdim<-max(dens_graf$dim[1],dens_graf$dim[2])
        dimpx<-num_px*dens_graf$dim[2]/maxdim
        dimpy<-num_px*dens_graf$dim[1]/maxdim
        
        l <- as.linnet(.linnet_map)
        l_lines <- as.psp(l)
        .pixel_mask <<- as.mask.psp(l_lines, dimyx = c(dimpy, dimpx))
      }
      
      lineimage <- as.im(.pixel_mask, value = 0)
      x_mtx <- rbind(.pixel_mask$xcol)
      y_mtx <- cbind(.pixel_mask$yrow)
      mask_mtx <- .pixel_mask$m
      colnames(mask_mtx) <- round(.pixel_mask$xcol, 4)
      rownames(mask_mtx) <- round(.pixel_mask$yrow, 4)

      #TODO: Delete tests
      # norm_coords = matrix(cbind(vertex_attr(.g)$long, vertex_attr(.g)$lat), ncol=2) # test
      # #norm_coords = matrix(cbind(vertex_attr(.g)$long, vertex_attr(.g)$lat), ncol=2) # test
      # win_linnet <- Window(.linnet_map) # test
      # plot(.linnet_map, window=TRUE, rescale=FALSE, axes=TRUE, layout = norm_coords) # test
      # image(lineimage) # test
      # georeferencedPlot() # test
      # tt <- .linnet_map# test
      #plot(.g, vertex.label=NA, vertex.size=2,vertex.size2=2) # test
      
      # # Invert rows and columns
      # mask_mtx <- mask_mtx[nrow(mask_mtx):1,]
      # mask_mtx <- mask_mtx[,order(ncol(mask_mtx):1)]
      
      return(list(mask = .pixel_mask, img = lineimage, mtx = mask_mtx))
    },
    
    
    #' Plot the graph using the coordinates if provided. If not, perform a normal plot.
    #' 
    #' @name Neat_georeferencedPlot
    #'
    georeferencedPlot = function(){
      if(!is.null(.distances_mtx)){
        # TODO: Remove test
        norm_coords = layout.norm(matrix(cbind(vertex_attr(.g)$long, vertex_attr(.g)$lat), ncol=2))
        #norm_coords = matrix(cbind(vertex_attr(.g)$long, vertex_attr(.g)$lat), ncol=2) # test
        x_min  = min(norm_coords[,1]) # test
        x_max  = max(norm_coords[,1]) # test
        y_min  = min(norm_coords[,2]) # test
        y_max  = max(norm_coords[,2]) # test
                     
        plot(.g, layout = norm_coords, vertex.label=NA, vertex.size=2, window=TRUE, axes=TRUE)
      }
      else{
        plot(.g, vertex.label=NA, vertex.size=2,vertex.size2=2)
      }
    },
    
    
    #' Get the shortest distance between two nodes (going through the graph).
    #' 
    #' @name Neat_shortestDistance
    #' @param v1 Source node.
    #' @param v2 Ending node.
    #' @return list containing the path and its total weight.
    #' 
    shortestDistance = function(v1, v2){
      weighted_path <- unlist(get.shortest.paths(.g, v1, v2)$vpath)
      if(!is.null(.distances_mtx)){
        weight_sum <- sum(E(.g, path = unlist(weighted_path))$weight)
      }
      else{
        weight_sum <- length(weighted_path)
      }
      list(path = weighted_path, weight = weight_sum)  
    },
    
    
    #' Get the shortest distance between a node and a neighbor crossing.
    #' 
    #' @name Neat_closestCrossing
    #' @param v1 Source node.
    #' @return lowest - list containing the path and its total weight.
    #' 
    closestCrossing = function(v1){
      #TODO: Check if a crossing has to count itself as the closest crossing
      # if(isCrossing(v1)){
      #   lowest <- list(path = c(v1), weight = 0)
      # }
      #else{
      weighted_paths <- neighborCrossings(v1)
      lowest <- list(path = c(), weight = -1)
      
      for (weighted_path in weighted_paths){
        if (weighted_path$weight < lowest$weight | lowest$weight == -1){
          lowest <- weighted_path
        }
      }
      #}
     
      lowest
    },
    
    
    #' Gives the paths and distances of the neighbor crossings of a node.
    #' 
    #' @name Neat_neighborCrossings
    #' @param v1 Source node.
    #' @return weighted_paths - list of lists containing the paths and its total weights.
    #' 
    neighborCrossings = function(v1){
      
      discovered <- c(v1)
      crossings <- list()
      weighted_paths <- list()
      
      # Get paths to crossings 
      crossings <- .recursiveNeighborCrossings(v1)
      for(crossing in crossings){
        weight_sum <- sum(E(.g, path = unlist(crossing$path))$weight)
        weighted_paths <- append(weighted_paths, list(list(path = crossing$path, weight = weight_sum)))
      }
      weighted_paths
    },
    
    
    #' (Private) Recursive function used by neighborCrossings
    #' 
    #' @name Neat_.recursiveNeighborCrossings
    #' @param v1 Source node.
    #' @param discovered Array of the visited nodes
    #' @return path or NULL if didn't end in a crossing
    #' 
    .recursiveNeighborCrossings = function(source, crossings = list(), path = c()){
      path <- c(path, source)
      
      if(isCrossing(source)){
        crossings <- c(crossings, list(list(crossing = source, path = path)))
      }

      else{
        for (element in neighbors(.g, source)){
          #Avoid loops
          if (!element %in% path){
            crossings <- .recursiveNeighborCrossings(element, crossings, path)
          }
        }
      }
      
      return(crossings)
    },
    
    
    #' Gives the paths and distances of the neighbor crossings of a node.
    #' 
    #' @name Neat_neighborCrossings_old
    #' @param v1 Source node.
    #' @return weighted_paths - list of lists containing the paths and its total weights.
    #' 
    neighborCrossings_old = function(v1){
      
      discovered <- c(v1)
      crossings <- list()
      weighted_paths <- list()
      
      #Get paths to crossings
      for(src_nei in neighbors(.g, v1)){
        weighted_path <- .recursiveNeighborCrossings(src_nei, discovered)
        if(!is.null(weighted_path)){
          crossings <- append(crossings, list(weighted_path))
        }
      }

      for(c_path in crossings){
        weight_sum <- sum(E(.g, path = unlist(c_path))$weight)
        weighted_paths <- append(weighted_paths, list(list(path = c_path, weight = weight_sum)))
      }
      weighted_paths
    },
    
    
    #' (Private) Recursive function used by neighborCrossings
    #' 
    #' @name Neat_.recursiveNeighborCrossings_old
    #' @param v1 Source node.
    #' @param discovered Array of the visited nodes
    #' @return path or NULL if didn't end in a crossing
    #' 
    .recursiveNeighborCrossings_old = function(source, discovered){
      discovered <- c(discovered, source)
      
      if (length(discovered) > 1){
        if(length(neighbors(.g, source)) > 2 || length(neighbors(.g, source)) == 1){
          return(discovered)
        }
        #TODO: Revise
        # If the path doesn't end in a crossing, return NULL
        else if(length(neighbors(.g, source)) == 1){
          return(NULL)
        }
      }
      
      for (element in neighbors(.g, source)){
        if (!element %in% discovered){
          discovered <- .recursiveNeighborCrossings(element, discovered)
          if(is.null(discovered)){
            break
          }
        }
      }
      
      discovered
    },
    
    #' Function that says if the given vertice is considered a crossing
    #' 
    #' @name Neat_isCrossing
    #' @return boolean - TRUE if crossing, FALSE if not
    #' 
    isCrossing = function(v){
      if(degree(.g, v) > 2 || degree(.g, v) == 1) TRUE
      else FALSE
    },
    
    
    #' Function that returns the crossings of the network
    #' 
    #' @name Neat_getCrossings
    #' @return crossings - array with an array of nodes with degree > 2
    #' 
    getCrossings = function(){
      crossings <- c()
      for(node in V(.g)){
        if(isCrossing(node))
        {
          crossings <- c(crossings, node)
        }
      }
      crossings
    },
    
    
    #' Given a mask of pixels, calculate the distance to the closest crossing. 
    #' Only calculates the distances of the pixels that has 'TRUE' inside the mask.
    #' 
    #' @name Neat_pixelMaskClosestCrossins
    #' @param size_px The size of the pixel
    #' 
    #' @return mask_distances - matrix of pixels with the distance to the closest crossing per pixel.
    #' 
    pixelMaskClosestCrossins = function(size_px){
      
      mask_info <- getPixelMask(size_px)

      mask_distances <- mask_info$mtx
      mask_distances[mask_distances == FALSE] <- NA
      mask_distances[mask_distances == TRUE] <- -1
      for(row in 1:nrow(mask_distances)) {
        for(col in 1:ncol(mask_distances)) {
          if(!is.na(mask_distances[row, col])){
            # Get the pixel coordinates from the mask
            long <- as.numeric(colnames(mask_distances)[col])
            lat <- as.numeric(rownames(mask_distances)[row])
            mask_distances[row, col] <- pointClosestCrossing(long, lat)$weight
          }
        }
      }
      aux <- mask_info$img
      
      aux$v <- mask_distances
      
      aux
    },
    
    
    #' Insert the events coordinates into the class in a matrix form
    #' 
    #' @name Neat_setEvents
    #' @param events_file_path Path of the .shp file
    #' 
    .setEvents = function(events_file_path){
      .event_mtx <<- do.call(rbind, st_geometry(st_read(events_file_path)))
      colnames(.event_mtx) <<- c("long", "lat")
    },
    
    #' Get the number of events in an area surrounding a vertice
    #' 
    #' @name Neat_eventsInArea
    #' @param node Node ID
    #' @param area_km Area to check in km
    #' 
    #' @return events - matrix of events in the area with the following column info.: distance, event longitude, event latitude
    #' 
    eventsInArea = function(node, area_km){
      #events <- list()
      events <- matrix(nrow = 0, ncol = 3)
      
      node_coords <- cbind(long = vertex_attr(.g, "long", node),
                           lat  = vertex_attr(.g, "lat", node))
      
      for(row in 1:nrow(.event_mtx)) {
        ev_long <- .event_mtx[row, "long"]
        ev_lat <- .event_mtx[row, "lat"]
        event_coords <- cbind(long = .event_mtx[row, "long"],
                              lat  = .event_mtx[row, "lat"])
        distance <- spDistsN1(event_coords, node_coords,longlat=TRUE)
        
        
        if (distance <= area_km){
          #events <- append(events, list(list(distance = distance, coords = event_coords))) # list format
          events <- rbind(events, c(distance, event_coords))
        }
      }
      colnames(events) <- c("distance","long", "lat")
      return(events)
    },
    
    #' Getter of the graph
    #'
    #' @name Neat_getGraph
    #' @return .g - the graph
    #' 
    getGraph = function(){
      .g
    }
  )
)

# Subclasses of Neat

#' Subclass of Neat specific to work with Undirected General graphs
#' 
UndirectedGeneral <- setRefClass(
  Class = "UndirectedGeneral",
  contains = "Neat",
  fields = list(),
  methods = list(
    
    #' Test function to know which class is refering
    #'
    #' @name UndirectedGeneral_what
    #'
    what = function(){
      print("Undirected General Graph Subclass")
    }
  )
)

#' Subclass of Neat specific to work with Undirected Plannar graphs
#' 
UndirectedPlannar <- setRefClass(
  Class = "UndirectedPlannar",
  contains = "Neat",
  fields = list(),
  methods = list(
    
    #' Test function to know which class is refering
    #'
    #' @name UndirectedPlannar_what
    #'
    what = function(){
      print("Undirected Plannar Graph Subclass")
    }
  )
)

#' Subclass of Neat specific to work with Directed Plannar graphs
#' 
DirectedPlannar <- setRefClass(
  Class = "DirectedPlannar",
  contains = "Neat",
  fields = list(),
  methods = list(
    
    #' Test function to know which class is refering
    #'
    #' @name DirectedPlannar_what
    #'
    what = function(){
      print("Directed Plannar Graph Subclass")
    }
  )
)
#--------------------------------------------------------------------------

#-----------------------------FACTORY CLASSES------------------------------

#' Reference class providing a Factory interface for Neat class
#' 
NeatFactoryInterface <- setRefClass(
  Class = "NeatFactoryInterface",
  fields = list(),
  methods = list(
    
    #' Abstract function to implement by the class subclasses
    #'
    #' @name NeatFactoryInterface_initGraph
    #' @param map_obj  Can be a path to the file, SpatialLinesDataFrame, linnet, 
    #' adjacency matrix or directly an igraph object. 
    #' @param graph_characteristic "General", "Plannar"...
    #' @param long Longitudes of the nodes.
    #' @param lat Latitudes of the nodes.
    #'
    initGraph = function(map_obj, events_file_path, graph_characteristic, long = NULL, lat = NULL){
      # implemented by subclasses
    },
    
    #' Constructs the correct Neat object
    #'
    #' @name NeatFactoryInterface_getExtendedGraph
    #' @param map_obj  Can be a path to the file, SpatialLinesDataFrame, linnet, 
    #' adjacency matrix or directly an igraph object.  
    #' @param graph_characteristic "General", "Plannar"...
    #' @param long Longitudes of the nodes.
    #' @param lat Latitudes of the nodes.
    #' @return xtnd_igraph - Neat object
    #'
    getExtendedGraph = function(map_obj, events_file_path, graph_characteristic, long = NULL, lat = NULL){
      xtnd_igraph <- initGraph(map_obj, events_file_path, graph_characteristic, long, lat) # Get specialized subfactory object
      xtnd_igraph
    }
  )
)

#' Factory implementation of NeatFactoryInterface for Undirected graphs.
#' 
UndirectedFactory <- setRefClass(
  Class = "UndirectedFactory",
  contains = "NeatFactoryInterface",
  fields = list(),
  methods = list(
    
    #' Initializes an Neat object based on Undirected graphs and 
    #'
    #' @name UndirectedFactory_initGraph
    #' @param map_obj  Can be a path to the file, SpatialLinesDataFrame, linnet, 
    #' adjacency matrix or directly an igraph object. 
    #' @param graph_characteristic "General", "Plannar"...
    #' @param long Longitudes of the nodes.
    #' @param lat Latitudes of the nodes. 
    #' @return proper Undirected object
    #'
    initGraph = function(map_obj, events_file_path, graph_characteristic, long = NULL, lat = NULL){
      
      if(tolower(graph_characteristic) == "general"){
        undirGnr <- UndirectedGeneral()
        undirGnr$createGraph(map_obj, events_file_path, "undirected", long, lat)
        
        # Return UndirectedGeneral object
        undirGnr
      }
      else if(tolower(graph_characteristic) == "plannar"){
        undirPln <- UndirectedPlannar()
        undirPln$createGraph(map_obj, events_file_path, "undirected", long, lat)
        
        # Return UndirectedPlannar object
        undirPln 
      }
    }
  )
)

#' Factory implementation of NeatFactoryInterface for Directed graphs.
#' 
DirectedFactory <- setRefClass(
  Class = "DirectedFactory",
  contains = "NeatFactoryInterface",
  fields = list(),
  methods = list(
    
    #' Initializes the correct Neat object
    #'
    #' @name DirectedFactory_initGraph
    #' @param map_obj  Can be a path to the file, SpatialLinesDataFrame, linnet, 
    #' adjacency matrix or directly an igraph object. 
    #' @param graph_characteristic "General", "Plannar"...
    #' @param long Longitudes of the nodes.
    #' @param lat Latitudes of the nodes. 
    #' @return proper Directed object
    #'
    initGraph = function(map_obj, events_file_path, graph_characteristic, long = NULL, lat = NULL){
      if(tolower(graph_characteristic) == "plannar"){
        print("Directed Plannar Graph Factory")
        dirPln <- DirectedPlannar()
        dirPln$create_graph(map_obj, events_file_path, "undirected", long, lat)
        
        # Return DirectedPlannar object
        dirPln 
      }
    }
  )
)
#--------------------------------------------------------------------------

# Debugging code

# area_shp_WGS84 <- "Data/AreaCat/Cat_Roads_Cut_Simplified_WGS84.shp"
# area_accidents_WGS84 <- "Data/AreaCat/Accidents_Area70_70_WGS84.shp"
# lleida_shp_WGS84 <- "Data/Lleida/Lleida_connected_WGS84.shp"
# lleida_accidents_WGS84 <- "Data/Lleida/Cat_Accidents_Cut_Lleida.shp"
# 
# undirected <- UndirectedFactory()
# undirected_plannar <- undirected$getExtendedGraph(map_obj = lleida_shp_WGS84, 
#                                                   events_file_path = lleida_accidents_WGS84,
#                                                   graph_characteristic = "plannar")
# 
# pixel_masks <- undirected_plannar$getPixelMask(0.005)