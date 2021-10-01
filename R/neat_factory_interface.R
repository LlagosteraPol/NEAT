source("neat.R")

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