source("neat_factory_interface.R")
source("directed_plannar.R")

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