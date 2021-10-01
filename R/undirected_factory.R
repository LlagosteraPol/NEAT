source("neat_factory_interface.R")
source("undirected_general.R")
source("undirected_plannar.R")

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