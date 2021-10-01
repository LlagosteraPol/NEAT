source("neat.R")

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