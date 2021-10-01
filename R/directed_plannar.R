source("neat.R")

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