source("neat.R")

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