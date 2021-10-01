
# Set working directory
setwd("R")


source("undirected_factory.R")

area_shp_WGS84 <- "../Data/AreaCat/Cat_Roads_Cut_Simplified_WGS84.shp"
area_accidents_WGS84 <- "../Data/AreaCat/Accidents_Area70_70_WGS84.shp"
lleida_shp_WGS84 <- "../Data/Lleida/Lleida_connected_WGS84.shp"
lleida_accidents_WGS84 <- "../Data/Lleida/Cat_Accidents_Cut_Lleida.shp"


undirected <- UndirectedFactory()
undirected_general <- undirected$getExtendedGraph(map_obj = area_shp_WGS84, 
                                                  events_file_path = area_accidents_WGS84,
                                                  graph_characteristic = "general")


# Get neighbor crossings path+weight from a node/vertice
undirected_general$neighborCrossings(50)

# Plot map
undirected_general$georeferencedPlot()

# Create mask
pixel_masks <- undirected_general$getPixelMask(0.005)
mask <- pixel_masks$mask
img <- pixel_masks$img
mask_mtx <- pixel_masks$mtx
mask_mtx
image(img)

# Get the pixel coordinates to work from the mask
long <- as.numeric(colnames(mask_mtx)[14])
lat <- as.numeric(rownames(mask_mtx)[70])

# Get the neighbor crossings of the given point (longitude, latitude)
pixel_crossings <- undirected_general$pointNeighborCrossings(long, lat)
pixel_crossings

# Get the closest crossings of the given point
pixel_shortest <- undirected_general$pointClosestCrossing(long, lat)
pixel_shortest

# On this example, the path starts at node 418. We can see that the
# distance of the projected pixel point to the segment it's also 
# compured by comparing if we get the path directly from the node 418

# Get only the shortest path to crossing
vertice_shortest <- undirected_general$closestCrossing(418)
vertice_shortest

# Get all the crossings of the network
crossings <- undirected_general$getCrossings()
crossings

# Get the pixelated mask of the network with the distances to the closest crossing per pixel
# Now we give the size of the pixel, so no need to call the function 'getPixelMask'
mask_distances <- undirected_general$pixelMaskClosestCrossins(0.005)
image(mask_distances)

# Get the events that fall inside an area of 20 km from node '1'
events_area <- undirected_general$eventsInArea(1, 20) # will return a matrix with the distances and the long, and lat of the event

