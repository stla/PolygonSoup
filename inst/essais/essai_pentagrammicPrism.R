library(PolygonSoup)
library(rgl)
mesh <- Mesh(
  mesh = pentagrammicPrism,
  triangulate = TRUE, normals = FALSE
)

tmesh <- toRGL(mesh)
open3d(windowRect = c(50, 50, 450, 450), zoom = 0.9)
shade3d(tmesh, color = "maroon")
plotEdges(
  mesh[["vertices"]], mesh$exteriorEdges, color = "darkred", 
  tubesRadius = 0.02, spheresRadius = 0.02
)


allEdges <- mesh[["edgesDF"]]
edges <- as.matrix(subset(allEdges, angle <= 91, select = c("i1", "i2")))


tmesh <- toRGL(mesh)
open3d(windowRect = c(50, 50, 450, 450), zoom = 0.9)
shade3d(tmesh, color = "maroon")
plotEdges(
  mesh[["vertices"]], edges, color = "darkred", 
  tubesRadius = 0.02, spheresRadius = 0.02
)

snapshot3d("pentagrammicPrism3.png", webshot = FALSE)
