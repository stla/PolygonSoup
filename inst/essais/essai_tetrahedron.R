library(PolygonSoup)
library(rgl)

# a tetrahedron with ill-oriented faces ####
vertices <- rbind(
  c(-1, -1, -1),
  c( 1,  1, -1),
  c( 1, -1,  1),
  c(-1,  1,  1)
)
faces <- rbind(
  c(1, 2, 3),
  c(3, 4, 2),
  c(4, 2, 1),
  c(4, 3, 1)
)

library(rgl)
tmesh <- tmesh3d(
  vertices    = t(vertices),
  indices     = t(faces),
  homogeneous = FALSE
)
open3d(windowRect = c(50, 50, 450, 450))
shade3d(tmesh, color = "blue", back = "cull")
shade3d(tmesh, color = "red", front = "cull")

movie3d(spin3d(axis = c(1, 1, 1), rpm = 10),
        duration = 6, fps = 20,
        movie = "zzpic", dir = ".",
        convert = FALSE, webshot = FALSE,
        startTime = 1/20)

library(gifski)
gifski(
  png_files = Sys.glob("zzpic*.png"),
  gif_file = "tetrahedron1.gif",
  width = 512,
  height = 512,
  delay = 1/11
)



# now run the `Mesh` function
mesh  <- Mesh(vertices, faces, normals = FALSE)
tmesh <- toRGL(mesh)
open3d(windowRect = c(50, 50, 450, 450))
shade3d(tmesh, color = "blue", back = "cull")

movie3d(spin3d(axis = c(1, 1, 1), rpm = 10),
        duration = 6, fps = 20,
        movie = "zzpic", dir = ".",
        convert = FALSE, webshot = FALSE,
        startTime = 1/20)

library(gifski)
gifski(
  png_files = Sys.glob("zzpic*.png"),
  gif_file = "tetrahedron2.gif",
  width = 512,
  height = 512,
  delay = 1/11
)
