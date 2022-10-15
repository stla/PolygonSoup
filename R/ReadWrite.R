#' @title Read a mesh file
#' @description Read mesh vertices and faces from a file.
#'
#' @param filepath path to the mesh file; supported formats are \code{stl},
#'   \code{ply}, \code{obj} and \code{off}
#'
#' @return A list with two fields: \code{vertices}, a numeric matrix with three
#'   columns, and \code{faces}, either a list of integer vectors or, in the
#'   case if all faces have the same number of sides, an integer matrix.
#' @export
#'
#' @importFrom data.table uniqueN
#'
#' @examples
#' library(PolygonSoup)
#' library(rgl)
#' vf <- readMeshFile(
#'   system.file("extdata", "beethoven.ply", package = "PolygonSoup")
#' )
#' mesh <- Mesh(
#'   vf[["vertices"]], vf[["faces"]], normals = TRUE
#' )
#' rglmesh <- toRGL(mesh)
#' open3d(windowRect = c(50, 50, 562, 562))
#' view3d(0, 0, zoom = 0.8)
#' shade3d(rglmesh, color = "palevioletred")
readMeshFile <- function(filepath){
  if(!file.exists(filepath)){
    stop("File not found.")
  }
  mesh <- readFile(filepath)
  faces <- mesh[["faces"]]
  usizes <- uniqueN(lengths(faces))
  if(usizes == 1L){
    mesh[["faces"]] <- do.call(rbind, faces)
  }
  mesh
}

#' @title Export mesh to a file
#' @description Export a mesh to a file.
#'
#' @param mesh a mesh given either as a list containing (at least) the fields
#'   \code{vertices} and \code{faces}, otherwise a \strong{rgl} mesh
#'   (i.e. a \code{mesh3d} object)
#' @param filename name of the file to be written, with extension \code{stl},
#'   \code{ply}, \code{obj} or \code{off}
#' @param precision positive integer, number of decimal digits for the vertices
#' @param binary Boolean, whether to write a binary file or an ASCII file
#'
#' @return No value, just generates the file.
#' @export
writeMeshFile <- function(mesh, filename, precision = 17L, binary = FALSE){
  stopifnot(isString(filename))
  stopifnot(isPositiveInteger(precision))
  stopifnot(isBoolean(binary))
  if(inherits(mesh, "mesh3d")){
    vft  <- getVFT(mesh, beforeCheck = TRUE)
    mesh <- vft[["rmesh"]]
  }
  vertices <- mesh[["vertices"]]
  faces    <- mesh[["faces"]]
  checkedMesh <- checkMesh(vertices, faces, gmp = FALSE, aslist = TRUE)
  vertices <- checkedMesh[["vertices"]]
  faces    <- checkedMesh[["faces"]]
  writeFile(
    filename,
    binary,
    as.integer(precision),
    vertices,
    faces
  )
  invisible(NULL)
}
