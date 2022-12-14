# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

readFile <- function(filename) {
    .Call(`_PolygonSoup_readFile`, filename)
}

writeFile <- function(filename, binary, precision, Vertices, Faces) {
    invisible(.Call(`_PolygonSoup_writeFile`, filename, binary, precision, Vertices, Faces))
}

SurfEMesh <- function(rmesh, isTriangle, triangulate, clean, normals) {
    .Call(`_PolygonSoup_SurfEMesh`, rmesh, isTriangle, triangulate, clean, normals)
}

SurfQMesh <- function(rmesh, isTriangle, triangulate, clean, normals) {
    .Call(`_PolygonSoup_SurfQMesh`, rmesh, isTriangle, triangulate, clean, normals)
}

