#' @exportS3Method print cgalMesh
print.cgalMesh <- function(x, ...){
	rgl <- attr(x, "toRGL")
	nv <- nrow(x[["vertices"]])
	nf <- if(is.list(x[["faces"]])) length(x[["faces"]]) else nrow(x[["faces"]])
	msg <- sprintf("Mesh with %d vertices and %d faces.\n", nv, nf)
	cat(msg)
	elr <- formatC(range(x[["edgesDF"]][["length"]]))
	msg <- sprintf("The edge lengths vary from %s to %s.\n", elr[1L], elr[2L])
	cat(msg)
	is <- if(rgl == 3L) " is " else " is not "
	msg <- paste0("This mesh", is, "triangle.\n")
	cat(msg)
	can <- if(isFALSE(rgl)) " cannot " else " can "
	msg <- paste0(
			"This mesh", can, "be converted to a 'rgl' mesh (see `?toRGL`).\n"
	)
	cat(msg)
	normals <- !is.null(x[["normals"]])
	has <- if(normals) " has " else " does not have "
	msg <- paste0("This mesh", has, "vertex normals.\n")
	cat(msg)
	invisible(NULL)
}

#' @importFrom gmp is.bigq is.matrixZQ
#' @importFrom data.table uniqueN
#' @noRd
checkMesh <- function(vertices, faces, gmp, aslist){
	if(gmp){
		if(!is.matrixZQ(vertices) || ncol(vertices) != 3L){
			stop("The `vertices` argument must be a matrix with three columns.")
		}
		stopifnot(is.bigq(vertices))
		vertices <- as.character(vertices)
	}else{
		if(!is.matrix(vertices) || ncol(vertices) != 3L){
			stop("The `vertices` argument must be a matrix with three columns.")
		}
		stopifnot(is.numeric(vertices))
		storage.mode(vertices) <- "double"
	}
	if(anyNA(vertices)){
		stop("Found missing values in `vertices`.")
	}
	homogeneousFaces <- FALSE
	isTriangle       <- FALSE
	toRGL            <- FALSE
	if(is.matrix(faces)){
		if(ncol(faces) < 3L){
			stop("Faces must be given by at least three indices.")
		}
		storage.mode(faces) <- "integer"
		if(anyNA(faces)){
			stop("Found missing values in `faces`.")
		}
		if(any(faces < 1L)){
			stop("Faces cannot contain indices lower than 1.")
		}
		if(any(faces > nrow(vertices))){
			stop("Faces cannot contain indices higher than the number of vertices.")
		}
		homogeneousFaces <- ncol(faces)
		if(homogeneousFaces %in% c(3L, 4L)){
			isTriangle <- homogeneousFaces == 3L
			toRGL <- homogeneousFaces
		}
		if(aslist){
			faces <- lapply(1L:nrow(faces), function(i) faces[i, ] - 1L)
		}else{
			faces <- t(faces - 1L)
		}
	}else if(is.list(faces)){
		check <- all(vapply(faces, isAtomicVector, logical(1L)))
		if(!check){
			stop("The `faces` argument must be a list of integer vectors.")
		}
		check <- any(vapply(faces, anyNA, logical(1L)))
		if(check){
			stop("Found missing values in `faces`.")
		}
		faces <- lapply(faces, function(x) as.integer(x) - 1L)
		sizes <- lengths(faces)
		if(any(sizes < 3L)){
			stop("Faces must be given by at least three indices.")
		}
		check <- any(vapply(faces, function(f){
							any(f < 0L) || any(f >= nrow(vertices))
						}, logical(1L)))
		if(check){
			stop(
					"Faces cannot contain indices lower than 1 or higher than the ",
					"number of vertices."
			)
		}
		usizes <- uniqueN(sizes)
		if(usizes == 1L){
			homogeneousFaces <- sizes[1L]
			isTriangle <- homogeneousFaces == 3L
			if(homogeneousFaces %in% c(3L, 4L)){
				toRGL <- homogeneousFaces
			}
		}else if(usizes == 2L && all(sizes %in% c(3L, 4L))){
			toRGL <- 34L
		}
	}else{
		stop("The `faces` argument must be a list or a matrix.")
	}
	list(
			vertices = t(vertices),
			faces = faces,
			homogeneousFaces = homogeneousFaces,
			isTriangle = isTriangle,
			toRGL = toRGL
	)
}

#' @title Make a 3D mesh
#' @description Make a 3D mesh from given vertices and faces; the returned
#'   faces are coherently oriented, normals are computed if desired, and
#'   triangulation is performed if desired. The mesh is also cleaned: 
#'   duplicated vertices or faces are merged, and isolated vertices are removed.
#'
#' @param vertices a numeric matrix with three columns, or a \code{bigq}
#'   matrix with three columns 
#' @param faces either an integer matrix (each row provides the vertex indices
#'   of the corresponding face) or a list of integer vectors, each one
#'   providing the vertex indices of the corresponding face
#' @param mesh if not \code{NULL}, this argument takes precedence over \code{vertices}
#'   and \code{faces}, and must be either a list containing the fields \code{vertices}
#'   and \code{faces} (objects as described above), otherwise a \strong{rgl} mesh
#'   (i.e. a \code{mesh3d} object)
#' @param triangulate Boolean, whether to triangulate the faces
#' @param normals Boolean, whether to compute the normals
#'
#' @return A list giving the vertices, the edges, the faces of the mesh, the
#'   exterior edges, the exterior vertices and optionally the normals. 
#'   If \code{triangulate=TRUE}, this list has two additional components 
#'   \code{edges0} and \code{normals0} giving the edges and the normals 
#'   before the triangulation, unless the mesh is already triangulated, 
#'   in which case the \code{triangulate} option is ignored.
#'
#' @export
#' 
#' @seealso See \code{\link{plotEdges}} for more details about the edges 
#'   returned by this function.
#'
#' @importFrom gmp as.bigq asNumeric is.matrixZQ
#'
#' @examples
#' library(PolygonSoup)
#' library(rgl)
#'
#' # a tetrahedron with ill-oriented faces ####
#' vertices <- rbind(
#'   c(-1, -1, -1),
#'   c(1, 1, -1),
#'   c(1, -1, 1),
#'   c(-1, 1, 1)
#' )
#' faces <- rbind(
#'   c(1, 2, 3),
#'   c(3, 4, 2),
#'   c(4, 2, 1),
#'   c(4, 3, 1)
#' )
#'
#' # plot the tetrahedron, hiding the back of the faces
#' # then some faces do not appear, as their orientation is not correct
#' tmesh1 <- tmesh3d(
#'   vertices = t(vertices),
#'   indices = t(faces),
#'   homogeneous = FALSE
#' )
#' open3d(windowRect = c(50, 50, 562, 562))
#' shade3d(tmesh1, color = "green", back = "cull")
#'
#' # now run the `Mesh` function
#' mesh2 <- Mesh(vertices, faces, normals = FALSE)
#' # plot the tetrahedron, hiding the back of the faces
#' # then all faces appear now
#' tmesh2 <- toRGL(mesh2)
#' open3d(windowRect = c(50, 50, 562, 562))
#' shade3d(tmesh2, color = "blue", back = "cull")
#'
#' # illustration of the cleaning feature ####
#' # we construct a mesh with a lot of duplicated vertices
#' library(misc3d) # to compute a mesh of an isosurface
#' a <- 0.94; mu <- 0.56; c <- 0.34 # cyclide parameters
#' f <- function(x, y, z, a, c, mu){ # implicit equation of the cyclide
#'   b <- sqrt(a^2 - c^2)
#'   (x^2 + y^2 + z^2 - mu^2 + b^2)^2 - 4*(a*x - c*mu)^2 - 4*b^2*y^2
#' }
#' x <- seq(-c - mu - a, abs(mu - c) + a, length.out = 45)
#' y <- seq(-mu - a, mu + a, length.out = 45)
#' z <- seq(-mu - c, mu + c, length.out = 30)
#' g <- expand.grid(x = x, y = y, z = z)
#' voxel <- array(with(g, f(x, y, z, a, c, mu)), c(45, 45, 30))
#' cont <- computeContour3d(voxel, level = 0, x = x, y = y, z = z)
#' ids <- matrix(1:nrow(cont), ncol = 3, byrow = TRUE)
#' # run the `Mesh` function 
#' mesh <- Mesh(cont, ids, normals = TRUE)
#' # plot the cyclide
#' tmesh <- toRGL(mesh)
#' open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
#' shade3d(tmesh, color = "green")
#'
#' # illustration of the `triangulate` option ####
#' # the faces of the truncated icosahedron are hexagonal or pentagonal:
#' truncatedIcosahedron[["faces"]]
#' # so we triangulate them:
#' mesh <- Mesh(
#'   mesh = truncatedIcosahedron,
#'   triangulate = TRUE, normals = FALSE
#' )
#' # now we can plot the truncated icosahedron
#' tmesh <- toRGL(mesh)
#' open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
#' shade3d(tmesh, color = "orange")
Mesh <- function(
		vertices, faces, mesh = NULL, triangulate = FALSE, normals = FALSE
){
	if(is.null(mesh)) {
	  gmp <- is.matrixZQ(vertices)
	} else {
	  gmp <- is.matrixZQ(mesh[["vertices"]])
	}
	if(!is.null(mesh)){
		if(inherits(mesh, "mesh3d")){
			vft  <- getVFT(mesh, beforeCheck = TRUE)
			mesh <- vft[["rmesh"]]
		}
		vertices <- mesh[["vertices"]]
		faces    <- mesh[["faces"]]
	}
	checkedMesh <- checkMesh(vertices, faces, gmp = gmp, aslist = TRUE)
	vertices         <- checkedMesh[["vertices"]]
	faces            <- checkedMesh[["faces"]]
	homogeneousFaces <- checkedMesh[["homogeneousFaces"]]
	isTriangle       <- checkedMesh[["isTriangle"]]
	rmesh <- list("vertices" = vertices, "faces" = faces)
  if(gmp) {
		mesh <- SurfQMesh(
				rmesh, isTriangle, triangulate, TRUE, normals
		)
	} else {
		mesh <- SurfEMesh(
				rmesh, isTriangle, triangulate, TRUE, normals
		)
	}
	if(triangulate && isTriangle) {
		message(
				"Ignored option `triangulate`, since the mesh is already triangulated."
		)
		triangulate <- FALSE
	}
	if(gmp){
		vertices <- as.bigq(t(mesh[["vertices"]]))
		mesh[["gmpVertices"]] <- vertices
		vertices <- asNumeric(vertices)
	}else{
		vertices <- t(mesh[["vertices"]])
	}
	mesh[["vertices"]] <- vertices
	edgesDF <- mesh[["edges"]]
	mesh[["edgesDF"]] <- edgesDF
	mesh[["edges"]] <- as.matrix(edgesDF[, c("i1", "i2")])
	exteriorEdges <- as.matrix(subset(edgesDF, exterior)[, c("i1", "i2")])
	mesh[["exteriorEdges"]] <- exteriorEdges
	mesh[["exteriorVertices"]] <- which(table(exteriorEdges) != 2L)
	if(normals){
		mesh[["normals"]] <- t(mesh[["normals"]])
	}
	if(triangulate){
	  edges0DF <- mesh[["edges0"]]
	  mesh[["edges0DF"]] <- edges0DF
	  mesh[["edges0"]] <- as.matrix(edges0DF[, c("i1", "i2")])
	  if(normals){
			mesh[["normals0"]] <- t(mesh[["normals0"]])
		}
	}
	if(triangulate || homogeneousFaces){
		mesh[["faces"]] <- do.call(rbind, mesh[["faces"]])
	}
	attr(mesh, "toRGL") <- ifelse(triangulate, 3L, checkedMesh[["toRGL"]])
	class(mesh) <- "cgalMesh"
	mesh
}

#' @title Conversion to 'rgl' mesh
#' @description Converts a CGAL mesh (e.g. an output of the \code{\link{Mesh}}
#'   function) to a \strong{rgl} mesh.
#'
#' @param mesh a CGAL mesh, that is to say a list of class \code{"cgalMesh"}
#'   (e.g. an output of the \code{\link{Mesh}} function); in order to be
#'   convertible to a \strong{rgl} mesh, its faces must have at most four sides
#' @param ... arguments passed to \code{\link[rgl]{mesh3d}}
#'
#' @return A \strong{rgl} mesh, that is to say a list of class \code{"mesh3d"}.
#' @export
#'
#' @importFrom rgl mesh3d
#'
#' @examples
#' library(PolygonSoup)
#' library(rgl)
#' mesh <- Mesh(
#'   truncatedIcosahedron[["vertices"]], truncatedIcosahedron[["faces"]],
#'   triangulate = TRUE
#' )
#' rglmesh <- toRGL(mesh, segments = t(mesh[["edges"]]))
#' open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
#' shade3d(rglmesh, color = "darkred")
toRGL <- function(mesh, ...){
	if(!inherits(mesh, "cgalMesh")){
		stop(
				"The `mesh` argument must be of class 'cgalMesh'",
				" (e.g. an output of the `Mesh` function)."
		)
	}
	rgl <- attr(mesh, "toRGL")
	if(isFALSE(rgl)){
		stop(
				"Impossible to convert this mesh to a 'rgl' mesh ",
				"(the faces must have at most four sides)."
		)
	}
	if(rgl == 3L){
		mesh3d(
				x         = mesh[["vertices"]],
				normals   = mesh[["normals"]],
				triangles = t(mesh[["faces"]]),
				...
		)
	}else if(rgl == 4L){
		mesh3d(
				x       = mesh[["vertices"]],
				normals = mesh[["normals"]],
				quads   = t(mesh[["faces"]]),
				...
		)
	}else{
		faces <- split(mesh[["faces"]], lengths(mesh[["faces"]]))
		mesh3d(
				x         = mesh[["vertices"]],
				normals   = mesh[["normals"]],
				triangles = do.call(cbind, faces[["3"]]),
				quads     = do.call(cbind, faces[["4"]]),
				...
		)
	}
}

#' @title Plot some edges
#' @description Plot the given edges with \strong{rgl}.
#'
#' @param vertices a three-columns matrix giving the coordinates of the vertices
#' @param edges a two-columns integer matrix giving the edges by pairs of
#'   vertex indices
#' @param color a color for the edges
#' @param lwd line width, a positive number, ignored if \code{edgesAsTubes=TRUE}
#' @param edgesAsTubes Boolean, whether to draw the edges as tubes
#' @param tubesRadius the radius of the tubes when \code{edgesAsTubes=TRUE}
#' @param verticesAsSpheres Boolean, whether to draw the vertices as spheres
#' @param only integer vector made of the indices of the vertices you want
#'   to plot (as spheres), or \code{NULL} to plot all vertices
#' @param spheresRadius the radius of the spheres when
#'   \code{verticesAsSpheres=TRUE}
#' @param spheresColor the color of the spheres when
#'   \code{verticesAsSpheres=TRUE}
#'
#' @return No value.
#'
#' @importFrom rgl cylinder3d shade3d lines3d spheres3d
#' @export
#'
#' @examples
#' library(PolygonSoup)
#' library(rgl)
#' 
#' \donttest{# we triangulate the truncated icosahedron mesh
#' mesh <- Mesh(
#'   mesh = truncatedIcosahedron,
#'   triangulate = TRUE, normals = FALSE
#' )
#' # now we can plot the truncated icosahedron
#' tmesh <- toRGL(mesh)
#' open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
#' shade3d(tmesh, color = "gold")
#' # we plot the edges given in `mesh[["edges0"]]`; these are the 
#' # edges of the mesh before the triangulation
#' plotEdges(mesh[["vertices"]], mesh[["edges0"]], color = "navy")}
#' 
#' # we triangulate the pentagrammic prism mesh
#' mesh <- Mesh(
#'   mesh = pentagrammicPrism,
#'   triangulate = TRUE, normals = FALSE
#' )
#' # now we can plot the pentagrammic prism
#' tmesh <- toRGL(mesh)
#' \donttest{open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
#' shade3d(tmesh, color = "navy")
#' # we plot the exterior edges only, given in `mesh[["exteriorEdges"]]`
#' plotEdges(
#'   mesh[["vertices"]], mesh[["exteriorEdges"]], color = "gold",
#'   tubesRadius = 0.02, spheresRadius = 0.02
#' )}
#' 
#' # or only plot the edges whose corresponding dihedral angle is acute:
#' allEdges <- mesh[["edgesDF"]]
#' edges <- as.matrix(subset(allEdges, angle <= 91, select = c("i1", "i2")))
# open3d(windowRect = c(50, 50, 562, 562), zoom = 0.9)
# shade3d(tmesh, color = "maroon")
# plotEdges(
#   mesh[["vertices"]], edges, color = "darkred", 
#   tubesRadius = 0.02, spheresRadius = 0.02
# )
plotEdges <- function(
		vertices,
		edges,
		color = "black",
		lwd = 2,
		edgesAsTubes = TRUE,
		tubesRadius = 0.03,
		verticesAsSpheres = TRUE,
		only = NULL,
		spheresRadius = 0.05,
		spheresColor = color
){
	for(i in 1L:nrow(edges)){
		edge <- edges[i, ]
		if(edgesAsTubes){
			tube <- cylinder3d(
					vertices[edge, ], radius = tubesRadius, sides = 90
			)
			shade3d(tube, color = color)
		}else{
			lines3d(vertices[edge, ], color = color, lwd = lwd)
		}
	}
	if(verticesAsSpheres){
		if(!is.null(only)){
			vertices <- vertices[only, ]
		}
		spheres3d(vertices, radius = spheresRadius, color = spheresColor)
	}
	invisible(NULL)
}
