isAtomicVector <- function(x){
	is.atomic(x) && is.vector(x)
}

isPositiveNumber <- function(x){
	is.numeric(x) && length(x) == 1L && x > 0 && !is.na(x)
}

isNonNegativeNumber <- function(x){
	is.numeric(x) && length(x) == 1L && x >= 0 && !is.na(x)
}

isPositiveInteger <- function(x){
	is.numeric(x) && length(x) == 1L && !is.na(x) && floor(x) == x
}

isStrictPositiveInteger <- function(x){
	isPositiveInteger(x) && x > 0
}

isBoolean <- function(x){
	is.logical(x) && length(x) == 1L && !is.na(x)
}

isString <- function(x){
  is.character(x) && length(x) == 1L && !is.na(x)
}

#' @importFrom gmp `%*%`
#' @noRd
`%^%` <- function(A, n){
	Reduce(gmp::`%*%`, replicate(n, A, simplify = FALSE))
}

getVFT <- function(mesh, beforeCheck = FALSE){
	transposed <- !beforeCheck
	i0 <- as.integer(transposed)
	if(inherits(mesh, "mesh3d")){
		triangles <- mesh[["it"]]
		if(!is.null(triangles)){
			triangles <- lapply(1L:ncol(triangles), function(i) triangles[, i] - i0)
		}
		quads <- mesh[["ib"]]
		isTriangle <- is.null(quads)
		if(!isTriangle){
			quads <- lapply(1L:ncol(quads), function(i) quads[, i] - i0)
		}
		faces <- c(triangles, quads)
		vertices <- mesh[["vb"]][-4L, ]
		if(!transposed){
			vertices <- t(vertices)
		}
		rmesh <- list("vertices" = vertices, "faces" = faces)
	}else if(inherits(mesh, "cgalMesh")){
		isTriangle <- attr(mesh, "toRGL") == 3L
		vertices <- mesh[["vertices"]]
		if(transposed){
			vertices <- t(vertices)
		}
		faces <- mesh[["faces"]]
		if(is.matrix(faces)){
			faces <- lapply(1L:nrow(faces), function(i) faces[i, ] - i0)
		}else if(!beforeCheck){
			faces <- lapply(faces, function(face) face - 1L)
		}
		rmesh <- list("vertices" = vertices, "faces" = faces)
	}else if(is.list(mesh)){
		rmesh <-
				checkMesh(mesh[["vertices"]], mesh[["faces"]], gmp = FALSE, aslist = TRUE)
		isTriangle <- rmesh[["isTriangle"]]
		if(beforeCheck){
			rmesh[["vertices"]] <- t(rmesh[["vertices"]])
			rmesh[["faces"]] <- lapply(rmesh[["faces"]], function(face) face + 1L)
		}
	}else{
		stop("Invalid `mesh` argument.", call. = FALSE)
	}
	list("rmesh" = rmesh, "isTriangle" = isTriangle)
}

