## the functions return an inverted matrix.  if the calculation has
## already been performed, then the inverted matrix is retrieved from a cache
## and the calculation is not repeated to save computation time


## makeCacheMatrix is a list of functions which retrieve or set the matrix
## that is going to be inverted, and retrieve or set the inverse into cache

makeCacheMatrix <- function(x = matrix()) {
 	  InvMat <- NULL
        set <- function(y) {
                x <<- y
                InvMat <<- NULL
        }
        get <- function() x
        setInvMat <- function(Inverse) InvMat <<- Inverse
        getInvMat <- function() InvMat
        list(set = set, get = get,
             setInvMat = setInvMat,
             getInvMat = getInvMat)
}


## cacheSolve inverts the matrix when data is the inverted matrix value is NULL,
## or retrieves the cached value if it already is exists 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  InvMat <- x$getInvMat()
        if(!is.null(InvMat)) {
                message("getting cached data")
                return(InvMat)
        }
        matrix4inv <- x$get()
        InvMat <- solve(matrix4inv, ...)
        x$setInvMat(InvMat)
        InvMat
}
