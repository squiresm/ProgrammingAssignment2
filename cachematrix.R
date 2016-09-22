# rm(list = ls(all = TRUE))

# Creates an object that stores (caches) a matrix and its inverse within 
# the function environment
#
# Args:
#   x: The matrix that an inverse will be calculated. The format of the
#      matrix must conform to:
#      1. numeric
#      2. square in structure (n-by-n)
#      3. A determinant that is not zero (not a singular matrix)
#
# Returns:
#   An object that contains two data objects
#   1. x (numeric matrix - see notes for matrix structure under Args comment)
#   2. The inverse matrix of x
#
makeCacheMatrix <- function(x = matrix()) {
    mi <- NULL
    set <- function(y) {
        x <<- y
        mi <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) mi <<- solve
    getInverse <- function() mi
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}


# Creates an object that accesses a cached invertible matrix if that
# invertible matrix is based on a prior set.
# If the invertible matrix has changed or not established, the
# function sets a new invertible matrix based on the data object
# passed in.
# Args:
#   x: An object created by the makeCacheMatrix(x) function.
#
# Returns:
#   An invertible  matrix 
#
cacheSolve <- function(x, ...) {
    mi <- x$getInverse()
    if(!is.null(mi)) {
        message("getting cached data")
        return(mi)
    }
    data <- x$get()
    mi <- solve(data, ...)
    x$setInverse(mi)
    mi
}

mt <- matrix(floor(runif(3, min=0, max=101)))