# rm(list = ls(all = TRUE))

# Creates an object that caches a matrix and its inverse within 
# the function environment
#
# Args:
#   x: The matrix that an inverse will be calculated. The format of the
#      matrix must conform to:
#      1. numeric
#      2. square in structure (n-by-n)
#      3. A determinant that is not zero (not a singular matrix)
# A rudimentary check is conducted on the parameter matrix to ensure
# that it is a numeric, square matrix. This test does not identify
# a matrix that is singular and hence not invertible.
#
# Returns:
#   An object that contains two data objects
#   1. x (numeric matrix - see notes for matrix structure under Args comment)
#   2. The inverse matrix of x
#
makeCacheMatrix <- function(x = matrix()) {
    if(!is.numeric(x)) {
            if(is.character(x)) {stop("This function only accepts a numeric, square matrix!")}
    }
    if(nrow(x) != ncol(x)) {stop("This function only accepts a numeric, square matrix!")}
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

# Test Results-------------------------------------------
# set.seed(77)
# mx <- matrix(floor(runif(16, min=1, max=10)), nrow = 4)
# cacheSolve(mcm)
#            [,1]       [,2]       [,3]       [,4]
# [1,] -0.16190476  0.2571429  0.1428571 -0.1619048
# [2,]  0.24761905  0.3714286  0.4285714 -0.7523810
# [3,]  0.00952381 -0.4857143 -0.7142857  1.0095238
# [4,] -0.15238095 -0.2285714  0.4285714 -0.1523810
#> cacheSolve(mcm)
#getting cached data
#            [,1]       [,2]       [,3]       [,4]
# [1,] -0.16190476  0.2571429  0.1428571 -0.1619048
# [2,]  0.24761905  0.3714286  0.4285714 -0.7523810
# [3,]  0.00952381 -0.4857143 -0.7142857  1.0095238
# [4,] -0.15238095 -0.2285714  0.4285714 -0.1523810
#
# Test for non-numeric
# mchar <- matrix(rep(c("a", "b", "c", "d"),4), nrow = 4) 
# mcc <- makeCacheMatrix(mchar)
# Error in makeCacheMatrix(mchar) : 
#    This function only accepts a numeric, square matrix!
#
# Test for a numeric, non-square matrix
# set.seed(77)
# mx <- matrix(floor(runif(16, min=1, max=10)), nrow = 2)
# mcm <- makeCacheMatrix(mx)
# Error in makeCacheMatrix(mx) : 
# This function only accepts a numeric, square matrix!
# -------------------------------------------------------
