## Cache the inverse of a matrix.
##
## Usage
## =====
## source("cachematrix.R")
## x <- stats::rnorm(16)
## dim(x) <- c(4,4)
## mx <- makeCacheMatrix(x)
## cacheSolve(mx)
## cacheSolve(mx)  # The second time you will get the already cached data


## makeCacheMatrix: This function creates a special "matrix" object that 
## can cache its inverse. 
## Important: Matrix must be square!

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve    
    getmatrix <- function() m
    list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("Getting cached data.")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}