
## 1. makeCacheMatrix:
##    This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
    cache <- NULL
    setMatrix <- function(newValue) {
        x <<- newValue
        cache <<- NULL
    }
    getMatrix <- function() {
        x
    }
    cacheInverse <- function(solve) {
        cache <<- solve
    }
    getInverse <- function() {
        cache
    }
    list(
        setMatrix = setMatrix,
        getMatrix = getMatrix,
        cacheInverse = cacheInverse,
        getInverse = getInverse)
}


## 1. cacheSolve:
##    This function computes the inverse of the special "matrix" returned by
##    makeCacheMatrix above. If the inverse has already been calculated (and the
##    matrix has not changed), then the cachesolve will retrieve the inverse
##    from the cache.

cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("Cached data found. Getting cached data...")
        return(inverse)
    }
    message("Cached data not found. Calculating inverse matrix...")
    data <- x$getMatrix()
    inverse <- solve(data)
    x$cacheInverse(inverse)
    inverse
}
