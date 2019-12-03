## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than
## compute it repeatedly.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mattoinv <- x$get()
    inv <- solve(mattoinv, ...)
    x$setInverse(inv)
    inv
}
