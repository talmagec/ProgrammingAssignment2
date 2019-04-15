## makeCacheMatrix creates an object that can hold a matrix and cache
## its inverse.  Used by cacheSolve to return inverse from cache or
## calculate and store to cache.

## Creates a matrixCache object that holds a matrix and caches its index

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Using cacheMatrix object created by makeCacheMatrix,
## check if inverse already cached.  If so, return.  If not,
## calculate, set cache, and return.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
