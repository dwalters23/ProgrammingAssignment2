## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly. This assignment is to write a pair of functions
## that cache the inverse of a matrix.

## This function creates a special "matrix" function that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    ## Create and initialize working variable
    m <- NULL
    ## Create the "set" function and set the value from the 
    ## calling environment
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## Create the "get" function, returning the matrix value
    get <- function() x
    ## Create the "setinverse" function, setting the value from the 
    ## calling environment
    setinverse <- function(inverse) m <<- inverse
    ## Create the "getinverse" function, getting the value from the 
    ## calling environment
    getinverse <- function() m
    ## Create the return vector of function calls
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already
## been calculated (and the matrix has not changed), then
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## If the inverse is already calculated, return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## If the inverse is not already calculated, do the calculation
    ## and then return it
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
