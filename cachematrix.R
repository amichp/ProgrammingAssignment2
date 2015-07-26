
## This function creates a special "matrix" object which is really a list 
## containing a function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated,
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getsolve()
    
    # if the inverse has already been calculated
    if(!is.null(m)) {
        # get it from the cache and skips the computation.
        message("getting cached data")
        return(m)
    }
    # otherwise, calculates the inverse 
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m 
}
