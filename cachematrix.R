## The two functions serve to create a special "matrix" (list) object that allows for the inverse of the matrix
## to be computed and retreived from cache.  This method relies on the super-assignment operator to make the
## "matrix" (list) properties available to other environments.


## Creates the "matrix" list of functions so that we might be able to retrieve a cached inverse value

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Checks to see if the inverse exists in cache, and returns the inverse of the matrix

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
