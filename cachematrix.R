## This pair of functions caches a matrix that is passed in,
## calculates its inverse and caches the inverse so that the calculation
## only needs to be performed one time. This function may be
## invoked in a loop and the calculation may consume a significant
## of time so it makes sense to cache the values

## makeCacheMatrix sets up a list of functions that is used by cacheSolve
## to set (cache) and get the original matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        matinv <- NULL
        set <- function(y) {
                x <<- y
                matinv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) matinv <<- solve
        getinv <- function() matinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve finds the inverse of a matrix. It initially calls
## makeCacheMatrix to prepare a list of utility functions that implement
## the caching of the matrix and its inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matinv <- x$getinv()
        if(!is.null(matinv)) {
                message("getting cached data")
                return(matinv)
        }
        data <- x$get()
        matinv <- solve(data, ...)
        x$setinv(matinv)
        matinv
}
