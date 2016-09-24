## This file provides functions makeCacheMatrix and cacheSolve that can speed
## up computations involving matrix inversion by caching inversion results.

## makeCacheMatrix creates an object that can hold a matrix with its inverse
## lazily cached.
makeCacheMatrix <- function(x = matrix()) {
        # inv is used for caching the inverse of x
        inv <- NULL
        # set is setter for x
        set <- function(y) {
                x <<- y
                # clear previously cached inverse
                inv <<- NULL
        }
        # get is getter for x
        get <- function() x
        # setinv is setter for inv
        setinv <- function(newinv) inv <<- newinv
        # getinv is getter for inv
        getinv <- function() inv
        # return list of setters and getters
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of the matrix held by x. It uses the inverse
## matrix cached from a previous invocation if any, or computes newly using
## solve() and caches the result.
cacheSolve <- function(x, ...) {
        # use cached data if any
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # calculate inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        # cache inverse
        x$setinv(inv)
        # return inverse
        inv
}
