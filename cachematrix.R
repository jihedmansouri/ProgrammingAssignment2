## Functions to solve a matrix and cache the result
## Use:
## Create a matrix:             m <- matrix(1:4,2,2)
## Build the functions vector:  cachedM <- makeCacheMatrix(m)
## Solve the matrix:            cacheSolve(cachedM)

## makeCacheMatrix creates a vector holding the inverted matrix if any
## and the functions for getting and setting the solved one.
## Takes the matrix to solve as argument.

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


## cacheSolve returns the cached solved matrix if any,
## otherwise it computes the inverted matrix and caches it.
## Takes the vector returned by makeCachedMatrix as argument.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        
        ## Little bonus
        ## Catch any error that can be raised by "solve" and display
        ## a simplified error message
        tryCatch(
          {
            data <- x$get()
            m <- solve(data, ...)
            x$setsolve(m)
            m
          },
          error=function(cond) {
            message("Matrix cannot be inverted!")
          }
        )
}
