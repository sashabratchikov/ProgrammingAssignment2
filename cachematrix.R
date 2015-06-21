## This pair of functions intended for calculating inversed matrix
## and storing it after it was calculated to take it next time
## from cache (if input matrix hasn't changed).
##
## It's assumed that the matrix supplied is always invertible.
## If cacheSolve function get non-invertible matrix as input
## it will return an error.

## makeCacheMatrix is a function contains the list of 4 functions:
## set changes the matrix stored in makeCacheMatrix
## get returns the matrix stored in makeCacheMatrix (or NULL)
## setSolve changes inversed matrix stored in makeCacheMatrix
## getSolve returns inversed matrix (or NULL) stored in makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
        inversed <- NULL
        set <- function(y) {
                x <<- y
                inversed <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) inversed <<- solve
        getSolve <- function() inversed
        list(set = set, get = get,
             setSolve = setSolve,
             getSolve = getSolve)
}


## This function calculates inversed matrix if getSolve 
## function returns NULL or just returns inversed matrix
## if it has already been calculated and set (if matrix
## hasn't changed).

cacheSolve <- function(x, ...) {
        inversed <- x$getSolve()
        if(!is.null(inversed)) {
                message("Getting cached data")
                return(inversed)
        }
        data <- x$get()
        inversed <- solve(data, ...)
        x$setSolve(inversed)
        inversed
}
