# Matrix inversion is usually a costly computation and there may be
# some benefit to caching the inverse of a matrix rather than computing
# it repeatedly. This file contains a pair of functions that allow to cache 
# the inverse of a matrix.

## makeCacheMatrix() creates a special "matrix" object that can 
## cache its inverse and returns a list containing the internal
## functions ('methods') so a calling function
## knows how to access those methods

makeCacheMatrix <- function(x = matrix()) {
## "i" variable contains matrix inverse computed by cacheSolve()
## and is reseted to NULL every time makeCacheMatrix() is called
        i <- NULL
## get() method returns original matrix used to call makeCacheMatrix()
        get <- function() {x}
## setinverse() method is used to save matrix inverse computed by cacheSolve()
## "<<-" is used to assign matrix inverse to "i" variable in a parent environment
        setinverse <- function(inverse) {i <<- inverse}
## getinverse() method returns matrix inverse computed by cacheSolve()
        getinverse <- function() {i}
        
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
}


## cacheSolve() computes the inverse of matrix used to call makeCacheMatrix() function.
## If the inverse has already been calculated, then cacheSolve retrieves the inverse from the cache.
## cacheSolve() returns inverse matrix, either calculated or cached

cacheSolve <- function(x, ...) {
## "i" variable gets matrix inverse computed by cacheSolve() earlier.
## If there is no matrix inverse, NULL is returned by getinverse()
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
## "data" variable gets original matrix used to call makeCacheMatrix()
        data <- x$get()
## "i" variable is used to store calculated matrix inverse 
        i <- solve(data, ...)
## sets inverse matrix variable in makeCacheMatrix() object
        x$setinverse(i)
        i
}

