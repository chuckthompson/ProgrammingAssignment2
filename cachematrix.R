## R Programming by Roger D. Peng, PhD, Jeff Leek, PhD, Brian Caffo, PhD
## Programming Assignment 2
## Course ID:  rprog-009
## Submitted by Chuck Thompson

## This file contains two functions, one to create a matrix object that can cache its
## inverse and the second to calculate and cache that inverse.  The value in doing this
## is that matrix inversion is a costly computation, especially for large matrices.  If
## the original matrix has not changed, there is no need to recalculate the inverse each
## time the inverse is needed.


## makeCacheMatrix:
##     Create a matrix object that has functions associated with it to set and return the
##     stored matrix and set and return the inverse of the stored matrix.
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the cached inverse matrix value.
    invMatrix <- NULL
    
    # The set function stores a new matrix value and resets any cached inverse matrix
    # since any cached value only applied to the previous matrix value.
    set <- function(amatrix) {
        x <<- amatrix
        invMatrix <<- NULL
    }
    
    # The get function simply returns the stored matrix.
    get <- function() x
    
    # The setinverse function sets the cached inverse matrix value.
    setinverse <- function(newInvMatrix) invMatrix <<- newInvMatrix
    
    # The getinverse function simply returns the cache inverse matrix value.
    getinverse <- function() invMatrix
    
    # Return the four created functions as named values.
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve:
##     Takes a CacheMatrix, returns the cached inverse matrix value if it is set.  If not
##     set, calculate, cache, and then return the inverse matrix value.
cacheSolve <- function(x, ...) {
    # Check if a cached value already exists.  If one does, print a message to that
    # effect and return the cached value.
    invMatrix <- x$getinverse()
    if(!is.null(invMatrix)) {
        message("getting cached data")
        return(invMatrix)
    }
    
    # Get the stored matrix value; calculate its inverse; cache that value; and return it.
    data <- x$get()
    invMatrix <- solve(data, ...)
    x$setinverse(invMatrix)
    invMatrix
}
