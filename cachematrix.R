## R Programming Assignment
## Project Assignment #2
## Programmer: Lorraine Figueroa (nyguerrillagirl@brainycode.com)
## Date: 1/24/2015

## This function creates a special "matrix" object that can
## cache its inverse, in order to get this done it will utilize
## two variables outside the function environment, inverseM (holds the
## cached inverse matrix (if calculated)) and theMatrix (holds the initial/current matrix
## object).
## Expected Usage:
##      1. Create a matrix object (e.g. matA <- matrix(data = c(4,3,3,2), nrow=2, ncol=2 ))
##      2. Create a makeCacheMatrix object using object from (1)
##          (e.g. x <- makeCacheMatrix(matA))
##      3. Now test calculating the Inverse matrix of matA (or disguised as x)
##          (e.g. xI <- cacheSolve(x))
##      The first time invoking cacheSolve(x) the actual inverse of x will be computed (no message)
##      All subsequent calls will print the message "getting cached data" because it
##      will detect that the makeCacheMatrix object already has the inverse!

makeCacheMatrix <- function(theMatrix = matrix()) {
    inverseM <- NULL
    # Use to establish/set initial matrix x
    set <- function(y) {
        theMatrix <<- y
        inverseM <<- NULL
    }
    
    # Use to obtain the theMatrix
    get <- function() theMatrix
    
    # Use to set the current inverse of theMatrix
    setInverse <- function(inverseValue) inverseM <<- inverseValue
    
    # Use to obtain/retrieve the inverse of theMatrix
    getInverse <- function() inverseM
    
    # Establish list of function this object supports
    list(set = set, get=get, setInverse = setInverse, getInverse = getInverse)
    
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    if (!is.null(m)) {
        message("getting cached data")
        return (m)
    }
    # No cached data found - therefore calculate
    data <- x$get()
    m <- solve(data,...)
    x$setInverse(m)
    m
}


testCacheSolve <- function() {
  matA <- matrix(data = c(4,3,3,2), nrow=2, ncol=2 )
  x <- makeCacheMatrix(matA)  
  # This returns non-cached data (hence no message)
  xI <- cacheSolve(x)
  # This returns cached data (see message!)
  xI <- cacheSolve(x)
  
}