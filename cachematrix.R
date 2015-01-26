## cachematrix.R
## The following functions provide a mechanism for caching the result of a time
## consuming operation, in this case calculating the inverse of a matrix. These
## functions will prevent re-calculating the inverse of a matrix if it has been
## already done, returning the cached inverse matrix.

## This functions return a list of functions to:
## - Set the value of a matrix
## - Set the inverse of the matrix
## - Return the value of the matrix
## - Return the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_mtrx <- NULL
    
    set <- function(y) {
        x <<- y
        inv_mtrx <<- NULL
    }
    
    get <- function() x
    
    setinverse <- function(invrs) inv_mtrx <<- invrs
    
    getinverse <- function() inv_mtrx
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function will calculate the inverse of a matrix, provided it hasn't been
## done previously, otherwise it will return the cached value.

cacheSolve <- function(x, ...) {
    
    inverse <- x$getinverse()
    
    ## Verify if the inverse has been calculated before, if so return the cached
    ## value
    if( !is.null(inverse) ) {
        message("getting chached inverse matrix")
        return(inverse)
    }
    
    mtrx <- x$get()
    
    ## Calculate the inverse of the matrix in case it has not been calculated
    inverse <- solve(mtrx, ...)
    
    x$setinverse(inverse)
    
    ## Return a matrix that is the inverse of 'x'
    inverse
}
