## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## This package caches the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        ## Creates a special object that can cache its inverse
        ## There are four functions created in the object:
        ## 1. set the value of the matrix
        ## 2. get the value of the matrix
        ## 3. set the value of the inverse
        ## 4. get the value of the inverse
        
        invertedMatrix <- NULL  ## create placeholder for inverted matrix
        
        set <- function (y) {
                x <<- y
                invertedMatrix <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) invertedMatrix <<- solve
        getinverse <- function() invertedMatrix
        
        ## Return the special object containing the original matrix and inverse
        list (set = set, get = get, setinverse = setinverse, 
              getinverse = getinverse)

}

## This function computes the inverse of the matrix in the special object 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cacheSolve will 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of the matrix in 'x'
        ## Assumes the argument 'x' is a square, invertible matrix
        
        m <- x$getinverse()
        
        ## if the inverted matrix value is not NULL, return the cached data
        if (!is.null(m)) {
                message ("getting cached data")
                return (m)
        }
        
        ## if the cache does not exist, create & return inverted matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        
        m
}
