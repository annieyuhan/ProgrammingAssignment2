## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse 
## of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we 
## will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.

## This function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## set the value of the matrix
## get the value of the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	       ## set s to store the inverse value
                s <- NULL
                set <- function(y) {
                  x <<- y
                  s <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) s <<- inverse
                getinverse <- function() s
            ## use list to store the value of this function
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
                
}

## This function cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            s <- x$getinverse()
        ## If there is inverse value stored, directly read it
            if(!is.null(s)) {
              message("getting cached data")
              return(s)
            }
        ## If no value is found, use solve function to get the inverse, store it and return the value
            data <- x$get()
            s <- solve(data, ...)
            x$setinverse(s)
            s                   
}

