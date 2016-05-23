## Date : 18May2016:wq
## Put comments here that give an overall description of what your
## functions do

## Assignment: Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly i
## This assignment is to write a pair of functions that cache the inverse of a matrix.


## Write a short comment describing this function

##This function creates a special "matrix" object that can cache its inverse.:w!


makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
 set <- function(y) {
                       x <<- y
                       inv <<- NULL
                     }
 get <- function() x
 setInverse <- function(inverse) inv <<- inverse
 getInverse <- function() inv
 list(set = set,
      get = get,
      setInverse = setInverse,
      getInverse = getInverse)

}


## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
##caching the inverse of a matrix rather than computing it repeatedly
cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'
   inv <- x$getInverse()
   if (!is.null(inv)) {
           message("getting cached data")
           return(inv)
	        }
   mm <- x$get()
   inv <- solve(mm, ...)
   x$setInverse(inv)
   inv
										
}
