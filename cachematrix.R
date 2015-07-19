## Name: cachematrix.R
## This script contains a set of functions that calculates the inverse of
## a matrix and supports caching of the results

## Function: makeCacheMatrix
## This function takes a matrix as an input parameter. It returns an object that 
## supports caching.This object can used to the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inver) m <<- inver
  getinverse <- function() m
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function: cacheSolve
## This function takes the cache-enabled object as an input. It checks whether
## a cached object is available. If not, it calculates the inverse of the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
