## Put comments here that give an overall description of what your
## functions do

## This function returns a cacheMatrix list ( with get, set, etc.) operating
## on a vector (assumed to be an invertable matrix)
## No checking of the x variable is done.
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function(){
    return (x)
  }
  getInverse <- function() { invM }
  
  setInverse <- function(inv) {
    invM <<- inv
  }
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## returns the inverse of a given matrix if a cached value is availble 
## otherwise computes the inverse, caches the value and return it
## Extra parameters are passed to the inverse function

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {
          message("getting cached inverse data")
          return(m)
        }
        m <- solve(x$get(), ...)
        x$setInverse(m)
        m
}
