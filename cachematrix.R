## Author: Tom Cassidy
## Date: 9/14/18
## Project: Programming Assignment 2 for R Programming

## The following functions create and then invert a matrix

## This functions creates a matrix that contains functions
## used later to get the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  ## set NULL
  m <- NULL
  
  ## set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## return matrix
  get <- function() x
  
  ## uses the inverse function to set inverse
  setinv <- function(inverse) inv <<- inverse
  
  ## retrieves inverse
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Getting data in cache")
    return(inv)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(inv)
  inv
}
