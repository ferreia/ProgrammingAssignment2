## The two functions below can be combined to create objects
## that cache a matrix and its inverse and calculate the 
## inverse of matrices stored in those objects to avoid computation
## by returning a previously cached inverse matrix.
## Example usage:
# 
# > m = makeCacheMatrix(matrix(c(1,3,5,7),nrow=2,ncol=2))
# > cacheSolve(m)
# calculating and caching inverse matrix
# [,1]   [,2]
# [1,] -0.875  0.625
# [2,]  0.375 -0.125
# > cacheSolve(m)
# returning cached inverse matrix
# [,1]   [,2]
# [1,] -0.875  0.625
# [2,]  0.375 -0.125



## This function returns a list with four functions:
## get, set, getinv and setinv that cache and return
## the a matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverse.mat <- NULL
  
  # caches a new matrix, mat, and resets its inverse, inverse.mat, to NULL
  set <- function(mat) {
    x <<- mat
    inverse.mat <<- NULL
  }
  
  # returns the cached matrix
  get <- function() x
  
  # caches the inverse in inverse.mat variable
  setinv <- function(inv) inverse.mat <<- inv
  
  # returns the cached inverse 
  getinv <- function() inverse.mat
  
  # return a list with the get and set functions
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes an object created by a call to 'makeCacheMatrix'
## that contains and cached matrix and possibly its inverse
## if the inverse is not yet cached, it calculates it by calling the 'solve'
## function and caching the resulting inverse before returning it

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("returning cached inverse matrix")
    return(inv)
  }
  
  # cached inverse is NULL, calculate it 
  message("calculating and caching inverse matrix")
  inv <- solve(x$get(), ...)
  
  # cache the result
  x$setinv(inv)
  
  inv
}
