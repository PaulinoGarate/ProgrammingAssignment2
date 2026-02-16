## This file defines two functions:
## 1) makeCacheMatrix(): creates a special matrix object that can cache its inverse
## 2) cacheSolve(): returns the inverse of the matrix, using the cached value if available

## makeCacheMatrix creates a list of functions to set/get the matrix
## and set/get the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the matrix stored in the object.
## If an inverse has already been computed and cached, it returns that instead.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
