## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
  inv <- NULL
  
  ## Method Set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Method Get the matrix
  get <- function() x
  
  ## Method Set the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  ## Method Get the inverse of the matrix
  getInverse <- function() inv
  
  ## Return a list of the methods
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Get the matrix
  mat <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  inv <- solve(mat) %*% mat
  
  ## Set the inverse to the object
  x$setInverse(inv)
  
  ## Return the inverse matrix
  inv
}
