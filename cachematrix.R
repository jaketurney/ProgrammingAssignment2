## Put comments here that give an overall description of what your
## functions do:
## 
## Matrix inversion can be a tedious computation. There is good 
## benefit to caching the inverse of a matrix instead of repeating the computation.
## Below two functions that first create a special object that 
## stores a matrix and then caches its inverse.

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse.


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

## The function below computes the inverse of the special matrix created by the makeCacheMatrix function. 
##If the inverse has already been calculated (and the matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting Cached Data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}
