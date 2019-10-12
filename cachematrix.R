## Programming Assignment 2: Lexical Scoping
## Goal - to create a pair of functions that stores a matrix and caches its inverse.

## makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## methods to get/set matrix
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  ## Return a list of the methods
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return the inverse of 'x' matrix
  i <- x$getInverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
    mat <- x$get()
  i <- solve(mat, ...)
  ## Set the inverse to the object
  x$setInverse(i)
  i
}
