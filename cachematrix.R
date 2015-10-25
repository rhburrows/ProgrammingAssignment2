## Matrix inversion can be an expensive computation. The following file provides
## functions to help by caching previously calculated values to avoid unecessary
## compulations.

## This function creates a special matrix that is really a list of 4 functions:
##   1. Get the value of the matrix
##   2. Set the value of the matrix
##   3. Get the inverse of the matrix
##   4. Set the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  getInverse <- function() m
  setInverse <- function(y) m <<- y
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}


## This function taks of the of the above special matrices, calculates the
## inverse if needed, caches the value, and returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
