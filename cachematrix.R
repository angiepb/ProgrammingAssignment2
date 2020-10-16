## Both functions work together to compute the inverse of a matrix.
## However, if the inverse was already stored in the makeCacheMatrix
## cacheSolve will skip the computation and get the result from the cache

## This function cache the inverse of a matrix

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of a matrix. If the inverse has already been calculated
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
