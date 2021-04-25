## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invM <<- inverse
  getInverse <- function() invM
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse )
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invM <- x$getInverse()
  if(!is.null(invM)) {
    message("getting cached data")
    return(invM)
  }
  matr <- x$get()
  invM <- solve(matr, ...)
  x$setInverse(invM)
  invM
}
