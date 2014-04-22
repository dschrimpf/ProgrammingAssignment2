## The function gives the posibility to create a cached matrix (only for the inverse matrix) - makeCacheMatrix
## the second method returns the inverse matrix - cacheSolve

## The function creates a cached matrix with a set of functions (get, set, getInverse, setInverse)
## To set the inverse matrix please call the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) m <<- inverseMatrix
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes a cacheMatrix "object". If the object has already an calculated inverse matrix 
## it returns the inverse matrix. Otherwise it performe the calculation, cache the result and returns the inverse matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
