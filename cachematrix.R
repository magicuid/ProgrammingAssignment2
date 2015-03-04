## These functions perform matrix inversion and cache the result
## in a global variable.

## This function provides an interface to save the cached data
## to a global variable for retrieval.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(invert) m <<- invert
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function returns a matrix that is inverse of
## x. It only performs the solve computation if no
## cached solution exist.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  
  # Cache solution exists
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # No available cached solution
  data <- x$get()
  m <- solve(data) %*% data
  x$setinverse(m)
  m
  
}