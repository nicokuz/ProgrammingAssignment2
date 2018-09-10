## Put comments here that give an overall description of what your
## functions do

## Returns special matrix object with functions for inverted matrix caching

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(m) {
    x <<- m
    invm <<- NULL
  }
  get <- function() x
  setinvm <- function(im) invm <<- im
  getinvm <- function() invm
  list(set = set, get = get,
       setinvm = setinvm,
       getinvm = getinvm)
}


## Return a matrix that is the inverse of 'x', caching it  

cacheSolve <- function(x, ...) {
  m <- x$getinvm()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvm(m)
  m  
}
