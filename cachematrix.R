## Make a cacheable matrix from an existing matrix
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() {
    x
  }
  setsolve <- function(i) {
    s <<- i
  }
  getsolve <- function() {
    s
  }
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## Calculates the inverse of a cacheable matrix created originally from makeCacheMatrix()
##
## If the inverse was already calculated, return it from the cache
cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("Getting cached result")
    return(s)
  }
  data <- x$get()
  s <- solve(data)
  x$setsolve(s)
  s
}
