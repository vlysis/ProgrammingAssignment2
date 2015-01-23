## Pair of functions to allow for computation of matrix inversion with caching of result

## Create a matrix with getters & setters for solve (matrix inverse)
## Allows inversion to be stored
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Solve (invert) a matrix, with caching capability
## check to see if the inverse is already stored via the getsolve function
## If is is return that inverse.   If not compute inverse and set stored inverse
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
