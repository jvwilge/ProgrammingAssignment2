## Provide two methods to be able to cache an inverse matrix calculation
##
## Step 1 is wrapping your matrix in a cacheMatrix list with the 
## makeCacheMatrix function.
##
## Step 2 is retrieving the cachedMatrix via cacheSolve. cacheSolve will
## calculate the inverse matrix the first time it's called. The successive
## calls will return the cached calculation.
##
## Assumption: the matrix supplied is always invertible
##
## Comments based on https://google.github.io/styleguide/Rguide.xml#comments

makeCacheMatrix <- function(x = matrix()) {
  # Wraps a matrix in a cacheMatrix object to provide caching of the inverse
  # matrix calculation
  #
  # Args:
  #    x: a matrix
  #
  # Returns:
  #     The cacheMatrix object. This is a list with the functions : 
  #     set: set the matrix
  #     get: get the matrix
  #     setsolve: set the inverse matrix (this will be the cached entry)
  #     getsolve: gets the cached inverse matrix (return NULL when there's no 
  #               entry)
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(mean) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

cacheSolve <- function(x, ...) {
  # Search the cache for the inverse matrix of x. When the entry is not in the
  # cache it's calculated, saved in the cache and returned.
  #
  # Args:
  #     x: a matrix wrapped in a cacheMatrix object (which can be created with 
  #     the makeCacheMatrix function)
  #
  # Returns:
  #    The inverse matrix (a 'matrix' object, NOT wrapped in the cacheMatrix)
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