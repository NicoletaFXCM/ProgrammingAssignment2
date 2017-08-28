
## makeCacheMatrix creates a special matrix object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(solve) inv <- solve
      getInverse <- function() inv
      list(set = set, get = get, setInv = setInv,
           getInv = getInv)
}


## cacheSolve computes the inverse of the 
## special matrix returned by makeCacheMatrix;
## if the reverse has already been calculated,
## the cacheSolve retrieves the inverse
## from the cache.


cacheSolve <- function(x, ...) {
      inv <- x$getInv()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setInv(inv)
      inv
}
