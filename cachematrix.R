## The two functions below are used to create a sequre matrix and invert it

## This first function is used to create a sequre matrix

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(v,d1,d2) {
            x <<- matrix(v,d1,d2)
            m <<- NULL
      }
      get <- function() x
      setinvert <- function(solve) m <<- solve
      getinvert <- function() m
      list(set = set, get = get,
           setinvert = setinvert,
           getinvert = getinvert)
}


## The below function is used to return an inverse of a sequre matrix from
## cache or calculate it if it is not exsisted.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      m <- x$getinvert()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinvert(m)
      m
}
