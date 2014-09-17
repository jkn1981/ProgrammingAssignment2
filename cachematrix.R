## Code written in order to cache the inverse of a matrix. In particular,
## there are 2 functions:
## - makeCacheMatrix, which caches the inverse of a matrix
## - cacheSolve, which returns the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
     m <- NULL
     
     # Set the value of the matrix (matrix x is cached, inverse m isn't yet)
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     
     # Get the value of the matrix (return x)
     get <- function() x
     
     # Set the value of the inverse matrix (calculate inverse m and cache it)
     setsolve <- function(solve) m <<- solve
     
     # Get the value of the inverse matrix (return m)
     getsolve <- function() m
     
     # List of functions of the object that can be used
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        
     # Try to retrieve the inverse matrix from the cached object
     m <- x$getsolve()
     
     # If the inverse exists, don't calculate it: just retrieve the cached one
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     
     # If it doesn't exist, get the original matrix...
     data <- x$get()
     
     #... calculate its inverse...
     m <- solve(data, ...)
     
     #... and cache it to avoid repeating calculations the next time
     x$setsolve(m)
     
     # Return matrix m, which is the inverse of matrix x
     m
}
