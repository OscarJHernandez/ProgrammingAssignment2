#======================================================================
## This function Makes a Cached Matrix
#======================================================================
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize the Cached Matrix
  m <- NULL
  
  # Here we set a value for the current matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Get the current matrix
  get <- function() x
  
  # Here we Set the Inverse Matrix
  setInverse <- function(inverse) m <<- inverse
  
  # Here we Retrieve the Inverse Matrix
  getInverse <- function() inverse
  
  
  # Lists Available functions for the Cached Matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}

#======================================================================
## Retrieve the inverse of the Matrix, or compute it if its not cached
#======================================================================
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Retrieve the inverse of a Matrix
  m <- x$getInverse()
  
  # If m is not null, then retrieve the Cached Data
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  #=====================================================
  # When the inverse has not previously been calculated
  #=====================================================
  
  # Obtain the current matrix
  data <- x$get()
  
  # Calculate the Inverse of the Matrix
  m <- solve(data, ...)
  
  # Now we set the Inverse Matrix, and store it in x
  x$setInverse(m)
  
  # Return the Calculated Matrix
  m
  
}
