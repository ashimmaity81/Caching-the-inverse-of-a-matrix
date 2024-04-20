# Function to create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  # Initialize a variable to store the cached inverse matrix
  inv <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Function to get the matrix
  get <- function() {
    x
  }
  
  # Function to get the cached inverse matrix
  getInverse <- function() {
    inv
  }
  
  # Function to cache the inverse of the matrix
  cacheInverse <- function(solve) {
    inv <<- solve
  }
  
  # Return a list containing the functions
  list(set = set, get = get, getInverse = getInverse, cacheInverse = cacheInverse)
}

# Function to compute the inverse of the special "matrix" object
cacheSolve <- function(x, ...) {
  
  # Get the cached inverse matrix if available
  inv <- x$getInverse()
  
  # If the inverse is not cached, compute it and cache it
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  # If the inverse is not cached, compute it and cache it
  data <- x$get()
  inv <- solve(data, ...)
  x$cacheInverse(inv)
  inv
}
# Create a special "matrix" object
mat <- makeCacheMatrix(matrix(1:4, 2, 2))

# Compute the inverse of the matrix
cacheSolve(mat)
