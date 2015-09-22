## Functions to be able to cache the inverse of a matrix, in order to save time
## if the inverted matrix is needed more than once.

# List of functions that sets value of a matrix, gets value of a matrix,
# sets the inverse or gets the inverse.
makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL
  set <- function(y) {
    x <<- y
    matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) matrix <<- solve
  getinverse <- function() matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# Solve the inverse of a matrix, returning the cached inverse matrix if it
# exists.
cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix <- x$getinverse()
  if(!is.null(matrix)) {
    message("getting cached data")
    return(matrix)
  }
  data <- x$get()
  matrix <- solve(data, ...)
  x$setinverse(matrix)
  return(matrix)
}
