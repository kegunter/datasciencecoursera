  ## These functions attempt to save time with caching the value of
  ## a matrix inverse.  When needing to calculate another inverse, 
  ## this will check to see if it has already been calculated.
  ## Put comments here that give an overall description of what your
  ## functions do
  
  ## This function creates a "matrix" which is a list of functions which
  ## sets the value of the matrix, gets the value of the matrix, sets the 
  ## inverse of the matrix, and gets the inverse of the matrix.
  ## Write a short comment describing this function
  
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function calculates the inverse of the matrix afrom above.
## It will check to see if it has alraedy been calculated firt.
## If so, it will retrieve the cached value, if not the value will be calculated.
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}