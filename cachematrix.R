## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a 'matrix' with getter and setter method
# returns cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function computes inverse of the 'matrix' returned by
# makeCacheMatrix, unless it has already been calculated
# Then it retrives it from cache

cacheSolve <- function(x, ...) {
          ## Return a matrix that is the inverse of 1:16'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
