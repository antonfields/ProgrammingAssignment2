## Put comments here that give an overall description of what your
## functions do
#
# This script contains a pair of functions that allow the inverse of a matrix
# to be cached and retrieved rather than recalculated if the matrix has not
# been changed.


## Write a short comment describing this function
#
# This function creates a special matrix object with four functions
# that can be applied:
#                     set - sets the matrix object
#                     get - retrieves the matrix
#                     setinv - sets the inverse
#                     getinv - gets the inverse
# 
# The function takes advantage of the lexical scoping capability in R
# so that the matrix and it's inverse are available in the parent
# environment - done using the <<- assignment operator.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
# This function returns the inverse of a matrix - first checking to see
# if there is an inverse already calculated and cached. If not then the inverse 
# is calculated and cached for potential future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}

