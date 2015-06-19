## Put comments here that give an overall description of what your
## functions do
## 
## Functions to implement a matrix that keeps a cached copy of its inverse and
## reuses it in future calls.

## Write a short comment describing this function
##
## Implements and packages a set of functions to provide a matrix object that
## caches its inverse after it is initially computed and resets it when the 
## matrix is changed.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set=set,
       get=get,
       setinv=setinv,
       getinv=getinv)
}


## Write a short comment describing this function
##
## Implements a function to return the matrix inverse of its input, using 
## caching to save recomputing the inverse if the matrix hasn't changed since
## the last call to this function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
