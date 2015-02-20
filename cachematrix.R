#Matrix inversion is usually costly computation and there may be some benefit 
#to chaching the inverse of a matrix rather than compute it repeatedly.
#MakeCacheMatrix creates a special matrix object that can cache its inverse.
#set the value of the matrix
#get the value of the matrix
#set the value of inverse of the matrix
#get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


# The following function computes the inverse of the matrix returned by the above function.
#It first checks if the inverse has already been computed. If so, it retrieves the result from cache.
#If not, it computes the inverse, sets the value in the cache via setInverse function.

# Assumption: matrix is always invertible.
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
