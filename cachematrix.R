## Put comments here that give an overall description of what your
## functions do
# makeCacheMatrix prepares the matrix cache

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverted) inv <<- inverted
  getInv <- function() inv
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
# cacheSolve gets the inversed of the matrix from cache if already calculated and the matrix is identical

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if(!is.null(inv)) {
      message("getting cached data")
      return(inv)
    }
    matr <- x$get()
    invertedMatrix <- solve(matr, ...)
    x$setInv(invertedMatrix)
    invertedMatrix
}
