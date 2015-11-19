## This is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  setmat <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  getmat <- function() x
  setInvMat <- function(solved) inv_matrix <<- solved
  getInvMat <- function() inv_matrix
  list(setmat = setmat,
       getmat = getmat,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_matrix <- x$getInvMat()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  dat <- x$getmat()
  inv_matrix <- solve(dat, ...)
  x$setInvMat(inv_matrix)
  inv_matrix
}
