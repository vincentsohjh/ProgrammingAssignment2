## This is a pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv_matrix <- NULL
  
  #set the matrix - default function
  setmat <- function(y) {
    x <<- y
    inv_matrix <<- NULL
  }
  
  
  #other sub-functions
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
  
  # It calls the matrix from the cache matrix, and check if an inverse is set.
  inv_matrix <- x$getInvMat()
  if(!is.null(inv_matrix)) {
    message("getting cached data")
    return(inv_matrix)
  }
  
  #If inverse matrix is not set, it will call the dunctions to solve the inverse matrix.
  dat <- x$getmat()
  inv_matrix <- solve(dat, ...)
  x$setInvMat(inv_matrix)
  inv_matrix
  
  #In R, getting the inverse matrix is use the solve function.
  
}
