## The follwing functions compute the inverse of a "matrix" by caching 
## already calculated results thereby reducing computation time

## This function returns a created list containing functions to 
## set & get the value of the matrix
## set & get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize inverse of x matrix to null
  invx <- NULL
  
  ## set the value of matrix
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  
  ## get the value of matrix
  get <- function() x
  
  ## set the value of inverse
  set_inv <- function(inverse) invx <<- inverse 
  
  ## get the value of inverse
  get_inv <- function() invx
  
  ## returns list of above defined functions
  return(list(set = set, get = get, set_inv = set_inv, get_inv = get_inv))
}


## This function calculates inverse of the special "matrix" created
## by the above function. It first checks if inverse has already been calculated
## If it has then it gets the inverse from the cache thereby reducing computation
## If not then calculates the inverse using set_inv function and returns it

cacheSolve <- function(x, ...) {
    
  ## get cached inverse
  invx <- x$get_inv()
  
  ## check if cached inverse is not null i.e. it actually exists
  ## if it does then return cached inverse
  if(!is.null(invx)) {
    message("getting cached inverse")
    return(invx)
  }
  
  ## if not then get matrix
  data <- x$get()
  
  ## calculate inverse
  invx <- solve(data, ...)
  
  ## cache inverse
  x$set_inv(invx)
  
  ## return result
  return(invx)
}
