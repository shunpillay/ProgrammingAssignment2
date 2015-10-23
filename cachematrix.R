## The following two functions are used to create a special object that stores a numeric matrix and caches its inverse.

## Creates a special "matrix" object which is a list of functions to ..
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  theinverse <- NULL
  set <- function(y) {
    x <<- y
    theinverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inputinverse) theinverse <<- inputinverse
  getinverse <- function() theinverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" object created with makeCacheMatrix.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  theinverse <- x$getinverse()
  if(!is.null(theinverse)) {
    message("getting cached data")
    return(theinverse)
  }
  data <- x$get()
  theinverse <- solve(data, ...)
  x$setinverse(theinverse)
  theinverse
}
