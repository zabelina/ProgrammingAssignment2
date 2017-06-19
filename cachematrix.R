## Datafile cachematrix.R contains two functions: makeCacheMatrix.R and 
## cacheSolve.R. These functions can be used to cache the inverse of a matrix.

## makeCacheMatrix.R
## Function makeCacheMatrix.R takes matrix x as an input and creates a special
## object that can cache the inverse of the matrix x.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve.R
## Function cacheSolve.R takes the special object created by makeCacheMatrix.R 
## as an input, checks if the inverse of thr matrix has been already computed
## and has been cached. If it was computed, then the function returns the cached value,
## otherwise it computes the inverse and stores it to the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
