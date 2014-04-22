## Given the delay on running the solve function to find the inverse of a matrix,
## these functions will cache a given matrix and its inverse by creating
## a list with cached values


## makeCacheMatrix creates and caches a matrix and spaces in the environment 
## to cache the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)  

}


## cacheSolve, when called for first time with a given matrix, finds the inverse
## of the matrix and stores (caches) it in the list created by running first the 
## makeCacheMatrix function. From there on, it finds the cached value and returns
## it without calculating it again.

cacheSolve <- function(x, ...) {
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
