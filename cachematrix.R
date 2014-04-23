## Given the delay on running the solve function to find the inverse of a matrix,
## these functions will cache a given matrix and its inverse; it will do so 
## by creating a list with cached values, so when the inverse is needed, it can 
## just be found and called, rather than calculated over and over again.


## makeCacheMatrix creates a list and caches a matrix in it in the field set, 
## and creates the field for its inverse (inv).

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


## cacheSolve, when called for first time with a given matrix, calculates the 
## inverse of the matrix and stores (caches) it in the list created by the 
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
