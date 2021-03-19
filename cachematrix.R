## The following functions are used to create a special matrix and cache's its inverse

## The function makeCacheMatrix gets an inversible matrix and returns a list containing
## functions to get and set the values of the matrix given and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function cacheSolve uses the special matrix created with the makeCacheMatrix 
## function to compute the inverse of the matrix if it has not been calculated yet 
## and get the cached data if it has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  y <- x$get()
  inverse <- solve(y)
  x$setinverse(inverse)
  return(inverse)
}
