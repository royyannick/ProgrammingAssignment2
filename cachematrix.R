## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix return an object (list) offering 4 methods.
## 1) set : allow to modify directly the data, in x.
## 2) get : allow to access directly the data, in x.
## 3) setInverse : store the inverse received in argument, in i.
## 4) getInverse : retreive the inverse stored in i.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  ## returns the list containing the 4 methods.
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve will either solve the inverse matrix and
## cache it for later use, or return the cached one if
## it's been calculated already.
## The Inverse of the Matrix is obtained with Solve.
## *** This function does not 'validate' the matrix ***
## *** (e.g. square ? inversible ? ...) ****
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m) ## A cached one exist. No need to compute !
  }
  ## No cached one. Let's compute and cache !
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m ## Return a matrix that is the inverse of 'x'
}
