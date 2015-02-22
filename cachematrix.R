## The purpose of these functions is to provide a function to compute the inverse 
## of a matrix (created with use of our makeCacheMatrix function) and then cache the result on the first computation to be able
## to provide it quickier on later calls (using the cacheSolve function to retrieve or compute the inverse)
## We make use of r lexical scoping to achieve persistence of the computation


## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions that are used to set / get the matrix we provide to the function 
## and get / set the inverse of this matrix

makeCacheMatrix <- function(x = matrix()) {

    minvert <- NULL
    set <- function(y) {
      x <<- y
      minvert <<- NULL
    }
    get <- function() x
    setinvert <- function(invert) minvert <<- invert
    getinvert <- function() minvert
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
  }
  


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  minvert <- x$getinvert()
  if(!is.null(minvert)) {
    message("getting cached data")
    return(minvert)
  }
  data <- x$get()
  minvert <- solve(data, ...)
  x$setinvert(minvert)
}
