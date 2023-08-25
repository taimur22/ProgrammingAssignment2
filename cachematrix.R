## Put comments here that give an overall description of what your
## functions do
## What the two functions do, it calculates and stores the inverse of a matrix
## then uses that stored data for an output rather than calculate the inverse
## again. 
## This stops repeated computation of an inverse of the matrix.

## Write a short comment describing this function
## the function of makeCacheMatrix is that it creates an object 
## that stores the matrix and its inverse
## It does this by defining variables which are to be used later (such as 's')
## in the function cacheSolve
## It also works to cache the matrix by defining the "getter and setter"
## By adding the list it allows access to the functions using their names

makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}

## Write a short comment describing this function
## the cacheSolve function retrieves the inverse matrix from makeCacheMatrix
## which has been stored as 's'.
## It also calculates the inverse of the matrix 
## when there is no data already stored. 

cacheSolve <- function(x, ...) {
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s
}
