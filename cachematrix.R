## This file has two functions, one which caches Matrix Inverse Operation
## and another function which retrieves cached value of inversed matrix

## makeCacheMatrix function creates a special object which has a list of
# functions which does the following operations
# set the value of a matrix
# get the value of a matrix
# set the value of the inversed matrix
# get the value of the invsrsed matrix
# every time there is a new invertible square matrix to be inversed, 
# call makeCacheMatrix function by passing the new matrix. 
makeCacheMatrix <- function(x = matrix()) {
  # every time makeCacheMatrix is called the the special object
  # is set to NULL which means the inverse have to be calculated again
  invVal <- NULL
  
  # assigns the new square matrix to x and returns it
  set <- function(y) {
    x <<- y
    invVal <<- NULL
  }
  # returns the square matrix
  get <- function() x
  
  # assigns the newly calculated inversed matrix
  setInv <- function(invMat) invVal <<- invMat
  
  # returns the cached inversed matrix
  getInv <- function() invVal
  
  # list of functions
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve function calls the functions listed using makeCacheMatrix fn
# and retrieve cached inversed matrix
# if it doesn't find the inversed matrix in cache, the inverse is calculated
# using solve command the inversed matrix is cached for future retrieval

cacheSolve <- function(x, ...) {
  # try retrieving the cached inversed matrix
  m <- x$getInv()
  
  # if the cache has the inversed matrix, retrieve it and return the same
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # in case the cache doesn't have the inversed matrix, then the following 
  # steps happen
  
  # retrive the square matrix  
  data <- x$get()
  # calculate the inverse
  m <- solve(data, ...)
  # save the inversed matrix in cache
  x$setInv(m)
  m
}
