## There are two functions in this file. They try to cache and retrieve the
## inverse of a matrix to save time in computation. The function structure is based 
## on the given example and modified according to the different application purpose.


## The makeCacheMatrix function aims to create a special matrix object to be able to 
## cache the inverse of that matrix. It creates a list of functions to set and get the
## matrix and also set and get its inverse. The "<<-" operator makes it possible to
## use the function in different matrix (just like different environment) as it could 
## assign a value to an object that is not in the current environment.  

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
    inver <<- NULL
  }
  get <- function(){
    x
  } 
  setinver <- function(inverse){
    inver <<- inverse
  }
  getinver <- function() {
    inver
  }
  list(set = set, get = get,
       setinver = setinver,
       getinver = getinver)
}


## This cacheSolve function is to get the inverse of the matrix created by the first
## function. It will first try to find the inverse in the cache and return it if it
## is already calculated. If not, it will calculate the inverse with the solve function
## and store it in the cache. 

cacheSolve <- function(x, ...) {
  inver <- x$getinver()
  if(!is.null(inver)){
    message("getting stored data")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinver(inver)
  inver
}
