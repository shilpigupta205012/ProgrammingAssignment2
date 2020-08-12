## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {#define the argument
  inv <- NULL                           #initialise inv as null,matrix which will return inverse
  set <- function(y) {                  #define the set of functions to assign new
    x <<- y                             #value of matrix in parent environment
    inv <<- NULL                        #if there is a new matrix, reset inv to null
  }
  get <- function(){
    x                                   #get func- returns the value ofmatrix argument
  }                      
  setInverse <- function(inverse) {
    inv <<- inverse                     #assign value of inverse in parent environment
  }
  getInverse <- function(){
    inv                                 #gets value of invere where called 
  } 
  list(set = set, get = get,               
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix, ...)
  x$setInverse(inv)
  inv
}        ## Return a matrix that is the inverse of 'x'

