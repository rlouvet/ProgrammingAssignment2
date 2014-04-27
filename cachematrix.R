## Edited by [rlouvet]
## The following two functions "makeCacheMatrix" and "cacheSolve" help user to compute inverse of matrix
## in an effective way, meaning that they prevent the inverse of a given matrix to be computed twice
## A new class of matrix "CacheMatrix" is created, which embed an "updated inverse matrix" attribute named inv


## Function 1 : "makeCacheMatrix"
## This function implements the new matrix structure whose inv attribute can be manipulated
## using getters and setters

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL ## Here is the constructor of the "CacheMatrix" structure
              ## we begin by initializing the inv attribute to NULL
  set <- function(y) {
    x <<- y      ## inv is reinitialized when setting the matrix anew
    inv <<- NULL ## Nb : x and inv persistance in the function's parent environment
  }
  get <- function() x                             ## Here is the matrix getter
  setInverse <- function(Inverse) inv <<- Inverse ## Here is the inv attribute setter
  getInverse <- function() inv                    ## Here is the inv attribute 
  list(set = set, get = get,    ## The CacheMatrix structure returns a list of getter and setter functions
       setInverse = setInverse,
       getInverse = getInverse)

}


## Function 2 : "cacheSolve"
## This function is used to keep the CacheMatrix up to date without useless inverse computation

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the Inverse of 'x', where x structure is the new cacheMatrix structure
  inv <- x$getInverse()     ## The cached inverse is retrived
  if(!is.null(inv)) {       ## If this inverse matrix is not NULL then the inverse of x has already been computed
    message("getting cached data")  ## and "cacheSove" returns this value
    return(inv)
  }
  data <- x$get()           ## If not, then the inverse function is computed...
  inv <- solve(data, ...)
  x$setInverse(inv)         ## ...set in the inv attribute
  inv                       ## then returned by "cacheSove"
  
}
