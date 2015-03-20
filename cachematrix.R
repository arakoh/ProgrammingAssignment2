

# makeCacheMatrix is a function that returns a list of function
#Its puspose is to store a martix and a cached value of the inverse of the  
# matrix. Contains the following functions: 
# * setMatrix      set the value of a matrix 
# * getMatrix      get the value of a matrix 
# * cacheInverse   get the cahced value (inverse of the matrix) 
# * getInverse     get the cahced value (inverse of the matrix) 
# 


makeCacheMatrix <- function(x = matrix()) {

# holds the cached value or NULL if nothing is cached 
# initially nothing is cached so set it to NULL 
cache <- NULL

 # store a matrix 
  setMatrix <- function (newValue) {
          x <<- newValue
          # since the matrix is assigned a new value, flush the cache
          cache <<- NULL
  }
   # returns the stored matrix
  getMatrix <- function() { 
          x
  }
  # cache the given argument
  cacheInverse <- function(solve) {
          cache <<- solve
  }
 # get the cached value
  getInverse <- function() {
          cache
  }
  # return a list.
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with  
# makeCacheMatrix 

cacheSolve <- function(y, ...) {
         inverse <-y$getInverse()    # get the cached value
  if(!is.null(inverse)) {                       # if a cached value exists return it
          message("getting cached data")
          return(inverse)
  }
   # otherwise get the matrix, caclulate the inverse and store it in the cache 

  data <- y$getMatrix()
  inverse <- solve(data)
  y$cacheInverse(inverse)
  
  inverse                # return the inverse
}
