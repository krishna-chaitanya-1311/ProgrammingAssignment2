## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m = matrix()) {
  
  # creating and declaring an inverse matrix as i as an empty matrix
  
  i <- NULL
  
  # setting the matrix
  
  set <- function(mat) {
    m <<- mat
    i <<- NULL
  }
  
  # getting the matrix
  
  get <- function() m
  
  # setting the inverse of matrix
  
  setmatrix <- function(inverse) i <<- inverse 
  
  # getting the inverse of matrix 
  
  getmatrix <- function() i
  
  #returning the list of above all methods
  
  list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## Cachesolve computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {
  
  ## Return a matrix which is inverse of x
    
  m <- x$getmatrix()
  
  # If the matrix is already set, the below will return the cache data
  
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  
  # Getting the matrix from the above declared object
  
  data <- x$get()
  
  #solve function is used to get the inverse by multiplying the matrix
  
  m <- solve(data) %*% data
  
  #Setting the inverse to the object and returing the final matrix
  
  x$setmatrix(m)
  m
  }
