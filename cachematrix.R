## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a list of functions ( a matrix) used by cacheSolve function
## The list of functions will be able to
##    Set the value of the matrix
##    Get the value of the matrix
##    Set the inverse of the matrix
##    Get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## Init m as NULL
  m <- NULL
  
  ##Set or Create the value of the matrix in the working environment
  set <- function(y) {
    x <<- y
    m <<- NULL
  }

  ##Get the value of the matrix
  get <- function() x
  
  #Invert the matrix and store the result in cache
  setMatrix <- function(inverse) m <<- inverse
  
  #Get the inverted matrix from cache
  getInverse <- function() m
  
  #Return the created functions to the working environment
  list(set = set, get = get, setMatrix = setMatrix, getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve calculates the inverse of the matrix created using function makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## get the inverse of the matrix stored getInverse function in makeCache
  m <- x$getInverse()
  
  ## Return a matrix that is the inverse of 'x' if it exists
    if(!is.null(m)) {
    message("Getting cached data")
    return(m)
  }
  
  ## Else create the matrix using get function created in makeCache
  vdata <- x$get()
  
  ## Check if vData is a square matrix, 
  ## else send message that matrix not invertible
  tryCatch ({ 
    
    # return inverse of vData matrix using solve function
    m <- solve(vdata, ...) 
    },
    
    ## If vdata has warning, print warning
    warning = function(war) {
      message("Warning:", war)
      return(NA)
    },
    
    ## If vdata isn't a square matrix, then print error
    error = function(err) {
      message("Error:", err)
      return(NA)
    },
    
    ## return the inverted matrix using setMatrix function in makeCache
    
    finally = {
      x$setMatrix(m)
    }
    )
  
  return (m)
  
}
