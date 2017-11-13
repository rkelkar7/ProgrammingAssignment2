##This function Function makeCacheMatrix gets a matrix as an input, set the value of the matrix,
 -#get the value of the matrix, set the inverse Matrix and get the inverse Matrix. The matrix object
 -#can cache its own object. 



makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function cacheSolve takes the output of the previous matrix makeCacheMatrix as an 
 -# input and checks inverse matrix from makeCacheMatrix has any value in it or not.
 -# In case inverse matrix from makeCacheMatrix is empty, it gets the original matrix data from 
 -# and set the invertible  matrix by using the solve function.
 -# In case inverse matrix from makeCacheMatrix has some value in it, it returns a message  "Getting Cached Invertible Matrix" 
 -#and the cached object

  
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
