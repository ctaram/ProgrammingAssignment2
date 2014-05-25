## This R program will make a cache inverse matrix
## 

## makeCacheMatrix accepts a matrix and cache its inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  matrix <- NULL    # initialize matrix to null
  
  # set matrix function assigns the matrix
  setMatrix <- function(y) {
    x <<- y 
    matrix <<- NULL
  }
  
  # get matrix function returns the same matrix
  getMatrix <- function() x
  
  # set inverse matrix accepts a matrix and sends back the inverse matrix
  setInverseMatrix <- function(inverse) matrix <<- inverse
  
  # get inverse matrix function returns the inverse matrix
  getInverseMatrix <- function() matrix
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## cacheSolve function checks if the inverse of a matrix is present in the cache
## if already present then it will return the inverse matrix or else it calculates it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  matrix <- x$getInverseMatrix()
  
  # if matrix already present in cache then return it.
  if(!is.null(matrix)) {
    message("getting cached inverse matrix")
    return(matrix)
  }
  
  # else create an inverse if the matrix using the solve function
  data <- x$getMatrix()
  
  # applying solve function to create an inverse matrix
  matrix <- solve(data)
  
  # setting the inverse matrix
  x$setInverseMatrix(matrix)
  matrix # returning the inverse matrix
}
