## R Programming MOOC week 3 Peer-graded Assignment: Programming Assignment 2: Lexical Scoping
## Autor: Antonio Guimarey
## Date: january 2019

makeCacheMatrix <- function(matrix = matrix()) {
  
  ## creates a special "vector", which is a list containing four functions:
  
  ## set the value of the matrix
  ## get the value of the matrix
  ## set the value of its inverse
  ## get the value of its inverse
  
  InverseMatrix <- NULL
  
  SetMatrix <- function(y) {
    matrix <<- y
    InverseMatrix <<- NULL
  }
  
  GetMatrix <- function() matrix
  
  SetInverseMatrix <- function(matrix) InverseMatrix <<- matrix
  
  GetInverseMatrix <- function() InverseMatrix
  
  list(SetMatrix = SetMatrix, GetMatrix = GetMatrix, 
       SetInverseMatrix = SetInverseMatrix, GetInverseMatrix = GetInverseMatrix)
  
}

cacheSolve <- function(matrix, ...) {
  
  ## computes the inverse of the matrix after checking if the value was calculated and cached befonematrix
  
  inverse <- matrix$GetInverseMatrix()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  InverseMatrix <- solve(matrix$GetMatrix()) 
  matrix$SetInverseMatrix(InverseMatrix)
  InverseMatrix
}
