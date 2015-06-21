## Users input a matrix and these functions store that matrix and use that matrix
## to compute its inverse. cacheSolve checks to see if the inverse
## has been cached. If it hasn't, it calls inversecalc which makesk the calculate
## cacheSolve then calls setinverse from makeCacheMatrix to save that result
## in the cache

## makeCacheMatrix has multiple functions that allow the user to 
## 1. retrieve both a matrix (get) and its inverse (getinverse)
## 2. reset the matrix and its inverse to null (set)
## 3. calculate the inverse (inversecalc) (~NOT REQUIRED)
## 4. store the calculated inverse value in a cache (setinverse) 
## so that outside functions can retreive the value (eg cacheSolve)

# Unsure where to retreive required library (added here)
library(MASS) 

makeCacheMatrix <- function(x = matrix()) {
# invertible matrix must be a square matrix. eg c(1,2,3,4) splits into the square root of its length and mod(sqrt(length(array)))==0
# must input matrix in the form matrix(1:4,2,2), eg a <- makeCacheMatrix(matrix(1:4,2,2))
  #library(MASS) 
  inverse <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  inversecalc <- function() inverse <<- ginv(x)
  #setinverse <- function() inverse <<- ginv(x)
  setinverse <- function(invert) inverse <<- invert
  getinverse <- function() inverse
  list(set = set, get = get, inversecalc = inversecalc,
       setinverse = setinverse,
       getinverse = getinverse)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  matrix <- x$get()
  # here is the part that is important ~ solving for the inverse
  inverse <- ginv(matrix, ...)
  x$setinverse(inverse)
  inverse
  # solve(x) doesn't seem to work on matrixes greater than 2x2 so 
  # i'm using ginv instead which requires the library MASS
}
