## Assigment2 for Coursera R_Programming
## Author Andrii Krasnyi  date:20.06.2015
## 
## The first function, makeVector creates a special "vector", 
## which is really a list containing a function to 
##   1.set the value of the vector
##   2.get the value of the vector
##   3.set the value of the inverse matrix.
##   4.get the value of the inverse matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  s <- NULL
  set <- function(y) {
## store matrix and make S variable NULL
    x <<- y
    s <<- NULL
  }
  ## get matrix; set inverse of matrix; get value of inverse matrix
  get <- function() x
  setsolve <- function(solve) s <<- solve
  getsolve <- function() s
  ## assign list
  list(set = set
       , get = get
       , setsolve = setsolve
       , getsolve = getsolve
  )
}

## CAcheSolve function check if invertion of the matrix is stored 
## in the memory ard return value from the memory, othervise calculates 
## invertion of the matrix by using solve() function 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  s <- x$getsolve()
  ## Check if inverse matrix is stored in the memory
    if(!is.null(s)) {
      message("getting cached data")
      return(s)
    }
    ## calculates  value of inverce matrix and store it using x$setsolve() 
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
  
}
