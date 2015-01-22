## Caching the Inverse of a Matrix. R-Programming Assignment 2
#   Jordan T Wilkerson,Jan 2015
#
## The following contains two fucntions makeCacheMatrix() and cacheSolve()
#   makeCacheMatrix() creates a special "matrix" object that can cache its inverse.
#   cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#
#  If the inverse has already been calculated (and the matrix has not changed), 
#   then the cachesolve should retrieve the inverse from the cache.
#
## To test these fucntions use the following commands:
#   > x <- matrix(c(1,-0.25,-0.25,1), 2,2)  # defines a sample matrix 'x' (but any square, invertable matrix will work)
#   > mat <- makeCacheMatrix(x)             # generates a list of caching functions
#   > cacheSolve(mat)                       # first time this is called it solves for the inverse of the matrix in 'mat'
#   > cacheSolve(mat)                       # second time, it sees the inverse has already been cached and retreives it
#   > cacheSolve(mat) %*% x                 # The inverse multiplied by the matrix produces the Idenity matrix (just a check)

## makeCacheMatrix()
#   This will take a matrix 'x' as an input, then create a list of functions.
#     The list of functions allow for storign the matrix, computing and caching the inverse.
#   These functions are intended to be used with cacheSolve(), which will determine if the inverse needs to be computed.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## cacheSolve()
#   Once the matrix 'x' has been applied to the makeCacheMatrix() function, 
#     then cacheSolve() will return the cached inverted matrix solution, or, in the absence of a solution compute it.
#   This is intended to be used with makeCacheMatrix(), which will determine if the inverse needs to be computed.
cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse of 'x'
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}


