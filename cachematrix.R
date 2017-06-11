## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##The idea is to save the inverse of the matrix to cache, rather than repeatedly compute it over and over
##so this function stores the matrix and caches its inverse


##Creation of the matrix object that can cache its inverse.

makeCacheMatrix <- function(x= matrix()){
  
  inv <- NULL
  set <- function(y){
    x<<- y
    
    inv  <- NULL 
  }
  get<- function () x
  
  setinvs <- function(inverse) inv <<- inverse
  
  getinvs <- function()inv
  
  list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
  
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinvs()
  if(!is.null(inv)){
    
    message("Getting Data")
    return(inv)
    
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinvs(inv)
  inv
        
}
