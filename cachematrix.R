## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix will create the special R vector which will contain
## the functions to get, set matrix as well as inverse of matrix
## also it will contain two objects x which is original matrix as well as
## inv which is inverese of matrix
makeCacheMatrix <- function(x = matrix()) {

  inv<- NULL
  set <- function(y){
    x <<- y        ## set the value of x in parent environment
    inv <<- NULL   ## set the value of inv in parent environment
  }
  get <- function() x    ## returns value of x
  setInv <- function(inverse){
    inv <<- inverse   ## set the value of inv in parent environment
  }
  getInv <- function() inv   ## returns value of inv
  
  list( set = set, get = get, 
        setInv = setInv, getInv = getInv) ## Create the special vector
  
}


## Write a short comment describing this function
##CacheSolve will check if x contains a matrix inverse cached or not
##if not then it will calculate the inverse and save it in the x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if (!is.null(Inv)){        ## if inverse is present or not
    message("Getting Cached Data")
    return(Inv)             ## returns the cached inverse matrix
  }
  mat <- x$get()
  Inv <- solve(mat, ...)
  x$setInv(Inv)
  Inv                      ## returns the freshly calculated inverse matrix
    
}
