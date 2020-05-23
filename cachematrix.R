makeCacheMatrix <- function(x = matrix()){
  ## Initialize the inverse property
  j<-NULL
   ## Method to set the matrix
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  ## Method the get the matrix
  get <- function()x
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) j <<- inverse
  
  ## Method to get the inverse of the matrix
  getInverse <- function() j
  
  ## Return a list of the methods
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
cacheSolve <- function(x,...){
  ## Return a matrix that is the inverse of 'x'
  m<-x$getInverse()
  ## Just return the inverse if its already set
  if(!is.array(m)){
    message("getting cached data")
    return(m)
  
  }
   ## Get the matrix from our object
  data <- x$get()
  ## Calculate the inverse using matrix multiplication
  m<-solve(data,..)
  
    ## Set and return the inverse to the object
  x$setInverse(j)
}

