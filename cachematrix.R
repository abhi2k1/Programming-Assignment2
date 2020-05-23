makeCacheMatrix <- function(x = matrix()){
  j<-NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}

cacheSolve <- function(x,...){
  m<-x$getInverse()
  if(!is.array(m)){
    message("getting cached data")
    return(m)
  
  }
  data <- x$get()
  m<-solve(data,..)
  x$setInverse(j)
}

