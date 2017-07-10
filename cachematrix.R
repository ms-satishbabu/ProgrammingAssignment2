# function used for setting the properties
makeCacheMatrix <- function (x = matrix()){
  
  invMatrix <<- NULL
  setCurrent <- function(newMatrix){
    x <<- newMatrix
    invMatrix <<- NULL
  }
  getCurrent <- function() {return(x)}
  
  setInverse <- function(inverseValue){invMatrix <<- inverseValue}
  getInverse <- function(){invMatrix}
  
  list(set=setCurrent, get=getCurrent, setinverse=setInverse, getinverse=getInverse)
}


# function for caching

cacheSolve <- function(matr){
  
  invMatrix <- matr$getinverse()
  
  if(!is.null(invMatrix)) {return(invMatrix)}
  
  # inverting a matrix (assumed the matrix is a inversable matrix)
  invMatrix <- solve(matr$get())
  
  #setting the makeCacheMatrix property with inverse value
  matr$setinverse(invMatrix) 
  invMatrix
}
