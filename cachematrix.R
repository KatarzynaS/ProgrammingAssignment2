#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
# if nothing to be cached setting null
  odwrot<-NULL
#setting and returning a matrix
  set<-function(y){
  x<<-y
  odwrot<<-NULL
}
get<-function() x

# setting and returning inverse

setinv<-function(calc) odwrot<<- calc
getinv<-function() odwrot

# returning a function list
list(set=set, get=get,
   setinv=setinv,
   getinv=getinv)
}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

cacheSolve <- function(x, ...) {

 # getting cached value

    odwrot<-x$getinv()
# returning if not empty
    if(!is.null(odwrot)){
      message("getting cached data")
      return(odwrot)
    }
# calculating otherwise
    data <- x$get() 
    odwrot<-calc(data)
    x$setinv(odwrot)
#retturning result   
odwrot
}
