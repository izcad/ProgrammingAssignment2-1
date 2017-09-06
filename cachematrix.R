#Caching the inverse of a Matrix
#Caching a previous generated data is useful because it improve
#the efficient by reducing the processing time

#This function creates a matrix that can cache its inverse
makeCacheMatrix<-function(x=matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}

#This function calculte the inverse of the matrix created by the makeCacheMatrix function above
#if the inverse has been previously calculated, it won't doing again, saving processing time
cachesolve<-function(x,...) {
  inv<-x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matx<-x$get()
  inv<-solve(matx,...)
  x$setinverse(inv)
  inv
}