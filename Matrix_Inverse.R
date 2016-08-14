makeCacheMatrix <- function(x = matrix()){
  
      inv<-NULL
      set<-function(y){
        x<<-y
        y<<-NULL
        
      }
      get<-function() x
      setinverse <- function(inverse) inv<<-inverse
      getinverse <- function() inv
      list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}
  

CacheSolve<-function(x,...){
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data,...)
  x$setinverse(inv)
  inv
  
}

##>x=rbind(c(1,-1/4),c(-1/4,1))
##> inv<-makeCacheMatrix(x)
##> inv$get()
##[,1]  [,2]
##[1,]  1.00 -0.25
##2,] -0.25  1.00
##> CacheSolve(inv)
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> CacheSolve(inv)
##getting cached data
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667
##> CacheSolve(inv)
##getting cached data
##[,1]      [,2]
##[1,] 1.0666667 0.2666667
##[2,] 0.2666667 1.0666667