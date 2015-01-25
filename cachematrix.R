## To find the inverse of matrix x
## 2 functions should be used concurrently, and makeCacheMatrix should be called perior to cacheSolve

## Creates R object CacheMatrix
##initialize a variable 'm'
##setinverse(): set inverse of matrix x into cache
##getinverse(): get the inverse of matrix x from cache

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
        	      x<<- y
        	      inv<<-NULL
        }
        get <- function() x
        setinverse<- function(solve) inv <<-solve
        getinverse<- function() inv
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Inversing of matrix x 
##check if matrix x is cached
##if found, then inverse the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinverse()
        if(!is.null(inv)){
        	      message("getting cached data")
        	      return(inv)
        }
        data<-x$get()
        inv<-solve(data, ...)
        x$setinverse(inv)
        inv
}
