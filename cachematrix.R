## the list of functions created by makeCacheMatrix are:
## set puts the input matrix into the global envt
## get provides a way of retrieving the matrix on demand
## setinv computes the inverse of the input matrix and stores it in the global envt
## getinv retrieves the calculated inverse
## 

## makeCacheMatric creates a closure for a list of functions for retrieving or computing 
## the inverse of a square matric
## The function CacheMatrix calls the function created by makeCacheMatrix


makeCacheMatrix <- function(x = matrix()) {
 p<- NULL
 set <- function(y) {
    x <<- y
    p <<- NULL
 }
 get <- function() x
 setinv <- function(invx) p <<-invx
 getinv <- function() p
 list(set=set, get=get, setinv= setinv, getinv = getinv)
 }



## cacheSolve, using the "getinv" function attempts to find a previously cached value for "p",  
## which is the inverse of the matrix, in the local or global environment. If it is found, 
## then it prints a message to the console and returns "m" to the local environment.
## If not found, then it call the other functions created by makeCacheMatrix which do the following:
##  1) gets the input matrix
##  2) calcs the inverse and stores it in "p" locally
##  3) stores the inverse in a "p" in the global envt

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   p<- x$getinv()
   print(!is.null(p))
   if(!is.null(p)) {
     print("getting cached inverse")
     return(p)
   }
   data <- x$get()
   print(x$get())
   p <- solve(data,...)
   x$setinv(p)
   p
}
