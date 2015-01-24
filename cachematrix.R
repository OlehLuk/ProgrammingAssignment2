## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL  
    
    set <- function (y){ 
        x <<- y
        inv <<- NULL       
    }
    
    get <- function() x  
    
    getInv <- function() inv
    
    setInv <- function(inver) {
        inv <<- inver             
    }
    
    list(set=set, get=get, getInv=getInv, setInv=setInv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    
    inv <-x$getInv()    
    
    if(!is.na(inv)){
        message("getting cached data")   
        return(inv)
    }
    
    data <- x$get()             
    inv <- solve(data)
    x$setInv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv  
}
