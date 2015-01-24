## This two functions are
## for caching the inverse of a matrix rather than computing it repeatedly 

## 'makeCacheMatrix'  creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## create variable inv, that indicates whether matrix
    ## was changed and store inverse matrix
    inv <- NULL  
    
    ## set the value of a matrix
    set <- function (y){ 
        x <<- y
        inv <<- NULL       
    }
    ## get the value of a matrix
    get <- function() x    
    ## get the cached inverse
    getInv <- function() inv    
    ## set the cached inverse
    setInv <- function(inver) {
        inv <<- inver             
    }
    
    list(set=set, get=get, getInv=getInv, setInv=setInv)
    
}


## 'cacheSolve' function computes the inverse of those special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ##get cached in inv inversed matrix
    inv <-x$getInv()  
    ## if the cache isn't empty return it
    if(!is.na(inv)){
        message("getting cached data")   
        return(inv)
    }
    ## if not, get the matrix, calculate the inverse and cache it
    data <- x$get()             
    inv <- solve(data)
    x$setInv(inv)
    ## Return a matrix that is the inverse of 'x'
    inv        
}