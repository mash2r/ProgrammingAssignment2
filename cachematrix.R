## This is porgramming assignment 2 by mash2r for "R Programming" class on Coursera. 
## The file contains two functions that allow to cache the inverse of a matrix. 

## Function makeCacheMatrix creates a special "matrix" object
## that can cache its inverse.If the "matrix" objexts gets set, 
## the inverse of that matrix becomes Null and new inverse 
## will be calculated using casheSolve function below. 

makeCacheMatrix <- function(x = matrix()) {
    ## set empty inverse on initialization
    inv <- NULL
    
    ## function to set matrix and reset inverse    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## function to get matrix
    get <- function() x
    
    ## functions to set and get inverse 
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    ## return list of all objects above 
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Function cacheSolve computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above if it has not been already done.
## If the inverse has been already cached, it returns that value
## without further computation

cacheSolve <- function(x, ...) {
    ## check if inverse already exists
    ## and return it if it does 
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if inv has not been cached, 
    ## get matrix, compute inverse  
    data <- x$get()
    inv <- solve(data, ...)
    
    ## save inverse back to cacheMatrix 
    x$setinv(inv)
    inv
}

## Sample test cases

## 1.  Caching inverse valuse of number 5 (= 1/5 = 0.2) 
## cacheSolve(makeCacheMatrix(5))
##
## Result: 
## [,1]
## [1,]  0.2

## 2. Caching inverse valuse of 2x2 matrix
##
## t<- makeCacheMatrix(matrix (1:4, nrow = 2, ncol = 2))
## cacheSolve(t)
##
## Result:
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
##
## Run again: 
## cacheSolve(t)
##
## Result: 
## getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
 
