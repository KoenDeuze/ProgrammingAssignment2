## Function "makeCacheMatrix" creates a "matrix" object that is able to cache its inverse. 
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse

## get --> a function that returns the matrix x stored in the main function
## set --> a function that changes the matrix stored in the main function
## setinverse and getinverse are functions similar to set and get.
## They don't calculate the matrix, they store the value of the input in a variable inv
## into the main function makeCacheMatrix(setinverse) and return it (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        ## @x: a square invertible matrix
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
                # use `<<-` to assign a value to an object in an environment 
                # different from the current environment. 

        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve 
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        
        ##  return: a list containing functions to 
        ##      1. set the matrix
        ##      2. get the matrix
        ##      3. set the inverse
        ##      4. get the inverse
        ##  this list is used as the input to cacheSolve()
        
}


## cachesolve computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## @x: output of makeCacheMatrix()
        ## return: inverse of the original matrix input to makeCacheMatrix()
        
        inv <- x$getinverse()
        
        # if the inverse has already been calculated
        if(!is.null(inv)) {
                
                # get it from the cache and skips the computation. 
                message("getting cached data")
                return(inv)
        }
        
        # else, calculate the inverse 
        data <- x$get()
        inv <- solve(data, ...)
        
        # set the value of the inverse in the cache via the setinv function.
        x$setinverse(inv)
        return(inv)
}
