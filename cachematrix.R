## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Function "makeCacheMatrix" creates a "matrix" object that is able to cache its inverse. 
## makeCacheMatrix contains 4 functions: set, get, setinverse, getinverse

## get --> a function that returns the matrix x stored in the main function
## set --> a function that changes the matrix stored in the main function

## setmean and getmean are functions similar to set and get.
## They don't calculate the matrix, they store the value of the input in a variable m
## into the main function makeCacheMatrix(setinverse) and return it (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
