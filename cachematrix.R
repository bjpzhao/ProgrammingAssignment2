## This R code includes a pair of functions that cache the inverse of a matrix


## makeCacheMatrix function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## Method to set the matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        ## Method to get the Matrix
        get <- function() x
        
        ## Method to set the inverse of the matrix
        setinverse <- function(inverse) i <<- inverse
        
        ## Method to get the inverse of the matrix
        getinverse <- function() i
        
        ## Return a list of the methods
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of "x"
        i <- x$getinverse()
        
        ## Just return the inverse if it is already set
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        ## Get the matrix from our object
        data <- x$get()
        
        ## Calculate the inverse
        i <- solve(data, ...)
        
        ## Set the inverse to the object
        x$setinverse(i)
        
        ## Return the matrix
        i
}
