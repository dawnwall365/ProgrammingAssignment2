## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been calculated (and the matrix has not changed), then the
## cachesolve should retrieve the inverse from the cache.

## The first function, makeVector creates a special "matrix", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <-null
        set <- function(y){
                x<<-y
                m<<-null
        }
        get <- function()x
        setinverse <- function(inverse) m<<-inverse
        getinverse <- function()m
        list(set = set , get = get ,setinverse = setinverse , getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse 
        if (!is.null(m)){
                message("getting cached data")
                return(m)      ## Return a matrix that is the inverse of 'x'
        }
        data <- x$get()
        m <- solve(data, ...)
        x <- setinverse(m)      
}
