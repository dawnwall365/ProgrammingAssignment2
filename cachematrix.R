## Put comments here that give an overall description of what your functions do
#  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#  above. If the inverse has already been calculated (and the matrix has not changed), then the
#  cachesolve should retrieve the inverse from the cache.

## Write a short comment describing this function
# makeCacheMatrix() creates and initualizes a "class object" "cachematrix" which contains
# the original matrix, a cache for its inverse matrix, and a set of functions to manipulate
# the original and inverse matrices.
# makeVector creates a special "matrix", which is really a list containing a function to
# 1.set the value of the matrix
# 2.get the value of the matrix
# 3.set the value of the inverse
# 4.get the value of the inverse
#makeCacheMatrix creates and initualizes a "class object" "cachematrix". 
# A cachematrix object has:
# Functions set and get = Assign and get the (source) matrix data.
# Variable matrix.cache = Matrix cache for holding an already calculated (inverse) matrix.
# Functions setinverse and getinverse = Assign and get the (inverse) matrix stored in matrix.cache.
# Arguments: matrix

makeCacheMatrix <- function(x = matrix()) {
        matrix.cache <-null
        # Set original matrix data and invalidate cached inverse
        set <- function(y){
                x<<-y
                matrix.cache << -null
        }
        # Get original matrix data
        get <- function()x
         # Store inverse matrix data in cache
        setinverse <- function(inverse) matrix.cache <<- inverse
         # Get inverse matrix data from cache
        getinverse <- function()matrix.cache
         # Return the cachematrix object
        list(set = set , get = get ,setinverse = setinverse , getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. 
# However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from 
# the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value 
# of the inverse in the cache via the setinverse function.
# Description: cacheSolve returns the inverse of the matrix of "class" cachematrix.
# Arguments: x = cachematrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Retrieve  a possibly previously calculated matrix inverse from the cache
        inverse.matrix <- x$getinverse 
        # If NULL, cache was empty and inverse must be calculated first.
        # Otherwise, return the cached inverse matrix.
        if (!is.null(inverse.matrix)){
                message("getting cached data")
                #Return the inverse matrix (from cache)
                return(inverse.matrix)      
        } else {
                 message("calculating inverse and storing it in cache")
                 # Get original matrix values
                 data <- x$get()
                 # Solve to obtain inverse matrix
                 inverse.matrix <- solve(data, ...)
                  # Store inverse matrix into object's cache
                x$setinverse(inverse.matrix)
                # Return the inverse matrix (calculated)
                inverse.matrix
                }
}
