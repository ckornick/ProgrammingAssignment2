## ProgrammingAssignment2
## 9.24.2020
## This file contains function definitions for makeCacheMatrix and cacheSolve 

## The function makeCacheMatrix makes an R object that stores a matrix and its
## inverse. The contents can only be accessed with getter and setter functions

## The function cacheSolve finds the inverse of a matrix by taking an object
## of type makeCacheMatrix, retrieving its contents, and using the solve() 
## function on the stored matrix. The result is then cached. 

##-----------------------------------------------------------------------------
## Function name: makeCacheMatrix
## Purpose: creates an object of type makeCacheMatrix to store a matrix and 
## its inverse. Contains getter and setter functions. 
## Arguments: a matrix 
## Returns: a list of function names so they can be called in cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  
    #initialize the inverse matrix
    inverse <- NULL
    
    #caches a new matrix in x (from y) and sets inverse to NULL
    set <- function(y) {
      x <<- y
      inverse <<- NULL
    }
    
    #returns the cached matrix 
    get <- function() x
    
    #sets the variable inverse to a new value 
    setinverse <- function(calculated_inverse) inverse <<- calculated_inverse
    
    #returns the cached inverse
    getinverse <- function() inverse
    
    #returns a list that contains the names of the four functions defined here
    #this list allows the contents of the cached matrix to be used in cacheSolve
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


##-----------------------------------------------------------------------------
## Function name: cacheSolve
## Purpose: finds the inverse of a cached matrix (then caches the inverse) 
## Arguments: an object of type makeCacheMatrix that contains a matrix, its
## inverse, and four getter/setter functions 
## Returns: the inverse of the matrix 

cacheSolve <- function(x, ...) {

    #gets the inverse of the cached matrix 
    inverse <- x$getinverse()
    
    #if the inverse isn't NULL/has already been cached, the inverse is returned
    if(!is.null(inverse)) {
      
      message("getting cached data")
      return(inverse)
      
    }
    
   #if the inverse is NULL/hasn't been cached, the matrix is retrieved 
   matrix_data <- x$get()
   
   #the solve() function is used to get the inverse of the matrix 
   inverse <- solve(matrix_data)
   
   #the calculated inverse is then cached
   x$setinverse(inverse)
   
   #returns a matrix that's the inverse of x 
   inverse 
}
