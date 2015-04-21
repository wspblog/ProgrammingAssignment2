## Put comments here that give an overall description of what your
## functions do
## 2 functions defined as
# makeCacheMatrix: 
# Create a list containing get,set functions 
# for a matrix, and its inverse

## Write a short comment describing this function
# cacheSolve: 
# Return the cached inverse matrix if it exists, or else it will
# compute the inverse matrix

# The cached inverse matrix procedure is a 2 step process
# First, invoke makeCacheMatrix to create the cache list
# Then invoke solveCache to either return the cached inverse matrix, 
# or perform a fresh compute for the inverse matrix. 
# The output of this function is a matrix


## makeCacheMatrix creates a list containing the following functions
## 1. Set the value of the input matrix        
## 2. Get the value of the matrix
## 3. Set the value of the matrix result
## 4. Get the value of the matrix result

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize parent variable m to NULL
        m <- NULL

        ## Set the value of the matrix 
        ## Note that x and m set as parent environment variables
        set <- function (y) {
                x <<- y
                m <<- NULL
        }
        
        ## Get the value of the matrix
        get <- function() x
        
        ## Set the matrix result to the variable m
        setinv <-  function(solved) m <<- solved

        ## Get the value of the variable m
        getinv <- function() m
        
        ## The output of makeCacheMatrix function is a list of functions
        list(set = set, get =get,
             setinv = setinv,
             getinv = getinv)
        
}


## cacheSolve returns a matrix that is the inverse of 'x'
## Input to cacheSolve is the vector created using cacheMatrix
## i.e. q<-cacheMatrix(matrix)
##      r<-cacheSolve(q)

cacheSolve <- function(x, ...) {
# if cached Inverse Matrix is available, return the cached value in x$getinv()        
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
# Otherwise compute the inverse matrix and set its value into cache
# Return the inverse matrix result in the variable m
        message("Fresh compute")
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

# Illustrate with the following operations
# create cache matrix
q<-matrix(c(1,34,23,42,53,45,12,3,88), nrow = 3, ncol = 3)
a<-makeCacheMatrix(q)

#First run of cacheSolve performs fresh compute of inverse matrix
print(cacheSolve(a))

#Second run retrieves cache data
print(cacheSolve(a))

#Third run retrieves cache data
print(cacheSolve(a))

