## Week 3 Assignment for Coursera R Programming class
## Programming Assignment 2: Lexical Scoping

## MakeCacheMatrix performs four functions:

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of inverse of the matrix
## 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             ## Set the inverse to NULL
    set <- function(y){
        x<<-y               ## Assign the y matrix to the x matrix
        inv <<- NULL        ## reset/clear?<<- inv to NULL for a new matrix
}
                            ## Return the matrix x
    get <- function() x    
   
                            ## assign inverse to inv
    setinverse <- function (inverse) inv<<-inverse 
    
                            ## Return the matrix inv
    getinverse <- function() inv 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


# This function returns the inverse of the matrix. It will return a cached value if
# the inverse has already been computed. 
# If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
                                ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()   ## set the inv variable. This might be NULL
       
                                ## If inv already exists, and is not null, then grab it from the cache
         if (!is.null(inv)) {
            message("Retrieving cached values") 
            return (inv)        ## return the cache and end the function
        }
        data <- x$get()         ## Get the matrix and assign to data
        inv <- solve(data)      ## Calculate the inverse amd assign to inv
        x$setinverse(inv)       ## Set the Inverse into the inv variable
        inv                     ## Return the inverse
}
