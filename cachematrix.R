## makeCacheMatrix is a function, that takes a matrix as input for which inverse has to be found 
## and returns a list. cacheSolve is a function that calculates the inverse of the matrix that was 
## input to makeCacheMatrix if its not cached or returns the cached inverse matrix if there is one.


## makeCacheMatrix takes the matrix for inverse calculation as input and caches both the matrix and its inverse. 
## It creates and outputs list consisting of four functions - set, get, setinverse, getinverse
## which gives the ability to set and/or interact with the matrix and it's inverse.


makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y = matrix()) {                ## set function inputs a matrix and assigns to x
                x <<- y
                m <<- NULL
        }
        get <- function() x                            ## get function does not take any inputs returns x
        setinverse <- function(Inverse) m <<- Inverse  ## setinverse takes inverse as input and returns m
        getinverse <- function() m                     ## getinverse takes no input but returns m
        list(set = set, get = get,                     ## makeCacheMatrix returns a list
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve is a function that computes and/or retrieves the inverse matrix.
## If the inverse has already been computed and cached, it gets the cached inverse matrix,
## otherwise calculates the inverse and caches it for subsequent use.

cacheSolve <- function(x) {
               
       m <- x$getinverse()                                            
        if(!is.null(m)) {
                message("getting cached data")         ## returning the cached inverse matrix
                return(m)
        }
        data <- x$get()                                ## assigning the input matrix in get() to variable data
        m <- solve(data)                               ## calculate the inverse of the matrix            
        x$setinverse(m)                                ## storing the calculated inverse
        m                                              ## returns the inverse
        
        
}
