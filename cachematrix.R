## A set of two functions which together reads a matrix, calculates its inverse and 
## stores it, such a way that unless its value does not change, it does not recaltulate 
## the entire operation, and instead just recalls the previously calculated value.  


## makeCacheMatrix is a function which a) sets the value of a matrix, b) gets the value of the matrix, 
## c) sets its inverse and lastly d) gets its inverse, and returns it in the form of a list.  

makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
		set <- function(y) {
        x <<- y
        m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## cacheSolve is the inverse of the given matrix and and return it value. But before calculating 
## it checks if the matrix inverse has been stored in the cache, and if so it recalls the stored value
## and skips the computation. 

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } 
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m               
}
