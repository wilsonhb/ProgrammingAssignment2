## Creates a special matrix that can cache its inverse
## First, it gets and then it sets the matrix


makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        # Setting the matrix
        set <- function(matrix) {
                m <<- matrix
                i <<- NULL
        }
        # Getting the matrix
        get <- function() m
        # Setting the inverse of the matrix
        setInverse <- function(solve) i <<- solve
        # Getting the inverse of the matrix
        getInverse <- function() i
        list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## It gets the matrix and calculates the inverse using
## marix multiplication

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Get the matrix from our object
        data <- x$get()
        #Calculate the inverse using matrix multipliction
        m <- solve(data) %*% data
        # Set the inverse to the object
        x$setInverse(m)
        # Return the matrix
        m
}
