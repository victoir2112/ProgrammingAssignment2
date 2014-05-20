## Create a matrix, and ensure that the calculation of the inverse
## is only undertaken if the matrix has changed.

## The first function creates a list containing a function to
##      set the value of the matrix
##      get the value of the matrix
##      set the value of the inverse
##      get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialise the value that will hold the inverse
        i <- NULL
        ## a function that sets the matrix.  It creates it in the
        ## parent environment, and resets the inverse to NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ## a function to return the matrix
        get <- function() x
        ## a function to set the cached inverse to the specified value
        setinv <- function(inverse) i <<- inverse
        ## a function to get the inverse from the cache
        getinv <- function() i
        ## name the functions so that they can be called using $
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## The second function gets the current value of the inverse from the cache.
## If there is a cached value, it means that the matrix has not changed since
## it was calculated, so return it.  If it is NULL, then calculate the new
## inverse.  Finally, place the value back in the cache. 

cacheSolve <- function(x, ...) {
        ## Get the cached inverse into i
        i <- x$getinv()
        ## If i is not NULL, then print the message and return the value, exiting
        ## the function
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## If the code gets to this point, then i is NULL, so needs to be
        ## calculated
        ## Put the matrix into 'data'
        data <- x$get()
        ## Now put 'data' into the solve function to calculate the inverse, and
        ## place into 'i'
        i <- solve(data, ...)
        ## Set the value of 'i'
        x$setinv(i)
        i
}
