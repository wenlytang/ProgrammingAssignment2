## Below two functions help cache the calculated inverse of a matrix. 
## The functions look up the computed inverse in the cache, instead of computing it first. 
## And it is able to compute if there's nothing stored in the cache.

## makeCacheMatrix creates a list containing a function to set and get the value of the matrix, 
## and set and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse_matrix <- function(inverse_matrix) m <<- inverse_matrix
        getinverse_matrix <- function() m
        list(set=set, get=get, setinverse_matrix=setinverse_matrix, getinverse_matrix=getinverse_matrix)
}


## The function will first look up the inverse of matrix that was already computed.
## If so, it retrieves the value in the cache, otherwise it computes the inverse and sets it in the cache via setinverse_matrix funcion.

cacheSolve <- function(x, ...) {
        m <- x$getinverse_matrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinverse_matrix(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
