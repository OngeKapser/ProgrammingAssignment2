## I have had to use the excellent reasource "Demystifying makeVector() 
## to make sense of this assignement 
## URL: (https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md)

## This functions creates a special matrix that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
        inv = NULL
        set <- function(y){
                x <<- y # set the value of the matrix
                inv <<- NULL # nulls the value of the inverse cache
        }
        get <- function() x # gets the matrix
        setinverse <- function(inverse)inv <<- inverse # sets the inverse
        getinverse <- function() inv # gets the inverse
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse) # creates list of above functions for later use
}


## This function returns the inverse of a matrix by calculation or 
## by pulling the cache from the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() # gets the value of the inverse from cache
        if(!is.null(inv)){
                message("Getting cached data") # if not NULL message is displayed and inv returned
                return(inv)
        }
        # if above is false the inverse is calculated
        data <- x$get() # gets matrix
        inv <- solve(data, ...) # calculates inverse
        x$setinverse(inv) # caches the inverse
        inv # returns inverse
}

## Use this Coursera resource to test the code: https://www.coursera.org/learn/r-programming/discussions/weeks/3/threads/ePlO1eMdEeahzg7_4P4Vvg
