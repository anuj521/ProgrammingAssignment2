## Put comments here that give an overall description of what your
## functions do

## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
        im <- NULL
        set <- function(y)
        {
                mt <<- y
                im <<- NULL
        }
        get <- function()
        { 
                mt
        }
        setim <- function(inverse) {
                im <<- inverse
        }
        getim <- function() 
        {
                im
        }
        list(set = set, get = get, setim = setim, getim = getim) 
}

## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getim()
        
        if(!is.null(inv))
        {
                message ("Getting cached inversed matrix.")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        
        x$setim(inv)
        
        return(inv)
}
