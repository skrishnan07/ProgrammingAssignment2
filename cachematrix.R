## The two fiunctions below are solutions to programming assignment #2
## from Week 3 of the R Programming course offerd on Coursera 
## by Johns Hopkins university


## This function makes a special matrix that can cache its inverse
## Assumes that the given matrix is a square matrix and is invertible
## Modeled after the makeVector function included in the assignment intro

makeCacheMatrix <- function(x = matrix()) 
{
   invMat <- NULL
   set <- function(y)
   {
            x <<- y
            invMat <<- NULL
   }
   get <- function() x
   
   setInverse <- function(invM = matrix())
   {
           invMat <<- invM
   }
   
   getInverse <- function() 
   {
        invMat
   }
   list(set = set, 
        get = get,
        setInverse = setInverse, 
        getInverse = getInverse)
}


## This function returns the inverse of the matrix
## It first looks to see if the inverse of the matrix has been cached
## If so, it returns the cached version.
## Otherwise, it computes the inverses, caches it and then returns the inverse

cacheSolve <- function(x, ...) 
{
        invMat <- x$getInverse()
        if ( !is.null(invMat) )
        {
                message("getting cached data")
                return(invMat)
        }
        data <- x$get()
        invMat <- solve(data)
        x$setInverse(invMat)
        invMat
}
