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


## Here is a sample run of the function to show it works
## Initialize a 4x4 random matrix

# x <- makeCacheMatrix(matrix(rnorm(16), nrow=4, ncol=4))

# x$get()
# [,1]       [,2]       [,3]         [,4]
# [1,]  0.52993922  1.4499146  0.4004010  1.455949605
# [2,]  1.42466179 -0.8880387  1.6081338  0.004537156
# [3,] -0.85988947  1.7895647 -0.1548914 -0.438168382
# [4,]  0.05809249 -0.6003635  0.2875434  0.378736494


## Now compute the inverse

# cacheSolve(x)
# [,1]        [,2]       [,3]       [,4]
# [1,]  0.2763462  0.20793996 -0.7811058 -1.9685070
# [2,]  0.2202366  0.07282495  0.2160123 -0.5976031
# [3,] -0.1243312  0.47862741  0.8117097  1.4113079
# [4,]  0.4011206 -0.27983687 -0.1540370  0.9235027


## Call the function again and this time assign it to y

# y <- cacheSolve(x)
# getting cached data
# y
# [,1]        [,2]       [,3]       [,4]
# [1,]  0.2763462  0.20793996 -0.7811058 -1.9685070
# [2,]  0.2202366  0.07282495  0.2160123 -0.5976031
# [3,] -0.1243312  0.47862741  0.8117097  1.4113079
# [4,]  0.4011206 -0.27983687 -0.1540370  0.9235027


## Do a sanity check on the inverse computation
## The round is needed for clarity. 
## Otherwise the off-diagonal entries show up as very small non-zero numbers

# round(x$get() %*% y)
# [,1] [,2] [,3] [,4]
# [1,]    1    0    0    0
# [2,]    0    1    0    0
# [3,]    0    0    1    0
# [4,]    0    0    0    1

