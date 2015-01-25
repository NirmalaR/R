## Computing the inverse of a matrix, caching and retrieving the matrix 
    ## if it is already cached or creating the inverse if it does not exist

## Create a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) 
{
   c<-NULL
   set <- function (y)
   {
       x <<- y
       c <<- NULL
   }
   get <-function() x
   inverseMatrix <- function(solve) c<<-solve
   cacheMatrix <- function() c
   list(set=set, get=get,inverseMatrix=inverseMatrix,cacheMatrix=cacheMatrix)
}


## Creates inverse of a square matrix using MakeCacheMatrix or retrieve the inverse 
## from cache if it already exits

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        c <- x$inverseMatrix()
        if(!is.null(c))
        {
            message("getting cached data")
            return(c)
        }
        data <- x$get()
        c<-solve(data,..)
        x$cacheMatrix(c)
        c
}
    