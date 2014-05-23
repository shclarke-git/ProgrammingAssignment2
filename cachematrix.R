## The functions below create a special object of type, matrix, and caches its inverse.

## The makeCacheMatrix function creates a special "matrix" which is a list containing functions 
## to get and set the value of the matrix, and to get and set the value of the inverse.

makeCacheMatrix <- function(x = matrix()) 
{
    m<-NULL
    set<-function(y, ...)
    {
            x<<-y
            m<<-NULL
    }
    get<-function()x
    setinverse<-function(solve)m<<-solve
    getinverse<-function()m
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## The cacheSolve function calculates the inverse of the special "matrix" with the function above.
## It first checks to see if the inverse has already been calculated and if so, gets the inverse from cache.
## If so, it gets the inverse from cache using the getinverse function and skips the computation.
## If the inverse is not in cache, it calculates the inverse of the matrix and sets the value in cache using the setinverse function.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m))
        {
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        m
}
