## The two functions demonstrate how Lexical Scoping works in R by realizing how objects like x and mat can be 
## accessed when they are defined in the original function's environment. This is made possible by using get() 
## and set functions


## Creates an object that stores a matrix and it's Inverse

makeCacheMatrix <- function(x = matrix()) {
        mat = NULL                                   ##The object mat is initialized to NULL
        set = function(y)                            ##sets the matrix
        {
                x <<-y                               ##assigns argument y to object x in the parent environment
                mat <<-NULL                          ##clears the value of mat cached
        }
        get <-function() x                           ##gets the value of x from the parent environment
        setinverse = function(solve)mat <<- solve    ##sets the inverse matrix
        getinverse = function()mat                   ##gets the inverse matrix
        list(
                set = set,
                get = get,
                setinverse = setinverse,             ##returns a list to the parent environment
                getinverse = getinverse              ##in which each element is assigned a function
        )                         
}


## Retrieves the cached Inverse Matrix if it is available in object x's environment

cacheSolve <- function(x, ...) 
{
        matinv = x$getinverse()                   ##attempts to retrieve inverse from object x
        if (!is.null(matinv))                     ##checks if the object is not NULL indicating that the cached inverse is found
        {
                message("getting cached data")
                return(matinv)                    ##returns the Inverse Matrix stored in the cache
        }
        data = x$get()                            ##Indicates that the cache is not found and gets input object x
        matinv = solve(data, ...)                 ##calculates the Inverse
        x$setinverse(matinv)                      ##sets the  Inverse Matrix
        matinv                                    ##prints the Inverse Matrix by returning the Inverse to the parent environment
}
