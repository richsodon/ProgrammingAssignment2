## Put comments here that give an overall description of what your
## functions do

## MakeCacheMatrix will create a special "matrix" object that can cache its inverse


# Function to create the CacheMatrix list
makeCacheMatrix <- function(x = numeric()) {
 #convert X to a matrix
        as.matrix (x)
 #Initialize input to make sure it is Null
        m <- NULL
 #create the set function
        set <- function(y) {
#store x & M in the other environment
                x <<- y
                m <<- NULL
        }
#create the get function to retrieve the matrix
        get <- function() x
#setSolve - set the inverse function name
        setsolve <- function(solve) m <<- solve
#getsolve gets the inverse matrix
        getsolve <- function() m
#create the return list with the created functions needed to make the matrix
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# CacheSolve will check to see if the inverse has already been calculated and stored in cache
# If it has, it will get the cached matrix and return the cached matrix with a message saying it is using the cache version
# If it hasn't, it will calculate the inverse with solve function and store it in the cached environment space

cacheSolve <- function(x, ...) {
#get the inverse
        m <- x$getsolve()
 #check to see if the matrix was already inverted (not null values)
        if(!is.null(m)) {
 #return the cached value and don't recalculate
                message("getting cached data")
                return(m)
        }
#not cached yet so solve it
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
#return the solved version
        m
}
