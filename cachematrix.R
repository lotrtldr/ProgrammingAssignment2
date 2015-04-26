## The following functions utilize the lexical scoping of R language to cache potentially expensive matrix inversion operations.


#This function takes a matrix and makes it into a "cache" matrix. It has getters and setters to access the matrix object and its "inverse" defined as an attribute of a makeCacheMatrix object. The function returns a list that allows access to these functions
makeCacheMatrix <- function(x=matrix()) {
    i <- NULL #initialize variable as NULL
    set <- function(y) {
        x <<- y #set input object y as argument x in makeCacheMatrix env
        i <<- NULL #ensure that set operation always resets cached i value to null in makeCacheMatrix env
    }
    get <- function() x #give the current object stored in the makeCacheMatrix env
    setInverse <- function(inverse) i <<- inverse #allow variable i to hold value inverse in makeCacheMatrix env
    getInverse <- function() i #return current i value in makeCacheMatrix env
    list(get = get,getInverse = getInverse,set = set,setInverse = setInverse) #finally return a list that allows access to the getter and setter functions
       
}

#This function wraps the solve function (which operates on a matrix object to give our desired inverse matrix value) so that the operation can be performed on a makeCacheMatrix object. It first checks if the value has already been calculated and returns the value if the cache exists (or calculates the inverse, stores it in the makeCacheMatrix object and returns that value)
cacheSolve <- function(x, ...) {
    i <- x$getInverse() #store value returned from makeCacheMatrix env (returned as getInverse element in return list)
    if(!is.null(i)) {
        message("Getting cached data")
        return(i)
    } else {
        data <- solve(x$get())
        i <- x$setInverse(data)
    } #check if i has a value and set or cache value accordingly
    i
}
   
    
    
