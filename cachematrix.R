#Programming Assignment 2
#Submission by Samir Yuja

#creates a special a matrix object in it's
#envir that can cache inverse, also holds
#getter and function to inverse

makeCacheMatrix <- function(orig_matrix = matrix(), ...){
    inver_matrix <- NULL
    
    #FUNCTION DEFINITIONS
    #replaces orig_matrix with another one
    set <- function(another_matrix){
        orig_matrix <<- another_matrix
        inver_matrix <<- NULL
    }
    
    #returns original matrix
    get <- function() orig_matrix
    #sets argument as the inverse
    setinverse <- function(alleged_inverse){
        inver_matrix <<- alleged_inverse
    }
    #returns the inverted matrix(may be wrong)
    getinverse <- function() inver_matrix
    
    #stores the FUNCTIONS in a list
    list(set = set, get = get, setinverse =
    setinverse, getinverse = getinverse)
}

#function computes the inverse unless
#the inverse has already been calculated
cacheSolve <- function(cache_List, ...){
    #cacheList is a list(returned by makeCacheMatrix)
    #first get inverse
    inver_matrix<- cache_List$getinverse()
    #check if null
    if(!is.null(inver_matrix)){
        message("getting cached data")
        return(inver_matrix)
    }
    #if it is null, calculate it
    orig_matrix <- cache_List$get()  #get original
    inver_matrix <- solve(orig_matrix)#calculate inverse
    cache_List$setinverse(inver_matrix)#cache it
    inver_matrix
}



