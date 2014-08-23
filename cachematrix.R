## These 2 functions are part of prgramming assignment 2 of the R programming class
## These are utility functions that will help increase speed of computation 
## while computing the Inverse of a matrix.

## makeCacheMatrix is the function used to create a special object storing a 
## numeric vector and cache's its mean

## This function need to be invoked ahead of the inverse calculation from cache
##Input to this function is a matrix for which and inverse can be calculated.

makeCacheMatrix <- function(x = matrix()) {
	
	temp<-NULL ## variable holding the value for the inverse across invocations
	
	set<-function(y){
		x<<-y
		temp<<-NULL
	}
	
	get<-function()x
	
	setinverse<-function(inverse) temp<<-inverse
	
	getinverse<-function()temp
	
	list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cacheSolve function uses the special object created by the makeCacheMatrix
## function and output the inverse of the matrix . The inverse is taken from 
## the cache if it exists or frshly computed.

## this function makes the assumption that the special object is already available
## and can be consumed for getting the inverse values from the cache if it already exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	inverse<-x$getinverse()
	if(!is.null(inverse))
	{
		message("inverse from cache");
		return(inverse)
	}
	
	data<-x$get()

	inverse<-solve(data)
	x$setinverse(inverse)
	inverse


}
