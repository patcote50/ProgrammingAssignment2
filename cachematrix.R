## The overall function takes a matrix and returns it's inverse using the Solve() function.
## The returning value is cached for faster future use.

## makeCacheMatrix creates an object to be used by cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL                   
        set<-function(y){                
                x<<-y                
                m<<-NULL                
        }        
        get<-function()x      
        setsolve<-function(solve)m<<-solve     
        getsolve<-function()m  
        list(set=set,get=get,             
             setsolve=setsolve,             
             getsolve=getsolve) 
}


## cacheSolve takes the element created by makeCacheMatrix and returns it's inverse. It takes
## the object in the if it's already there, if not it runs all functions.

cacheSolve <- function(x, ...) { 
        m<-x$getsolve()        
        if(!is.null(m)){        
                message("getting cached data") 
                return(m)         
        }
        data<-x$get()           
        m<-solve(data, ...)     
        x$setsolve(m)           
        m                      
}
