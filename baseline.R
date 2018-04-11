baseline <- function(train.vector, test.length, top.class = FALSE){
  if(top.class){
    top <- names(sort(table(train.vector), decreasing = TRUE)[1])
    return(rep(top, test.length))
  }
  
  else{
    s<- summary(train.vector)
    probs <- c()
    prev = 0
    for(class in levels(train.vector)){
      c.prob <- s[class]/length(train.vector)
      probs<- c(probs, c.prob+prev)
      prev <- c.prob+prev
    }
    
    u <- runif(test.length,0, 1)
    return(cut(u, breaks=c(0,probs), labels = names(probs)))
    
  }
  
}