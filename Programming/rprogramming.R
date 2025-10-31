
#check length of scalar vs vector
length(5)

length(c(5))

5 == c(5)

#illustration of data frame object
df <- data.frame(ids = 2:51, score = rnorm(50))
df$id
df$sco

df$score2 = rbinom(size=50,n=10, prob=rep(1/10,10))
df$sco

is.data.frame(df$score2)
#df is not a vector, it is a list
is.vector(df)
is.list(df)
is.vector(df$score2)
is.vector(df[,"score2"])


nrow(df)
df$score3 = 1:5 #1:5 is shorthand for c(1,2,3,4,5)
head(df,10)
df$score4 = 1:7

tb = dplyr::tibble(df)
tb$score
tb[1:5,'score']

#assign madedf internally to function
#assign madedf2 globally
makedf = function(){
  madedf = data.frame(one=c(1,2,3),two=c(4,5,6))
  madedf2 <<- data.frame(one=c("a","b","c"), two=c("d","e","f"))
}

# trying to print madedf throws error 
makedf()
madedf2
madedf

add_variable=function(d){
  d$new = 1
}

head(df)
add_variable(df)
#new variable does not appear
head(df)

add_variable2 = function(d){
  d$new = 1
  return(d)
}

#overwrite df object
df = add_variable2(df)
head(df)