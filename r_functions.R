# yliu867

#*******************************************
#question 2:log_gamma_loop(n) log((n - 1)!)
#*******************************************
log_gamma_loop = function(n) {
  sum =0
  if (n==1){
    return (0)}
  if (n==2)
  {return (0)}
  for (n in 3:n){
    result =log(n-1)
    sum =sum+result
  }
  return (sum)
}

for (i in 1:5)
{ print (log_gamma_loop(i))
  print (lgamma(i))
}

#***********************************************
#question 3:log_gamma_recursive(n) log((n - 1)!)
#***********************************************
log_gamma_recursive = function(n) {
  if(n == 1 ) {
    return(0)
  } 
  else if(n==2)
  {return (0)}
  
  else { 
    return (log(n-1) + log_gamma_recursive(n-1))
  }
}

for (i in 1:5) {
  print (log_gamma_recursive(i))
  print (lgamma(i))
}

#***************************************************************
#question 4:sum_log_gamma_loop(n) and sum_log_gamma_recursive(n)
#***************************************************************

sum_log_gamma_loop = function(n) {
  sum =0
  for (i in 1:n)
  {    sum = sum +log_gamma_loop(i)
  }
  return (sum)
}

sum_log_gamma_recursive = function (n){
  sum =0
  for (i in 1:n)
  {
    sum = sum +log_gamma_recursive(i)
  }
  return(sum)
}

sum_normal_lgamma = function(n)
{ sum =0
for (i in 1:n)
{
  sum= sum +lgamma(i)
}
return (sum)
}

sum_normal_lgamma(700)
sum_log_gamma_loop(700)
sum_log_gamma_recursive(700)

#*************************************************
#question 5:Compare Results to Built-In R Function
#*************************************************
expressions=500000

a = seq(1,500, length.out = 500)
b = seq(1,500, length.out = 500)
c = seq(1,500, length.out = 500)


for (i in 1:500)
{   result= (system.time(sum_normal_lgamma(i)))
  a[i] = result
}

for (i in 1:500)
{ 
  result= (system.time(sum_log_gamma_loop(i)))
  b[i] = result
}


for (i in 1:500)
{ 
  result= (system.time(sum_log_gamma_recursive(i)))
  c[i] = result
}


R=data.frame(a=a, b=b,c=c)

names(R) = c("sum_builtin_lgamma","sum_log_gamma_loop","sum_log_gamma_recursive")
R$n_number = seq(1,500,length.out = 500)

head(R)

subR=subset(R, n_number%%50==0 )
head(subR)
ggplot() + 
  geom_line(data=subR, aes(x=n_number, y=sum_builtin_lgamma), color='red') + 
  geom_line(data=subR, aes(x=n_number, y=sum_log_gamma_loop), color='green') + 
  geom_line(data=subR, aes(x=n_number, y=sum_log_gamma_recursive), color='blue') +
  xlab("time need for three different lgamma algorithm")+
  ylab("time in seconds") +
  labs(title="Compare Results to Built-In R Function")

subR
tt=seq(1,2,length.out=2)
tt
s = system.time(sum(as.numeric(1:100000000)))
print(s)
tt[1]=s
tt[2]=s

xxx = seq(1,10, length.out = 10)
for (i in 1:10)
{   result= (system.time(sum_normal_lgamma(i)))
xxx[i] = result
print (xxx[i])
}
xxx
   