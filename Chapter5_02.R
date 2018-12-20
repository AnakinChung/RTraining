p = 0.03
if (p<=0.05)  print("p <= 0.05!") else  
  print("p > 0.05!")

print('-----')
ifelse(p<=0.05, print(“p <= 0.05!”), print(“p > 0.05!”))

print('-----')
x <- c(6:-4) 
sqrt(x) #- gives warning 
sqrt(ifelse(x >= 0, x, NA)) 
x=ifelse(NA, "a", "b")
x

