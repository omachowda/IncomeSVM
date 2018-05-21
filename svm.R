library(ggplot2)
data = read.csv("/Users/omachowda/Desktop/AMLHW2/adult.csv",header = FALSE,skipNul = TRUE)
data$label = 1
for (i in 1:length(data$V1)){
  if (as.character(data[i,]$V7) == " <=50K"){
    data[i,]$label = -1
  }
}

uvar <- function(x) {
  replace = c()
  m = mean(x)
  s = sd(x)
  print (s)
  for (i in 1:length(x)){
    replace[i] = (x[i]-m)/s
  }
  return (replace)
}

mean(uvar(data$V1))
var(uvar(data$V2))

udata = data.frame(uvar(data$V1), uvar(data$V2), uvar(data$V3), uvar(data$V4), uvar(data$V5), uvar(data$V6), data$label)

smp_size <- floor(0.8 * nrow(udata))
set.seed(123)

train_ind <- sample(seq_len(nrow(udata)), size = smp_size)

train <- udata[train_ind, ]
split <- udata[-train_ind, ]

smp_size <- floor(0.5 * nrow(split))
train_ind <- sample(seq_len(nrow(split)), size = smp_size)
test <- split[train_ind, ]
validate <- split[-train_ind, ]

inita = c(1,1,1,1,1,1)
initb = 0

update = function(learn, reg, iterations,val_samps){
  a = inita
  b = initb
  acc = c()
  weight = c()
  counter = 1
  check = validate[sample(nrow(validate),val_samps),]
  for (j in 1:iterations){
    i = sample(1:26048, 1)
    #print (i)
    x = train[i,1:6]
    y = train[i, 7]
    if ((as.matrix(x) %*% as.matrix(inita) * y ) >=1){
      a = a - learn*(reg*(a))
    }
    else{
      a =a - learn*(reg*(a)) + y*(x)
      b = b -learn*(y*-1)
    }
   if (j%% 30 == 0){
    a[7] = b
    acc[counter] = predict(a, check)
    bt = 0 
    for (t in 1:6){
      bt = bt + (a[t]**2)
    }
    weight[counter] = sqrt(bt)
    counter = counter+1
    a = a[-7]
    }
  }
   a[7] = b
    return (c(acc,weight))
}

predict = function(a,validate1){
  y_pred = c()
  for ( i in 1: length(validate1[,1])){
    if ((as.matrix(validate1[i,1:6]) %*% t(as.matrix(a[1:6])) + a[7]) > 0){
      y_pred[i] = 1
    }
    else{
      y_pred[i] = -1
    }
  }
  return (table(y_pred == validate1$data.label)["TRUE"]/(table(y_pred == validate1$data.label)["FALSE"]
                                                        +table(y_pred == validate1$data.label)["TRUE"]))
}

a= update(1,0.001,300,50)

accmag = function(constant){
  accuracies = c()
  magnitudes = c()
  index=10
  for (i in 1:50){
    dead= update(1,constant,300,50)
    print(dead[11:20])
    accuracies[(index-9) : index] = dead[1:10]
    magnitudes[(index - 9) : index] = dead[11:20]
    index=index+10
  }
  return (c(accuracies,magnitudes))
}

accmag1 = accmag(1)
accmag2 = accmag(0.1)
accmag3 = accmag(0.01)
accmag4 = accmag(0.001)

ac4 <- data.frame( epoch=seq(,from = 1,to = 500,by = 1),accy=accmag4[1:500])
ac3 <- data.frame( epoch=seq(,from = 1,to = 500,by = 1),accy=accmag3[1:500])
ac2 <- data.frame( epoch=seq(,from = 1,to = 500,by = 1),accy=accmag2[1:500])
ac1 <- data.frame( epoch=seq(,from = 1,to = 500,by = 1),accy=accmag1[1:500])

m4 <- data.frame( epoch=seq(,from = 1,to = 500,by = 1),accy=accmag4[501:1000])
m3 <- data.frame( epoch=seq(,from = 1,to = 500,by = 1),accy=accmag3[501:1000])
m2 <- data.frame( epoch=seq(,from = 1,to = 500,by = 1),accy=accmag2[501:1000])
m1 <- data.frame( epoch=seq(,from = 1,to = 500,by = 1),accy=accmag1[501:1000])

plot(x=ac4, xName="epoch", yName='acc')

plot(ac4, type="o",col="blue")
lines(ac3, type="o",   col="red")
lines(ac2, type="o", col="green")
lines(ac1, type="o",   col="yellow")

plot(m4, type="o", col="blue")
lines(m3, type="o", pch=22, lty=2, col="red")
lines(m2, type="o", pch=22, lty=2, col="green")
lines(m1, type="o", pch=22, lty=2, col="yellow")


mean(accmag(0.01)[1:500])
#check1 = validate[sample(nrow(validate),50),]
#my1 = predict(a,check1)








