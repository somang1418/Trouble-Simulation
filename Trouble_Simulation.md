Trouble
================

\#\#\#What is the average length of a move?

A function generates a simulation of die

``` r
die<-function(){
  sample(1:6,1)
}
die()
```

    ## [1] 3

``` r
aDie<-function(x){
  sample(1:6,x,replace=TRUE)
}
#15 times rolling 
aDie(15)
```

    ##  [1] 3 3 6 2 2 2 6 5 4 3 5 6 4 4 6

If you roll x, you can roll and move again\! This move function
simulates the process

``` r
move<-function(x){
  myroll=die()
  total=myroll
  while(myroll==x){
    myroll=die()
    total=myroll+total
    }
   return(total)
}
```

***movelist generates n length of vector of total depending on gogain
value=x***

``` r
movelist<-function(n,x){
  i=1
  ret=c()
  while(i<=n){
    ret[i]=move(x)
    i=i+1
  }
  return(ret)
}

mean(movelist(10000,6))
```

    ## [1] 4.1972

The average length of a move is around 4.2

\#\#\#\#Graphing variables with a line that shows a mean value

``` r
goagain1=movelist(1000,1)
goagain2=movelist(1000,2)
goagain3=movelist(1000,3)
goagain4=movelist(1000,4)
goagain5=movelist(1000,5)
goagain6=movelist(1000,6)

goagaintable=cbind(goagain1,goagain2,goagain3,goagain4,goagain5,goagain6)
meanvalue=c(mean(goagain1),mean(goagain2),mean(goagain3),mean(goagain4),mean(goagain5),mean(goagain6))
goagaintable=rbind(goagaintable,meanvalue)
```

``` r
par(mfrow=c(2,3))
plot(goagain1,main="d=1",ylab="total")
abline(h=mean(goagain1))
plot(goagain2,main="d=2",ylab="total")
abline(h=mean(goagain2))
plot(goagain3,main="d=3",ylab="total")
abline(h=mean(goagain3))
plot(goagain4,main="d=4",ylab="total")
abline(h=mean(goagain4))
plot(goagain5,main="d=5",ylab="total")
abline(h=mean(goagain5))
plot(goagain6,main="d=6",ylab="total")
abline(h=mean(goagain6))
```

![](Trouble_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
meanvalue
```

    ## [1] 4.180 4.205 4.088 4.158 4.239 4.360

It shows that values for 1,2,3,4,5,6 dice values vary when we generate
values for 1000 times, but when their mean values are very similar. The
average length of a move is around 4.1 to 4.3 When d=1,2,3,4,5,6.

\#\#\#Real Question: Is this a fair game with using different “go again
value”(ex. using 1 vs using 6), in other words both players have an
equal probability of winning?

``` r
Prob<-function(n){
  i=1
  win6=0
  win1=0
  equal=0
  dice6=movelist(n,6)
  dice1=movelist(n,1)
  while(i<=n){
    if(dice6[i]>dice1[i]){
      win6=win6+1
    }
    if(dice1[i]>dice6[i]){
      win1=win1+1
    }
      if(dice1[i]==dice6[i]){
        while(dice6[i]==dice1[i]){
          dice1[i]=movelist(1,1)
          dice6[i]=movelist(1,6)
          if(dice6[i]>dice1[i]){win6=win6+1}
          if(dice1[i]>dice6[i]){win1=win1+1}
          }
      }
    i=i+1
  }
  prob1=win1/n#probability of winning with 1
  prob6=win6/n#probability of winning with 6
  prob=c(prob1,prob6)
  print(prob[1]+prob[2])
  return(prob)
}

cat("The probability of winning a game with 1,6 with 1000000 times simulation is",Prob(10000))
```

    ## [1] 1
    ## The probability of winning a game with 1,6 with 1000000 times simulation is 0.6033 0.3967

When we simulated 10000 times, a probability of winning with 1 is higher
than a probability of winning with 6. Adding up two probabilities is
1(verifying)\!

``` r
Prob1<-function(n){
  i=1
  win2=0
  win5=0
  dice2=movelist(n,2)
  dice5=movelist(n,5)
  while(i<=n){
    if(dice2[i]>dice5[i]){
      win2=win2+1
    }
    if(dice5[i]>dice2[i]){
      win5=win5+1
    }
    if(dice2[i]==dice5[i]){
      while(dice2[i]==dice5[i]){
          dice2[i]=movelist(1,2)
          dice5[i]=movelist(1,5)
          if(dice2[i]>dice5[i]){win2=win2+1}
          if(dice5[i]>dice2[i]){win5=win5+1}
          }
      }
    i=i+1
  }
  prob2=win2/n#probability of winning with 2
  prob5=win5/n#probability of winning with 5
  prob1=c(prob2,prob5)
  print(prob1[1]+prob1[2])
  return(prob1)
}


cat("The probability of winning a game with 2,5 with 1000000 times simulation is",Prob1(10000))
```

    ## [1] 1
    ## The probability of winning a game with 2,5 with 1000000 times simulation is 0.551 0.449

When we simulated 10000 times, a probability of winning with 2 is higher
than a probability of winning with 5. Adding up two probabilities is
1(verifying)\!

\#\#\#Summary

Choosing smaller number has a better chance to win a game since a user
can have more rollings, which enhances a chance of winning
game(ex.getting 6 means that you already wins the game, so you will not
roll. However, if you get 1, you are going to roll more, which ends up
making a bigger total number.
