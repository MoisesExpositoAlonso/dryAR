ratiowomen<-function(){

exp<-c(1,0,0,1,1,0,1,0,1,1,1,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,0,1,0)
conflu2017<-c(0,0,1,1,1,1,1,0,1,1,0,0,0,0,0,0,0,1,1,1,0,1,1,0,0,0,1,1,1,1,0,0,0,1,1,1,0,0,0,1,0,0,1,0,1)
conflu<-c(1,1,1,0,1,0,1,0,1,0,0,0,1,1,1,0,0,1,0,0,0,1,0,1,1,1,1,0,1,1,0,1,0,1,
          0,0,0,0,1,0,1,0,0,0,1) # from memmory
badurach=c(0,1,0,0,1,0,0,0,0,1,1,1,0,0,0,1,0,1,0,0,1,1,0,1,1,1,1,1,1,0,1,0,0,1,1,1,0,0,0,0,0,0,0)

lab=badurach

print("the ratio of woman altruistically working in the experiment 2015-2016 was:")
print(sum(exp) / length(exp) )
print('while in Weigel lab (spring 2015 Bad Urach)')
print(100* sum(conflu) / length(conflu) )


Pw= sum(exp) / length(exp)

bayesAB=function(BA,A,B){ BA * A / B }

Phelp=length(exp)/length(lab)

Pwomen=sum(lab) / length(lab)
Pmen= 1-(sum(lab) / length(lab))

Phelp.women=sum(exp) / sum(lab)
Phelp.men= (length(exp)-sum(exp) ) / sum(lab)

Pwomen.help=bayesAB(BA=Phelp.women,
                    A= Pwomen,
                    B=Phelp
                      )
print(paste("the bayes posterior probability of helping given you are a women is:",Pwomen.help))


Pmen.help=bayesAB(BA=Phelp.men,
                    A= Pmen,
                    B=Phelp
                      )
print(paste("the bayes posterior probability of helping given you are a men is:",Pmen.help))

tab=matrix( c(sum(lab), sum(exp),
       length(lab)-sum(lab), length(exp)-sum(exp) )
       ,ncol=2)

print(paste("the p-value of a Fisher test to evaluate whether there is interaction of bein women and helping was:", format(fisher.test(tab)$p.value,digits=3) ))


}



