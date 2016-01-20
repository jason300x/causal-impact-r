# comments are only in polish right now-- please forgive me
#-------- funkcja pozwalająca rysować kilka wykresów obok siebie--------------
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


c_data_analysis<-function(a,b,a1,b1,a2,b2,c,d,e,F,n,PA) {

library(CausalImpact)
set.seed(1)
x1 <- b + arima.sim(model = list(ar = 0.999), n)
y <- a * x1 + rnorm(n)
y1 <-x1
y2 <-x1
y3 <- a * x1 + rnorm(n)

a3=a2+F
b3=b2+F

y[a1:b1] <- y[a1:b1] + c
y[a2:b2] <- y[a2:b2] + d
y[a3:b3] <- y[a3:b3] + e


# 1 byc moze do zmiany na a1 i koniec na b3 z n

for (i in 1:b1)
y1[i]=y[i]
for(j in a2:b2)
y1[j]=y[j]
for(k in a3:n)
y1[k]=y[k]


data2<-cbind(y1,x1)
i=1
j=1
k=1
l=1
m=1
Z1=1

while(i<=PA-1)
{
prei.period<-c(1,(n/PA)*i)
posti.period<-c(((n/PA)*i)+1,(n/PA)*(i+1))
impacti<-CausalImpact(data2,prei.period,posti.period)
ploti<-plot(impacti)

#\print(j)
#\print(prei.period)
#\print(posti.period)
#\print(impacti)

if(i==1)
{
multiplot(ploti) 
sys.sleep(5)
}
else if((i>1)&(i<=5))
{
prej.period<-c(1,(n/PA)*j)
postj.period<-c(((n/PA)*j)+1,(n/PA)*(j+1))
impactj<-CausalImpact(data2,prej.period,postj.period)
plotj<-plot(impactj)

multiplot(ploti,plotj,cols=2) 
}
else if (i>=6)
{
prej.period<-c(1,(n/PA)*j)
postj.period<-c(((n/PA)*j)+1,(n/PA)*(j+1))
impactj<-CausalImpact(data2,prej.period,postj.period)
plotj<-plot(impactj)

prem.period<-c(1,(n/PA)*m)
postm.period<-c(((n/PA)*m)+1,(n/PA)*(m+1))
impactm<-CausalImpact(data2,prem.period,postm.period)
plotm<-plot(impactm)

prek.period<-c(1,(n/PA)*k)
postk.period<-c(((n/PA)*k)+1,(n/PA)*(k+1))
impactk<-CausalImpact(data2,prek.period,postk.period)
plotk<-plot(impactk)

prel.period<-c(1,(n/PA)*l)
postl.period<-c(((n/PA)*l)+1,(n/PA)*(l+1))
impactl<-CausalImpact(data2,prel.period,postl.period)
plotl<-plot(impactl)

preZ1.period<-c(1,(n/PA)*Z1)
postZ1.period<-c(((n/PA)*Z1)+1,(n/PA)*(Z1+1))
impactZ1<-CausalImpact(data2,preZ1.period,postZ1.period)
plotZ1<-plot(impactZ1)
multiplot(ploti,plotj,plotm,plotk,plotl,plotZ1,cols=3) 
}
 
#print(prei.period)
summary(impacti, "summary")

#printsummary(impacti)
#print(impacti$summary$p[2])

#zmienna p prawdopodobiestwo odejmowanie po wykryciu efektu
#brakuje zmiennej że musi rosnąć - bo efekt malejący to wciąż efekt

x<-impacti$summary$p[1]
x<-(1-x)
Rel<-impacti$summary$RelEffect[1]
print(Rel)

PX1=(((n/PA)*i)-1)
PX2=((n/PA)*(i+1)+2)

#print(PX1)
#print(PX2)

if((x>=0.700 & Rel>=0.0040) &i==1)
{
for( i1 in PX1:PX2)
{
#najważniejsza rzecz do zmiany
y1[i1]=y3[i1]
data2<-cbind(y1,x1)
}

i=i
}

else
{
i=i+1
j=i-1
m=i-2
k=i-3
l=i-4
Z1=i-5
}
#tutaj kończymy, w tej chwili odejmujemy - całkowicie wypłaszczamy
}
}

c_data_analysis(1.2,180,21,80,60,120,8,12,22,40,250,8)



