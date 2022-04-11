boxplot(2,3,4,5)

?subset
attach(iris)
attach(mtcars)
show(iris)
show(mtcars)

a<-subset(iris,Sepal.Length==5.7)

a1<-subset(iris,Sepal.Length>5.7)

a2<-subset(iris,select=Sepal.Length:Species)

a3<-subset(iris,select=Sepal.Length:Petal.Length)


a2<-subset(mtcars, mpg>mean(mpg), select =cyl)
a2

?barplot

aggregate(Sepal.Length,list(Species),mean)

file<-read.csv("/home/dsc2/Documents/R Sem4/bank.csv")
print(file)

#Q1
z<-subset(file,(loan=="yes")&(marital!="divorced"))
print(z)

aggregate(z$marital,list(z$marital),length)


#Q2
y<-subset(file,balance<1000,select = c(salary,job))
print(y)

#Q3
y<-subset(file,((loan=="yes")&(age>28)),select = age)
print(y)
