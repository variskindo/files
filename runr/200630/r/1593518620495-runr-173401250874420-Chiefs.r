#Kaizer Chiefs last 6 league games performance in the last five league seasons(2013/14-2018/19)
Match<-c('Chiefs v Black Aces','Plk  v Chiefs','Wits v Chiefs','Chiefs v F.S Stars','Tuks v Chiefs','Chiefs v Amazulu')
Date<-c('09/04/2014','16/04/2014','23/04/2014','30/04/2014','06/05/2014','10/05/2014')
Results<-c('0:1','2:0','0:0','1:1','1:0',3:0)
Win_Lose_Draw<-c('lose','lose','draw','draw','lose','win')
Points<-c(0,0,1,1,0,3)
Points_accumulated<-sum(Points)
data.frame<- (Match,Date,Results,Win_Lose_Draw,Points,Points_accumulated)
print(data.frame)
