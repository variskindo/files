install.packages(c("odbc","DBI","tidyverse","lubridate"))
library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
con <- dbConnect(odbc(),
                 Driver="SQL Server",
                 Server="met-sql19.bu.edu",
                 Database="NYC Real Estate",
                 Port=1433)
Neighborhood <- dbReadTable(con,"Neighborhood") 
Neighborhood$NbhoodName<-str_trim(Neighborhood$NbhoodName)
NYCCurrent <- dbReadTable(con,"NYCCurrent")
NYCHistorical <- dbReadTable(con,"NYCHistorical")
BuildingCode <- dbReadTable(con,"BuildingCode")
Borough <- dbReadTable(con,"Borough")

NYC<-full_join(NYCHistorical,NYCCurrent)

#without filter
YearlySale_1 <- NYC %>%
  left_join(Neighborhood,by="NbhoodID") %>%
  left_join(BuildingCode,by=c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(NbhoodID,Status,GrossSqFt,SalePrice,SaleDate) %>%
  filter(NbhoodID=="113",Status=="Residential") %>%
  group_by(year=year(SaleDate))
df1<-summarise(YearlySale_1,ResidentialSales1=sum(SalePrice),TotalGrossSqFt1=sum(GrossSqFt)) %>%
  mutate(AverageResidential1=ResidentialSales1/TotalGrossSqFt1)

#compare
YearlyNeighborhood<-function(Name){
  df <- NYC %>%
  left_join(Neighborhood,by="NbhoodID") %>%
  left_join(BuildingCode,by=c("BuildingClassTimeOfSale"="BuildingCodeID")) %>%
  select(NbhoodID,NbhoodName,Status,GrossSqFt,SalePrice,SaleDate) %>%
  filter(NbhoodName==Name,GrossSqFt!=0,SalePrice!=0,Status=="Residential") %>%
  group_by(year=year(SaleDate)) 
summarise(df,ResidentialSales=sum(SalePrice),TotalGrossSqFt=sum(GrossSqFt)) %>%
  mutate(AverageResidential=ResidentialSales/TotalGrossSqFt)
}

FLATBUSHEAST<-YearlyNeighborhood("FLATBUSH-EAST")
FLATBUSHNORTH<-YearlyNeighborhood("FLATBUSH-NORTH")
FLATLANDS<-YearlyNeighborhood("FLATLANDS")

ggplot()+geom_line(data=FLATBUSHEAST,size=2,aes(x=year,y=ResidentialSales,color="blue"))+geom_line(data=FLATBUSHNORTH,size=2,aes(x=year,y=ResidentialSales,color="red"))+geom_line(data=FLATLANDS,size=2,aes(x=year,y=ResidentialSales,color="green"))+scale_color_discrete(name="YearlyNeighborhood",labels=c("FLATBUSH-EAST","FLATLANDS","FLATBUSH-NORTH"))

print.data.frame(FLATBUSHEAST)
print.data.frame(FLATBUSHNORTH)
print.data.frame(FLATLANDS)