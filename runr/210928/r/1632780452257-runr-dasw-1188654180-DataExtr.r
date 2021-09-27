#install relevant packages and activate them
install.packages("guf")
install.packages("rvest")
install.packages("xml2")
install.packages("writexl")
library(guf)
library(rvest)
library(xml2)
library(writexl)

#define the googledoc as matrix
roj <- read_excel("P:\\Downloads\\Test\\Roj.xlsx")
mat <- as.matrix(roj)

#extraction function
data_extract <- function(i) {
  site <- paste("https://mypst.com.br/jogos/", mat[i,2], sep = "")
  pagecode = read_html(site)
  pagecode = html_nodes(pagecode, css = ".txtGameName")
  pagecode = html_text(pagecode)
  pagecode
}

#write them to relevant columns
for (i in 2:2453) {
  data_extract(i)
  if (right(data_extract(i), 1) == "]"){
  #You do not need to write the names by hand if you activate the following line
  mat[i,3] <- str_sub(data_extract(i), end = -7)
  mat[i,1] <- right(data_extract(i), 5)}
  else {mat[i,3] <- data_extract(i)}
}

#export an excel file
output <- as.data.frame(mat)
write_xlsx(output,"P:\\Downloads\\Test\\output.xlsx")







