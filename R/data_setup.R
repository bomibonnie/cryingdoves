

library(xml2)
library(rvest)
library(tidyverse)

#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_201')
library(rJava)
library(tabulizer)
library(tabulizerjars)

location <- "C:/Users/bomim/Documents/cryingdoves/data-raw/GallupKoreaDailyOpinion_174(20150807).pdf"
tab <- extract_tables(location, pages=3)

x <- cbind(tab[[1]][c(6, 8, 10, 12, 14),3:17]) %>%
  data.frame() %>%
  separate(X2, c("Q12", "Q13"), sep = " ") %>%
  separate(X5, c("Q22", "Q23"), sep = " ") %>%
  separate(X8, c("Q32", "Q33"), sep = " ") %>%
  separate(X11, c("Q42", "Q43"), sep = " ") %>%
  separate(X14, c("Q52", "Q53"), sep = " ") 

president <- c("Y Kim", "D Kim", "M Roh", "M Lee", "G Park")
rownames(x) <- president
quarter <- c("1.1", "1.2", "1.3", "1.4", "2.1", "2.2", "2.3", "2.4",
             "3.1", "3.2", "3.3", "3.4", "4.1", "4.2", "4.3", "4.4",
             "5.1", "5.2", "5.3", "5.4")
colnames(x) <-quarter
x <- rownames_to_column(x, var = "president")

app_df <- x %>%
  gather(quarter, approval, -president) %>%
  mutate(year1 = ifelse(president == "Y Kim", 1992, 
                        ifelse(president== "D Kim", 1997, 
                             ifelse(president=="M Roh", 2002,
                                  ifelse(president=="M Lee", 2007, 2012)))),
       year = year1 + as.numeric(str_sub(quarter, 1, 1)),
      quarter = str_sub(quarter, -1, -1)) %>%
  arrange(year, quarter)

app_df <- app_df[,c("year", "quarter", "president", "approval")]  
write.csv(app_df, "C://Users//bomim//Documents//cryingdoves/data//korapp.csv", row.names = FALSE)


terr_link <- "https://osf.io/4m2u7/files/"