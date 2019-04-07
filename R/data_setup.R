

library(xml2)
library(rvest)
library(tidyverse)

#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_201')
library(rJava)
library(tabulizer)
library(tabulizerjars)

# Dependent Variable

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


# Independent Variables

library(rio)
library(jsonlite)
library(haven)

terr1994 <- stream_in(gzfile("C:/Users/bomim/Documents/cryingdoves/data-raw/terrier-no-location-source-complete-1994.json.gz"))
varnames<-colnames(terr1994)

terr1993 <- import("C:/Users/bomim/Documents/cryingdoves/data-raw/terrier-no-location-source-complete-1993.json.gz.tsv")
colnames(terr1993) <- varnames

gold1993m <- terr1993 %>%
   group_by(tgt_actor, month) %>%
    filter(src_actor=="KOR", tgt_actor=="JPN"|tgt_actor=="PRK"|tgt_actor=="CHN") %>%
    summarize(avr_gold=mean(goldstein),
              year=mean(year)) %>%
    spread(tgt_actor, avr_gold) %>%
    transmute(year = ifelse(month>=3, as.numeric(year), as.numeric(year)-1),
              month = as.numeric(month),
              quarter = ifelse((month>=3)&(month<6), 1, 
                               ifelse((month>=6)&(month<9), 2, 
                                      ifelse((month>=9)&(month<12), 3, 4))),
              ko_jp_gol = JPN,
              ko_pr_gol = PRK,
              ko_ch_gol = CHN) 

### Make a function 
gol_format <- function(df){
  x <- df
  colnames(x) <- varnames
  x<- x %>% 
  group_by(tgt_actor, month) %>%
    filter(src_actor=="KOR", tgt_actor=="JPN"|tgt_actor=="PRK"|tgt_actor=="CHN") %>%
    summarize(avr_gold=mean(goldstein),
              year=mean(year)) %>%
    spread(tgt_actor, avr_gold) %>%
    transmute(year = ifelse(month>=3, as.numeric(year), as.numeric(year)-1),
              month = as.numeric(month),
              quarter = ifelse((month>=3)&(month<6), 1, 
                               ifelse((month>=6)&(month<9), 2, 
                                      ifelse((month>=9)&(month<12), 3, 4))),
              ko_jp_gol = JPN,
              ko_pr_gol = PRK,
              ko_ch_gol = CHN) 
}

test1993<-gol_format(terr1993)
rm(terr1993)

terr1994 <- import("data-raw/terrier-no-location-source-complete-1994.json.gz.tsv")
gold1994m <- gol_format(terr1994)
rm(terr1994)

terr1995 <- import("data-raw/terrier-no-location-source-complete-1995.json.gz.tsv")
gold1995m <- gol_format(terr1995)
rm(terr1995)

terr1996 <- import("data-raw/terrier-no-location-source-complete-1996.json.gz.tsv")
gold1996m <- gol_format(terr1996)
rm(terr1996)

gold_total <- list()

for(t in 1998:2000){
  namet <- paste0("data-raw/terrier-no-location-source-complete-", 1998, ".json.gz.tsv")
  terrt<- import(namet)
  goldtm <- gol_format(terrt)
  gold_total[[t-1997]] <- goldtm
}

gold1998m <- gold_total[[1]]


## Collapse data (quarterly)

gold1993q <-  gold1993m %>%
    group_by(year, quarter) %>%
      summarize(kj_q = mean(ko_jp_gol),
                kp_q = mean(ko_pr_gol),
                kc_q = mean(ko_ch_gol))

