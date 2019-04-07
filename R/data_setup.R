

library(xml2)
library(rvest)
library(tidyverse)

#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_201')
library(rJava)
library(tabulizer)
library(tabulizerjars)

# Dependent Variable

location <- "data-raw/GallupKoreaDailyOpinion_174(20150807).pdf"
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
write.csv(app_df, "data//korapp.csv", row.names = FALSE, na = "")


# Independent Variables

library(rio)
library(jsonlite)
library(haven)

terr1994 <- stream_in(gzfile("C:/Users/bomim/Documents/cryingdoves/data-raw/terrier-no-location-source-complete-1994.json.gz"))
varnames<-colnames(terr1994)
rm(terr1994)

terr1993 <- import("C:/Users/bomim/Documents/cryingdoves/data-raw/terrier-no-location-source-complete-1993.json.gz.tsv")
colnames(terr1993) <- varnames

gold1993m <- terr1993 %>%
   group_by(tgt_actor, month) %>%
    filter(src_actor=="KOR", tgt_actor=="JPN"|tgt_actor=="PRK"|tgt_actor=="CHN") %>%
    summarize(avr_gold=mean(goldstein, na.rm=TRUE),
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
    summarize(avr_gold=mean(goldstein, na.rm=TRUE),
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

gold_total <- list()

for(t in 1994:2001){
  namet <- paste0("data-raw/terrier-no-location-source-complete-", t, ".json.gz.tsv")
  terrt<- import(namet)
  goldtm <- gol_format(terrt)
  gold_total_b[[t-1993]] <- goldtm
}


# Download data @ flash drive


for(t in 2002:2014){
  namet <- paste0("D:/terrier-no-location-source-complete-", t, ".json.gz.tsv")
  terrt<- import(namet)
  goldtm <- gol_format(terrt)
  gold_total[[t-1993]] <- goldtm
}

rm(terrt)

gold1994m <- gold_total[[1]]
gold1995m <- gold_total[[2]]
gold1996m <- gold_total[[3]]
gold1997m <- gold_total[[4]]
gold1998m <- gold_total[[5]]
gold1999m <- gold_total[[6]]
gold2000m <- gold_total[[7]]
gold2001m <- gold_total[[8]]
gold2002m <- gold_total[[9]]
gold2003m <- gold_total[[10]]
gold2004m <- gold_total[[11]]
gold2005m <- gold_total[[12]]
gold2006m <- gold_total[[13]]
gold2007m <- gold_total[[14]]
gold2008m <- gold_total[[15]]
gold2009m <- gold_total[[16]]
gold2010m <- gold_total[[17]]
gold2011m <- gold_total[[18]]
gold2012m <- gold_total[[19]]
gold2013m <- gold_total[[20]]
gold2014m <- gold_total[[21]]


gold_all_m <- rbind(gold1993m, gold1994m, gold1995m, gold1996m,
                    gold1997m, gold1998m, gold1999m, gold2000m,
                    gold2001m, gold2002m, gold2003m, gold2004m,
                    gold2005m, gold2006m, gold2007m, gold2008m,
                    gold2009m, gold2010m, gold2011m, gold2012m,
                    gold2013m, gold2014m)

write.csv(gold_all_m, "data//gold_all_m.csv", row.names = FALSE, na = "")

## Collapse data (quarterly)

gold_all_q <-  gold_all_m %>%
    group_by(year, quarter) %>%
      summarize(kj_q = mean(ko_jp_gol, na.rm=TRUE),
                kp_q = mean(ko_pr_gol, na.rm=TRUE),
                kc_q = mean(ko_ch_gol, na.rm=TRUE))

write.csv(gold_all_q, "data//gold_all_q.csv", row.names = FALSE, na = "")


# Control Variables

## Unemployment rate (https://data.oecd.org/unemp/unemployment-rate.htm)

unemp <- read.csv("data-raw//DP_LIVE_07042019232133993.csv")
head(unemp)
unemp_kor <- unemp %>%
  filter(ï..LOCATION=="KOR") %>%
  separate(TIME, c("year", "month"), sep = "-") %>%
  transmute(month = as.numeric(month),
            year = ifelse(month>=3, as.numeric(year), as.numeric(year)-1),
            quarter = ifelse((month>=3)&(month<6), 1, 
                 ifelse((month>=6)&(month<9), 2, 
                        ifelse((month>=9)&(month<12), 3, 4))),
            unemp_k = Value)


unemp_q <- unemp_kor %>%
  group_by(year, quarter) %>%
  summarize(unemp_k_q = mean(unemp_k, na.rm=TRUE))

write.csv(unemp_q, "data//unemp_k_q.csv", row.names = FALSE, na = "")

## CPI (https://data.oecd.org/price/inflation-cpi.htm)

cpi <- read.csv("data-raw//DP_LIVE_07042019232324254.csv")
head(cpi)

cpi_kor <- cpi %>%
  filter(ï..LOCATION=="KOR") %>%
  separate(TIME, c("year", "month"), sep = "-") %>%
  transmute(month = as.numeric(month),
            year = ifelse(month>=3, as.numeric(year), as.numeric(year)-1),
            quarter = ifelse((month>=3)&(month<6), 1, 
                             ifelse((month>=6)&(month<9), 2, 
                                    ifelse((month>=9)&(month<12), 3, 4))),
            cpi_k = Value)


cpi_q <- cpi_kor %>%
  group_by(year, quarter) %>%
  summarize(cpi_k_q = mean(cpi_k, na.rm=TRUE))

write.csv(cpi_q, "data//cpi_k_q.csv", row.names = FALSE, na = "")
