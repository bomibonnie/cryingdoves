

library(xml2)
library(rvest)
library(tidyverse)

#Sys.setenv(JAVA_HOME='C:/Program Files/Java/jre1.8.0_201')
library(rJava)
library(tabulizer)
library(tabulizerjars)

# Dependent Variable 1 (Gallup: Quarterly)

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
      quarter = as.numeric(str_sub(quarter, -1, -1))) %>%
  arrange(year, quarter)

app_df <- app_df[,c("year", "quarter", "president", "approval")]  
write.csv(app_df, "data//korapp.csv", row.names = FALSE, na = "")

# Dependent Variable 2 (Research&Research: Monthly)
library(xlsx)
?read.xlsx
app_m <-read.xlsx("data-raw//KoreanApprovalR&R.xlsx", sheetName = "Sheet1")
app_m <- cbind(app_m[,1:3]) %>%
  rename(year=Year,
         month=Month,
         app_r=Approval_R) %>%
  filter(year<2015) 
  


# Independent Variables: Goldstein scores(monthly & quarterly)

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
    transmute(year = as.numeric(year),
              month = as.numeric(month),
              ko_jp_gol = JPN,
              ko_pr_gol = PRK,
              ko_ch_gol = CHN) 


gold1993q <- terr1993 %>%
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

### Make a function for monthly data
gol_formatm <- function(df){
  x <- df
  colnames(x) <- varnames
  x<- x %>% 
  group_by(tgt_actor, month) %>%
    filter(src_actor=="KOR", tgt_actor=="JPN"|tgt_actor=="PRK"|tgt_actor=="CHN") %>%
    summarize(avr_gold=mean(goldstein, na.rm=TRUE),
              year=mean(year)) %>%
    spread(tgt_actor, avr_gold) %>%
    transmute(year = as.numeric(year),
              month = as.numeric(month),
              ko_jp_gol = JPN,
              ko_pr_gol = PRK,
              ko_ch_gol = CHN) 
}

### Make a function for quarterly data
gol_formatq <- function(df){
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

test1993<-gol_formatm(terr1993)
rm(terr1993)

## For loop for monthly data
gold_totalm <- list()

for(t in 1994:2001){
  namet <- paste0("data-raw/terrier-no-location-source-complete-", t, ".json.gz.tsv")
  terrt<- import(namet)
  goldtm <- gol_formatm(terrt)
  gold_totalm[[t-1993]] <- goldtm
}

# Download data @ flash drive

for(t in 2002:2014){
  namet <- paste0("D:/terrier-no-location-source-complete-", t, ".json.gz.tsv")
  terrt<- import(namet)
  goldtm <- gol_formatm(terrt)
  gold_totalm[[t-1993]] <- goldtm
}

rm(terrt)

gold1994m <- gold_totalm[[1]]
gold1995m <- gold_totalm[[2]]
gold1996m <- gold_totalm[[3]]
gold1997m <- gold_totalm[[4]]
gold1998m <- gold_totalm[[5]]
gold1999m <- gold_totalm[[6]]
gold2000m <- gold_totalm[[7]]
gold2001m <- gold_totalm[[8]]
gold2002m <- gold_totalm[[9]]
gold2003m <- gold_totalm[[10]]
gold2004m <- gold_totalm[[11]]
gold2005m <- gold_totalm[[12]]
gold2006m <- gold_totalm[[13]]
gold2007m <- gold_totalm[[14]]
gold2008m <- gold_totalm[[15]]
gold2009m <- gold_totalm[[16]]
gold2010m <- gold_totalm[[17]]
gold2011m <- gold_totalm[[18]]
gold2012m <- gold_totalm[[19]]
gold2013m <- gold_totalm[[20]]
gold2014m <- gold_totalm[[21]]


gold_all_m <- rbind(gold1993m, gold1994m, gold1995m, gold1996m,
                    gold1997m, gold1998m, gold1999m, gold2000m,
                    gold2001m, gold2002m, gold2003m, gold2004m,
                    gold2005m, gold2006m, gold2007m, gold2008m,
                    gold2009m, gold2010m, gold2011m, gold2012m,
                    gold2013m, gold2014m)

write.csv(gold_all_m, "data//gold_all_m.csv", row.names = FALSE, na = "")



## For loop for quarterly data
gold_totalq <- list()

for(t in 1994:2001){
  namet <- paste0("data-raw/terrier-no-location-source-complete-", t, ".json.gz.tsv")
  terrt<- import(namet)
  goldtq <- gol_formatq(terrt)
  gold_totalq[[t-1993]] <- goldtq
}

# Download data @ flash drive

for(t in 2002:2014){
  namet <- paste0("D:/terrier-no-location-source-complete-", t, ".json.gz.tsv")
  terrt<- import(namet)
  goldtq <- gol_formatq(terrt)
  gold_totalq[[t-1993]] <- goldtq
}

rm(terrt)

gold1994q <- gold_totalq[[1]]
gold1995q <- gold_totalq[[2]]
gold1996q <- gold_totalq[[3]]
gold1997q <- gold_totalq[[4]]
gold1998q <- gold_totalq[[5]]
gold1999q <- gold_totalq[[6]]
gold2000q <- gold_totalq[[7]]
gold2001q <- gold_totalq[[8]]
gold2002q <- gold_totalq[[9]]
gold2003q <- gold_totalq[[10]]
gold2004q <- gold_totalq[[11]]
gold2005q <- gold_totalq[[12]]
gold2006q <- gold_totalq[[13]]
gold2007q <- gold_totalq[[14]]
gold2008q <- gold_totalq[[15]]
gold2009q <- gold_totalq[[16]]
gold2010q <- gold_totalq[[17]]
gold2011q <- gold_totalq[[18]]
gold2012q <- gold_totalq[[19]]
gold2013q <- gold_totalq[[20]]
gold2014q <- gold_totalq[[21]]


gold_all_q <- rbind(gold1993q, gold1994q, gold1995q, gold1996q,
                    gold1997q, gold1998q, gold1999q, gold2000q,
                    gold2001q, gold2002q, gold2003q, gold2004q,
                    gold2005q, gold2006q, gold2007q, gold2008q,
                    gold2009q, gold2010q, gold2011q, gold2012q,
                    gold2013q, gold2014q)

## Collapse data (quarterly)

gold_all_q <-  gold_all_q %>%
    group_by(year, quarter) %>%
      summarize(kj_q = mean(ko_jp_gol, na.rm=TRUE),
                kp_q = mean(ko_pr_gol, na.rm=TRUE),
                kc_q = mean(ko_ch_gol, na.rm=TRUE))

write.csv(gold_all_q, "data//gold_all_q.csv", row.names = FALSE, na = "")


# Control Variables

## Unemployment rate (https://data.oecd.org/unemp/unemployment-rate.htm)

unemp <- read.csv("data-raw//DP_LIVE_07042019232133993.csv")
head(unemp)
unemp_kor_m <- unemp %>%
  filter(ï..LOCATION=="KOR") %>%
  separate(TIME, c("year", "month"), sep = "-") %>%
  transmute(month = as.numeric(month),
            year = as.numeric(year),
            unemp_k = Value)

write.csv(unemp_kor_m, "data//unemp_k_m.csv", row.names = FALSE, na = "")

unemp_kor_q <- unemp %>%
  filter(ï..LOCATION=="KOR") %>%
  separate(TIME, c("year", "month"), sep = "-") %>%
  transmute(month = as.numeric(month),
            year = ifelse(month>=3, as.numeric(year), as.numeric(year)-1),
            quarter = ifelse((month>=3)&(month<6), 1, 
                             ifelse((month>=6)&(month<9), 2, 
                                    ifelse((month>=9)&(month<12), 3, 4))),
            unemp_k = Value)


unemp_q <- unemp_kor_q %>%
  group_by(year, quarter) %>%
  summarize(unemp_k_q = mean(unemp_k, na.rm=TRUE))

write.csv(unemp_q, "data//unemp_k_q.csv", row.names = FALSE, na = "")

## CPI (https://data.oecd.org/price/inflation-cpi.htm)

cpi <- read.csv("data-raw//DP_LIVE_07042019232324254.csv")
head(cpi)

cpi_kor_m <- cpi %>%
  filter(ï..LOCATION=="KOR") %>%
  separate(TIME, c("year", "month"), sep = "-") %>%
  transmute(month = as.numeric(month),
            year = as.numeric(year),
            cpi_k = Value)

write.csv(cpi_kor_m, "data//cpi_k_m.csv", row.names = FALSE, na = "")

cpi_kor_q <- cpi %>%
  filter(ï..LOCATION=="KOR") %>%
  separate(TIME, c("year", "month"), sep = "-") %>%
  transmute(month = as.numeric(month),
            year = ifelse(month>=3, as.numeric(year), as.numeric(year)-1),
            quarter = ifelse((month>=3)&(month<6), 1, 
                             ifelse((month>=6)&(month<9), 2, 
                                    ifelse((month>=9)&(month<12), 3, 4))),
            cpi_k = Value)


cpi_q <- cpi_kor_q %>%
  group_by(year, quarter) %>%
  summarize(cpi_k_q = mean(cpi_k, na.rm=TRUE))

write.csv(cpi_q, "data//cpi_k_q.csv", row.names = FALSE, na = "")

# Merge data
## Monthly
doves_datm <- app_m %>%
  left_join(gold_all_m, by = c("year", "month")) %>%
  left_join(unemp_kor_m, by = c("year", "month")) %>%
  left_join(cpi_kor_m, by = c("year", "month")) %>%
  filter(year<2015) %>%
  mutate(newgov = ifelse((year==1993)&(month==3), 1, 
                         ifelse((year==1998)&(month==3), 1, 
                                ifelse((year==2003)&(month==3), 1,
                                       ifelse((year==2008)&(month==3), 1,
                                              ifelse((year==2013)&(month==3), 1,0))))))
        
write.csv(doves_datm, "data//doves_korm.csv", row.names = FALSE, na = "")

## Quarterly
doves_datq <- app_df %>%
  left_join(gold_all_q, by = c("year", "quarter")) %>%
  left_join(unemp_q, by = c("year", "quarter")) %>%
  left_join(cpi_q, by = c("year", "quarter")) %>%
  filter(year<2015) %>%
  mutate(newgov = ifelse((year==1993)&(quarter==1), 1, 
                         ifelse((year==1998)&(quarter==1), 1, 
                                ifelse((year==2003)&(quarter==1), 1,
                                       ifelse((year==2008)&(quarter==1), 1,
                                              ifelse((year==2013)&(quarter==1), 1,0))))),
         quarterly= paste0(year, "q", quarter))

write.csv(doves_datq, "data//doves_korq.csv", row.names = FALSE, na = "")

