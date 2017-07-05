library(dplyr)
library(openxlsx)
library(stringr)

d1314 <- read.csv("")

d1415 <- read.csv("")

d1516 <- read.csv("")

names(d1516)[29] <- "DBN"

str(d1516)

cte_ml <- read.xlsx("", sheet = "inventory 2017.01.23") 

cte_ml$CIP.code <- gsub('^[0]', '',cte_ml$CIP.code )

cte_ml$dbn_cip <- paste(cte_ml$dbn, cte_ml$CIP.code, sep = "-")

str(cte_ml)

ap <- cte_ml %>%
          filter(program.status == "Approved") %>%
          select(dbn, CIP.code) 

names(ap)[1] <- "DBN"

str(ap)

write.csv(ap, "")
table(d1314$DIPLOMA)

names(d1314_1)
tbl_df(cte_ml)

d1314_1 <- d1314 %>%
  filter(str_detect(DIPLOMA, 'CTE')) %>%
  inner_join(ap, by="DBN") %>%
  mutate(dbn_id = paste(DBN, ID, sep="-")) %>%
  filter(!duplicated(dbn_id))

d1314_1$dup <- duplicated(d1314_1$ID)
table(d1314_1$dup)

str(d1415)
d1415_1 <- d1415 %>%
  filter(str_detect(DIPLOMA, 'CTE')) %>%
  inner_join(ap, by="DBN") %>%
  mutate(dbn_id = paste(DBN, ID, sep="-")) %>%
  filter(!duplicated(dbn_id))
d1415_1$dup <- duplicated(d1415_1$ID)
table(d1415_1$dup)


d1516_1 <- d1516 %>%
  filter(str_detect(diploma, 'CTE')) %>%
  inner_join(ap, by="DBN") %>%
  mutate(dbn_id = paste(DBN, student_id, sep="-")) %>%
  filter(!duplicated(dbn_id))

d1516_1$dup <- duplicated(d1516_1$student_id)
table(d1516_1$dup)


count1314 <- as.data.frame(table(d1314_1$DIPLOMA))

count1314_1 <- count1314 %>%
                filter(Freq > 0) %>%
                mutate(year="2013-14")

count1415 <- as.data.frame(table(d1415_1$DIPLOMA))

count1415_1 <- count1415 %>%
  filter(Freq > 0) %>%
  mutate(year="2014-15")

count1516 <- as.data.frame(table(d1516_1$diploma))

count1516_1 <- count1516 %>%
  filter(Freq > 0) %>%
  mutate(year="2015-16")

counts_cte_grads <- rbind(count1314_1, count1415_1, count1516_1)

duplicated()

write.csv(counts_cte_grads, "D:counts_cte_grads2013_14 to 2015_16.csv")




