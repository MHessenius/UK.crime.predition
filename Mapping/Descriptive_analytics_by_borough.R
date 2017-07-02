### Descriptive analytics
if(!require("dplyr")) install.packages("dplyr"); library("dplyr")

# Crime type
table(crime.whole$Crime.type)

crime.type <- data.frame(table(crime.whole$Crime.type)) %>% arrange(Freq, desc(Freq))

# Crime type 2011

crime.type.2011 <- data.frame(table(crime2011$Crime.type, crime2011$id))
colnames(crime.type.2011) <- c("Crime_type", "id", "Freq")
mutate(crime.type.2011, total_percentage = Freq/sum(crime.type.2011$Freq))

mutate(crime.type.2011, id_percentage = Freq/sum(crime.type.2011$Freq[]))

library(dplyr)

crime.type.2011 >%> 
  group_by(crime.type.2011, id) >%>
  summarise(crime.type.2011, sumFreq = sum(Freq))

total.crime.2011 <- summarise(crime.type.2011,sumFreq = sum(Freq))


# "R sumif equivalent" --> dplyr package
# total %>%
#   group_by(names) %>%
#   summarise(Sumx1 = sum(x1), Sumx2 = sum(x2))