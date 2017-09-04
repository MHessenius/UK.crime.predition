crime <- readRDS("./crime.whole.cleaned.2011.exploded.rds")
if(!require("data.table")) install.packages("data.table"); library("data.table")
if(!require("ggplot2")) install.packages("ggplot2"); library("ggplot2")
if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
if(!require("plotly")) install.packages("plotly"); library("plotly")
train <- data.table(crime)
## crime type and location/borough
train[, .N, by = Location][order(N, decreasing = T)][1:10]
train[, .N, by = Crime.type][order(N, decreasing = T)][1:10]
train[, .N, by = LSOA.name][order(N, decreasing = T)][1:10]
d=train[, .N, by = Crime.type][order(N, decreasing = T)]
ggplot(d, aes(x = reorder(Crime.type, -N), y = N, fill = N)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  scale_fill_continuous(guide=FALSE) + 
  labs(x = '', y = 'Total Number of Crimes', title = 'Total Count of Crimes per Category') 
train$LSOA.name <- as.character(train$LSOA.name)

d2 = train[, .N, by = LSOA.name][order(N, decreasing = T)]
ggplot(d2, aes(x = reorder(LSOA.name, -N), y = N, fill = LSOA.name)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  guides(fill = F) + 
  labs(x = '', y = 'Total Number of Crimes', title = 'Total Number of Crimes per LSOA.name') 
train[,.N, by =.(Location, LSOA.name)][order(N, decreasing = T)][1:10]
d4 = train[, .N, by =.(LSOA.name, Crime.type)][order(N, decreasing = T)][i= !Crime.type %in% c('NONE')]
g = ggplot(d4, aes(x = LSOA.name, y = Crime.type, fill = N)) + 
  geom_tile() +
  scale_fill_distiller(palette = 'RdYlGn') +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(g)
d5 = train[, .N, by =.(LSOA.name, Crime.type)][,Percentage:=N/sum(N)*100, by = Crime.type][order(N, decreasing = T)]
g1 = ggplot(d5, aes(x = LSOA.name, y = Crime.type, fill = Percentage)) + 
  geom_tile() +
  geom_hline(yintercept=seq(0.5, 30, by=1), size = 1, color = 'grey') + 
  scale_fill_distiller(palette = 'RdYlGn') +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
ggplotly(g1)
## we dont have days and weeks to check whether crime rate increase during weekend or during specific periods such as Xmas :/
### weather and Months
d3 = train[, .N, by = Month][order(N, decreasing = T)]
ggplot(d3, aes(x = reorder(Month, -N), y = N, fill = Month)) + 
  geom_bar(stat = 'identity') + 
  coord_flip() + 
  guides(fill = F) + 
  labs(x = '', y = 'Total Number of Crimes', title = 'Total Number of Crimes per Month') 
### high number during months: 05/07/06/08 ! Hypothesis : high temperature higher crimes 
## need to incorporate the weather data 
d6 = train[, .N, by =.(LSOA.name, Month)][,Percentage:=N/sum(N)*100]
g2 = ggplot(d6, aes(x = Month, y = LSOA.name, fill = Percentage)) + 
  geom_tile() +
  scale_fill_distiller(palette = 'RdYlGn') +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank())
ggplotly(g2)
#see whether certain months have more crime rate?
d7 = train[, .N, by =.(LSOA.name, Month)][,Percentage:=N/sum(N)*100, by = Month]

g3 = ggplot(d7, aes(x = Month, y = LSOA.name, fill = Percentage)) + 
  geom_tile() +
  geom_vline(xintercept=seq(0.5, 10, by=1), size = 1, color = 'grey') +
  scale_fill_distiller(palette = 'RdYlGn') + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank())
ggplotly(g3)
#whether on a given day, the crime rate differs across boroughs
d8 = train[, .N, by =.(LSOA.name, Month)][,Percentage:=N/sum(N)*100, by = LSOA.name]

g4 = ggplot(d8, aes(x = Month, y = LSOA.name, fill = Percentage)) + 
  geom_tile() +
  geom_hline(yintercept=seq(0.5, 10, by=1), size = 1, color = 'grey') + 
  scale_fill_distiller(palette = 'RdYlGn') +
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank())
ggplotly(g4)
## this plot just confirms during May to August crime rate is higher !
#Month-crime type relation
d9 = train[, .N, by =.(Crime.type, Month)][,Percentage:=N/sum(N)*100, by = Month][order(N, decreasing = T)]
g5 = ggplot(d9, aes(x = Month, y = Crime.type, fill = Percentage)) + 
  geom_tile() +
  geom_vline(xintercept=seq(0.5, 10, by=1), size = 1, color = 'grey') + 
  scale_fill_distiller(palette = 'RdYlGn') + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank())
ggplotly(g5)
## ASB is higher during august and april / Other crime is low between september and december / other types are constant during the whole year / drungs + thefts + shoplifting + criminal damage and weapons exist only between september and december !
d10 = train[, .N, by =.(Crime.type, Month)][,Percentage:=N/sum(N)*100, by = Crime.type][order(N, decreasing = T)]

g6 = ggplot(d, aes(x = Month, y = Crime.type, fill = Percentage)) + 
  geom_tile() +
  geom_hline(yintercept=seq(0.5, 40, by=1), size = 1, color = 'grey') + 
  scale_fill_distiller(palette = 'RdYlGn') + 
  theme(axis.title.y = element_blank(), 
        axis.title.x = element_blank())
ggplotly(g6)
