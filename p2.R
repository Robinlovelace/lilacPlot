# Plot 2 - Income

d <- read.csv("p2.csv")[1:20,]
sapply(d, class)
sapply(d, mode)
names(d)
attach(d)

d$MDR <- MDR + ADR + LEF + HC
d$ADR <- ADR + LEF + HC
d$LEF <- LEF + HC

# % spent on hc
d$MDR / TAI

d[2:6] <- sapply(d[2:6], sqrt)

ggplot(d, aes(x = House)) + 
  geom_bar(aes(y = TAI), stat = "identity", width = 1, 
           fill = "white", linetype = 2, color = "black") +
  geom_bar(aes(y = MDR), stat = "identity", 
           fill = "grey20", width = 1, color = "black") +
  geom_bar(aes(y = ADR), stat = "identity", 
           fill = "grey40", width = 1, color = "black") +
  geom_bar(aes(y = LEF), stat = "identity", 
           fill = "grey60", width = 1, color = "black") +
  geom_bar(aes(y = HC), stat = "identity", 
           fill = "white", width = 1, color = "black") +
  coord_polar()
  
  

mean(d$H)

qplot(x = 1:length(d$HC), y = d$HC, geom = "bar", stat = "identity")
ggplot(d, aes(x = House)) + geom_bar(aes(y = HC), stat = "identity")

tp <- function(x){
  qplot(x = 1:length(x), y = x, geom = "bar", stat = "identity", width = 1)
}

tp(d$HC)


