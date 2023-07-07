# setwd("C:/Users/Irvine Udinge/OneDrive/R/Trades")

#install.packages('dplyr')
library(dplyr)
# install.packages("ggplot2")
library(ggplot2)
library(readxl)

#all you should do is change the name of the file below, then run the entire code
File_Name <- 
  "eToroAccountStatement - IrvineUdinge - 01-09-2020 - 16-01-2021.xlsx"

Trades <- read_excel(File_Name, sheet = "Closed Positions") %>%
  mutate(
    Profit = as.numeric(Profit),
    Amount = as.numeric(Amount),
    `Close Date` = as.POSIXct(`Close Date`, format = "%d/%m/%Y %H:%M"),
    `Open Date` = as.POSIXct(`Open Date`, format = "%d/%m/%Y %H:%M"),
    Hrs_Open = as.double(`Close Date` - `Open Date`) / 60 / 60,
    Leverage = as.factor(as.numeric(ifelse(
      is.na(Leverage), 1, Leverage
    )))
  )


Sum_Trade <- Trades %>%
  group_by(Action) %>%
  summarise(Profit = sum(Profit)) %>%
  ggplot(aes(x = Action, y = Profit)) +
  geom_bar(stat = "identity", aes(reorder(Action, Profit))) +
  coord_flip() +
  theme_classic() + labs (title = "Profit/Loss by Action Since eToro Registration")
print(Sum_Trade)

Profit_Loss_USD_Hrs <- Trades %>%
  ggplot(aes(x = Hrs_Open, y = Profit)) +
  geom_point() +
  geom_abline(colour = "green") +
  theme_classic() + labs (title = "Relationship Between Realised Profit/Loss (in US$) and Length of Open Trade",
                          x = "Hours Open", y = "Profit Amount")
print(Profit_Loss_USD_Hrs)

Losses_USD_Hrs <- Trades %>%
  filter(Profit<=0)%>%
  ggplot(aes(x = Hrs_Open, y = Profit,colour=Leverage)) +
  geom_point() +
  geom_smooth(method = "lm",se=F) +
  theme_classic() + 
  labs (title = "Relationship Between Realised Profit/Loss (in US$) and Length of Open Trade",
                          x = "Hours Open", y = "Profit Amount")
print(Losses_USD_Hrs)

Profit_Rate_Hrs_Zm <- Trades %>%
  ggplot(aes(
    x = Hrs_Open,
    y = (Profit / Amount),
    colour = Leverage
  )) +
  geom_point(aes(alpha = .2)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() + labs (title = "Relationship Between Realised Profit/Loss (in %) and Length of Open Trade (log  10 axis)",
                          x = "Hours Open", y = "Profit Rate") +
  scale_y_log10() + scale_x_log10()
print(Profit_Rate_Hrs_Zm)

Profit_Rate_Hrs <- Trades %>%
  ggplot(aes(x = Hrs_Open, y = (Profit / Amount))) +
  geom_point(aes(alpha = .1)) +
  geom_smooth(method = "lm", se = F) +
  #theme_classic()+
  facet_grid( ~ Leverage) +
  labs (title = "Relationship Between Realised Profit/Loss (in %) and Length of Open Trade (faceted by leverage)",
        x = "Hours Open", y = "Profit Rate")
print(Profit_Rate_Hrs)

Profit_Rate_Hrs_Lv1 <- Trades %>%
  filter(Leverage == "1") %>%
  ggplot(aes(x = Hrs_Open, y = (Profit / Amount))) +
  geom_point(aes(alpha = .1)) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic() +
  labs (title = "Relationship Between Realised Profit/Loss (in %) and Length of Open Trade for Leverage of 1",
        x = "Hours Open", y = "Profit Rate")
print(Profit_Rate_Hrs_Lv1)

Transactions <-
  read_excel(File_Name, sheet = "Transactions Report") %>%
  mutate(Date = as.POSIXct(Date, format = "%Y-%m-%d %H:%M:%S"))

PointTransactions <- Transactions %>%
  filter(Type == "Deposit")

LinePointTransactions <- Transactions %>%
  ggplot(aes(x = Date, y = `Realized Equity`)) +
  geom_line(colour = "blue") +
  geom_point(data = PointTransactions) +
  theme_classic() +
  labs(title = "Realized Equity Over Time (black dots represent points at which deposits were made)")
print(LinePointTransactions)

RealisedLG <- Transactions %>%
  filter(Type == "Profit/Loss of Trade") %>%
  ggplot(aes(x = Date, y = cumsum(Amount))) +
  geom_line() +
  theme_classic() + labs(y = "Cumulative Realised Gains/Losses",
                         title = "Cumulative Realised Gains/Losses Over Time")
print(RealisedLG)