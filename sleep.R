library(tidyverse)
library(lubridate)

df <- read.csv("Box/myData/mi3_sleep.csv", header = T, stringsAsFactors = F)
df <- df[complete.cases(df), ]

df$Date <- mdy(df$Date)
df$Time_Bed <- hm(df$Time_Bed)
df$Time_Wake <- hm(df$Time_Wake)
df$Total_Length <- hm(df$Total_Length)
df$Deep_Length <- hm(df$Deep_Length)
df$Shallow_Length <- hm(df$Shallow_Length)

df$weekday <- wday(df$Date)

ggplot(df, aes(x = Date, y = Score)) +
  geom_line(color = "tomato") +
  scale_x_date(date_breaks = "1 weeks") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggplot(df, aes(x = Date, y = Total_Length)) +
  geom_line(color = "tomato") +
  geom_hline(aes(yintercept = hm("8:00")), lty = 2) +
  scale_x_date(date_breaks = "1 weeks") +
  scale_y_time() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggplot(df, aes(x = Date, y = Deep_Length, color = "Deep")) +
  geom_line() +
  geom_line(aes(y = Shallow_Length, color = "Light")) +
  scale_color_discrete(name = "Sleep Phase") +
  scale_x_date(date_breaks = "1 weeks") +
  scale_y_time() +
  labs(y = "Length") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "bottom")

ggplot(df, aes(x = Date, y = Time_Bed)) +
  geom_line(color = "tomato") +
  geom_hline(aes(yintercept = hm("24:00")), lty = 2) +
  scale_x_date(date_breaks = "1 weeks") +
  scale_y_time() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggplot(df, aes(x = Date, y = Time_Wake)) +
  geom_line(color = "tomato") +
  geom_hline(aes(yintercept = hm("6:39")), lty = 2) +
  scale_x_date(date_breaks = "1 weeks") +
  scale_y_time() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

df1 <- df %>% 
  mutate(Total_Length = as.duration(Total_Length))

wkday <- df1 %>% 
  group_by(weekday) %>% 
  summarise(meantime = mean(Total_Length))

ggplot(wkday, aes(x = weekday, y = meantime)) +
  geom_col(fill = "tomato") +
  geom_hline(aes(yintercept = hm("8:00")), lty = 2) +
  scale_y_time() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggplot(df, aes(x = Total_Length, y = Score)) +
  geom_point() +
  labs(x = "Total Length") +
  scale_x_time() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggplot(df, aes(x = Deep_Length, y = Score)) +
  geom_point() +
  labs(x = "Deep Length") +
  scale_x_time() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggplot(df, aes(x = Shallow_Length, y = Score)) +
  geom_point() +
  labs(x = "Light Length") +
  scale_x_time() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

ggplot(df, aes(x = Shallow_Length, y = Deep_Length)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Light Length", y = "Deep Length") +
  scale_x_time() +
  scale_y_time() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

