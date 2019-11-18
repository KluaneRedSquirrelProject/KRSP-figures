# Code for plot of first taggings of each year
# McAdam

# Plot is a time series 

# Bring in Data
library(tidyverse)
library(krsp)
library (lubridate)

# Create Connection using krsp_connect using the AWS instance of the KRSP database
connection_1<-krsp_connect (host = "krsp.cepb5cjvqban.us-east-2.rds.amazonaws.com",
                     dbname ="krsp",
                     username = Sys.getenv("krsp_user"),
                     password = Sys.getenv("krsp_password")
)

#Importing midden census data
tbl(connection_1, "litter") %>% 
  filter(grid %in% c("SU", "KL"),
         yr > 1988) %>% 
  select(year=yr, tagDt) %>% 
  collect() %>% 
  mutate(tagDt = yday(ymd(tagDt))) %>% 
  group_by(year) %>% 
  summarize(first_tags = min(tagDt, na.rm=T)) %>% 
  rbind(c(2019, yday("2019-07-03"))) %>% 
# Create plot
  ggplot(aes(x=year, y=first_tags)) +
  theme(
    axis.text=element_text(size=14),
    axis.title=element_text(size=18), 
    plot.title=element_blank(),
    plot.margin = unit(c(1,1,1,1), "cm"),
    panel.border = element_blank(), 
    axis.line = element_line(),
    panel.background = element_rect(fill = "white"), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()) + 
  geom_vline(xintercept = 1993, colour="light grey", size=5) +
  geom_vline(xintercept = 1998, colour="light grey", size=5) +
  geom_vline(xintercept = 2005, colour="light grey", size=5) +
  geom_vline(xintercept = 2010, colour="light grey", size=5) +
  geom_vline(xintercept = 2014, colour="light grey", size=5) +
  geom_vline(xintercept = 2019, colour="light grey", size=5) +
  geom_hline(yintercept = yday("2019-03-01")) +
  geom_hline(yintercept = yday("2019-07-01")) +
  annotate("text", x = 1990, y = yday("2019-03-06"), label = "March 1st") + 
  annotate("text", x = 1990, y = yday("2019-07-06"), label = "July 1st") + 
  geom_line() +
  geom_point(size = 5, color = "blue") + 
  scale_x_continuous(breaks=c(1993, 1998, 2005, 2010, 2014, 2019)) +
  #scale_x_continuous(breaks=seq(1989,2020, 4)) +
  scale_y_continuous(limits=c(50,210)) +
  ylab("Date first litter was tagged") +
  xlab("Year") + 
# Save plot
  ggsave(filename="first tags.jpg", plot=last_plot())

