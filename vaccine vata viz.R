library(tidyverse)
library(cowplot)

df <- read.table('clipboard', header=T, sep = '\t')

b<- df %>% 
  ggplot(aes(Total, Type, fill=Day))+
  geom_bar(stat='identity', width = .2, show.legend = F)+
  theme_bw()+
  geom_segment(x=0, xend=0, y=.87, yend=1.12, size=8, colour='tomato')+#handle
  geom_segment(x=30, xend=30, y=.87, yend=1.12, size=5, colour='tomato')+#handle
  geom_segment(x=128.6, xend=128.6, y=.93, yend=1.07, size=8, colour='tomato')+#top
  geom_segment(x=30, xend=127.90, y=.90, yend=.90, size=2, colour='tomato')+#bottm
  geom_segment(x=30, xend=127.90, y=1.1, yend=1.1, size=2, colour='tomato')+#top
  geom_segment(x=127.5, xend=127.5, y=.90, yend=1, size=2.5, colour='tomato')+#edge
  geom_segment(x=127.5, xend=127.5, y=1, yend=1.1, size=2.5, colour='tomato')+#edge
  geom_segment(x=127.5, xend=165, y=1, yend=1, size=3.5, colour='tomato')+#cyringe
  scale_x_continuous(limits = c(0, 170))+
  annotate('rect', xmin=0.1, xmax=29.9, ymin=.95, ymax=1.05, fill='tomato')+
  scale_fill_manual(values = c('white', '#0c113b', '#183e76', '#1e4d94', '#2a6dcf', '#4480d9', '#6294df', '#9dbcef', 'white'))+
  annotate('text', x=35, y=.8, label='Day 1', size = 6, colour='blue')+
  annotate('text', x=122, y=.8, label='Day 7', size = 6, colour='blue')+
  annotate('text', x=81, y=1.2, 
           label='Proportion of eligible children population vaccinated by day', size = 6.5, colour='blue')+
  geom_segment(x=45, xend=112, y= .8, yend=.8, arrow = arrow(), size=2, colour='blue')+
  theme_void()

a <- df %>% 
  drop_na(day1) %>% 
  ggplot(aes(day1, per))+
  geom_line(size=1.5, colour='tomato')+
  geom_point(size=5, colour='tomato')+
  theme_classic()+
  geom_text(aes(label=per), vjust=-3, size=5, colour='tomato')+
  scale_y_continuous(limits = c(15, 110), breaks = seq(0,110,20))+
  scale_x_continuous(limits = c(1,7), breaks = seq(1,7, 1))+
  labs(y='percent',
       x='day')+
  geom_curve(x=3, xend=3.9, y= 105, yend=100, arrow = arrow(), curvature = -.3, size=1.5, colour='blue')+
  annotate('text', x=2, y=105, label='By the end of fourth day \n Bhutan vaccinated more \n80% of children', size=7)+
  annotate('text', x=6, y=50, label='Bhutan is among the \n few countires in the world \n to have vaccinated more than \n 97% of its eligble population ', size=7)
  theme(axis.text = element_text(size=18),
        axis.title = element_text(size = 23))
  

plot_grid(a, b, rel_heights = c(1.5, .7), nrow = 2, rel_widths = c(2, 1))
