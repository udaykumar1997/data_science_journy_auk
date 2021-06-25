########################################################################
## exercise: Properties of Stars									  ##
########################################################################

library(tidyverse)
library(dslabs)
library(ggrepel)
data(stars)
options(digits = 3)   # report 3 significant digits

str(stars)

mean(stars$magnitude)
sd(stars$magnitude)

ggplot(data=stars,aes(magnitude))+geom_density()

ggplot(data=stars,aes(temp))+geom_density()

ggplot(data=stars,aes(temp,magnitude))+geom_point()
ggplot(data=stars,aes(temp,magnitude))+geom_point()+scale_y_reverse()
ggplot(data=stars,aes(temp,magnitude))+geom_point()+scale_y_reverse()+scale_x_reverse()+scale_x_log10()

ggplot(data=stars,aes(temp,magnitude,label=star))+geom_point()+scale_y_reverse()+geom_text_repel()
+scale_x_reverse()
+ geom_text(aes(label = country, hjust = 1))
+geom_text(aes(x = location, label = country, hjust = 0), show.legend = FALSE)

stars %>% filter(temp>5000 & magnitude>0) %>%  ggplot(aes(temp,magnitude,label=star)) +
  geom_point() + scale_y_reverse()+ geom_text_repel()

stars %>% filter(temp<10000 & magnitude<0) %>%  ggplot(aes(temp,magnitude,label=star)) +
  geom_point() + scale_y_reverse()+ geom_text_repel()

stars %>%  ggplot(aes(temp,magnitude,col=type)) +
  geom_point() + scale_y_reverse()+ geom_text_repel()