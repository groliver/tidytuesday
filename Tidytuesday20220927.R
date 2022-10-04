library(tidyverse)
library(readr)
# Get the Data

#  read in the data manually

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-27/artists.csv')

#eda 
artists  %>%
  group_by(type,state) %>%
  summarise( total= sum(artists_share)) %>%
  ggplot(aes(x=total,color=state,fill=state)) + geom_histogram()+
  scale_x_log10(labels = scales::label_percent()) +
  facet_wrap(.~type) +
  ggthemes::theme_economist( )



artists  %>%
  group_by(type,state) %>%
  summarise( total= sum(artists_share)) %>%
  pivot_wider(id_cols= state ,names_from=type, values_from=total )
  ggplot(aes(x=total,color=state,fill=state)) + geom_histogram()+
  scale_x_log10(labels = scales::label_percent()) +
  facet_wrap(.~type) +
  ggthemes::theme_economist( )
  
  
  artists  %>%
    group_by(type,state) %>%
    summarise( total= sum(artists_share,na.rm=T)) %>%
    pivot_wider(id_cols= state ,names_from=type, values_from=total ) %>% 
    ungroup %>%
    #names
    ggplot(aes(x=Musicians, y=Architects,col=state,label=state)) + geom_label()+
    ggthemes::theme_economist( )+
    guides(color = FALSE)
  
  #cluster with hierarchical cluster highlighting 4 groups with rects
  artists  %>%
    group_by(type,state) %>%
    summarise( total= sum(artists_share,na.rm=T)) %>%
    pivot_wider(id_cols= state ,names_from=type, values_from=total ) %>% 
    ungroup %>%
    #names
    select(!state) %>%
  cor(use = 'everything') %>% 
    corrplot::corrplot( method = 'number', , order = 'hclust', addrect = 4)
  