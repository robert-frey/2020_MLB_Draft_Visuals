library(tidyverse)
####Filter Data####
d_20 <- draft_2020 %>% filter(is.na(person_primary_position_abbreviation)==FALSE) %>%
  mutate(Pos = case_when(person_primary_position_abbreviation == "P" & person_pitch_hand_code == "L" ~ "LHP",
                         person_primary_position_abbreviation == "P" & person_pitch_hand_code == "R" ~ "RHP",
                         TRUE ~ person_primary_position_abbreviation),
         School_Type = case_when(str_detect(school_name,"\\bHS\\b")==TRUE ~ "HS",
                                 str_detect(school_name,"\\bInstitute\\b")==TRUE ~ "HS",
                                 str_detect(school_name,"\\bSS\\b")==TRUE ~ "HS",
                                 str_detect(school_name,"\\bAcademy\\b")==TRUE ~ "HS",
                                 TRUE ~ "College"),
         School_Class = case_when(str_detect(school_name,"\\bMcLennan\\b")==TRUE ~ "JC",
                                  str_detect(school_name,"\\bSan Jacinto\\b")==TRUE ~ "JC",
                                  str_detect(school_name,"\\bWabash Valley\\b")==TRUE ~ "JC",
                                  str_detect(school_name,"\\bNorthwest Florida State\\b")==TRUE ~ "JC",
                                  School_Type=="HS" ~ "HS",
                                  str_detect(school_name,"\\bLe Moyne College\\b")==TRUE ~ "D2",
                                  str_detect(school_name,"\\bBellarmine\\b")==TRUE ~ "D2",
                                  str_detect(school_name,"\\bUC San Diego\\b")==TRUE ~ "D2",
                                  str_detect(school_name,"\\bChapman\\b")==TRUE ~ "D3",
                                  TRUE ~ "D1")) %>% select(bis_player_id:school_name,School_Type,School_Class,everything())
####Position Plots####
ggplot(d_20,aes(x=reorder(Pos,Pos,
                          function(x)-length(x)))) +
  geom_bar(fill="red", stat="count") +
  geom_text(stat="count",aes(label=..count..), vjust = -1) +
  theme(element_blank()) +
  xlab("Position") +
  ylab("Number of Picks") +
  labs(title = "2020 Draft Picks by Position", caption = "Data: baseballr, Graph: @RobertFrey40")

####School Class####
ggplot(d_20,aes(x=reorder(School_Class,School_Class,
                          function(x)-length(x)))) +
  geom_bar(fill="red",stat="count") +
  geom_text(stat="count",aes(label=..count..), vjust = -1) +
  theme(element_blank()) +
  xlab("Class") +
  ylab("Number of Picks") +
  labs(title = "2020 Draft Picks by School Class", caption = "Data: baseballr, Graph: @RobertFrey40")
####Age####
ggplot(d_20,aes(x=reorder(as.factor(person_current_age),as.factor(person_current_age),
                          function(x)-length(x)))) +
  geom_bar(fill="blue",stat="count") +
  geom_text(stat="count",aes(label=..count..), vjust = -1) +
  theme(element_blank()) +
  xlab("Age") +
  ylab("Number of Picks") +
  labs(title = "2020 Draft Picks by Age", caption = "Data: baseballr, Graph: @RobertFrey40")

####Most Picks by School####
library(ncaahoopR)
colors = ncaa_colors %>% rename(school = ncaa_name)
colors = edit(colors)
colors = colors %>% select(school,primary_color,logo_url)

library(ggimage)

d_20_group = d_20 %>% group_by(school_name) %>% summarise(Total = n()) %>% arrange(-Total)
d_20_group = d_20_group %>% filter(Total >= 3) %>% left_join(colors,by=c("school_name"="school"))
ggplot(d_20_group,aes(x=reorder(school_name,-Total),
                      y=Total)) +
  geom_col(fill=d_20_group$primary_color) +
  geom_image(image = d_20_group$logo_url, asp = 16/9, nudge_y = 1) + 
  geom_text(aes(label=Total), vjust = -1) +
  theme(element_blank()) +
  ylim(c(0,7)) +
  xlab("School") +
  ylab("Number of Picks") +
  labs(title = "2020 Draft Picks by School", subtitle = "Schools with at least 3 picks", caption = "Data: baseballr, Graph: @RobertFrey40")
####Birth State Picks####
ggplot(d_20 %>% filter(is.na(person_birth_state_province)==FALSE),aes(x=reorder(person_birth_state_province,person_birth_state_province,
                                                                                function(x)-length(x)))) +
  geom_bar(fill="blue",stat="count") +
  geom_text(stat="count",aes(label=..count..), vjust = -1) +
  theme(element_blank()) +
  xlab("State") +
  ylab("Number of Picks") +
  labs(title = "2020 Draft Picks by Birth State/Province", caption = "Data: baseballr, Graph: @RobertFrey40")
####Height Picks####
ggplot(d_20,aes(x=reorder(person_height,person_height,
                          function(x)-length(x)))) +
  geom_bar(fill="blue",stat="count") +
  geom_text(stat="count",aes(label=..count..), vjust = -1) +
  theme(element_blank()) +
  xlab("Height") +
  ylab("Number of Picks") +
  labs(title = "2020 Draft Picks by Height", caption = "Data: baseballr, Graph: @RobertFrey40")
####Players Who Had Been Drafted Previously####
d_20_d = d_20 %>% mutate(Drafted = ifelse(nchar(person_draft_year)==4,"Y","N"))
ggplot(d_20_d,aes(x=reorder(Drafted, Drafted,
                            function(x)-length(x)))) +
  geom_bar(fill="blue",stat="count") +
  geom_text(stat="count",aes(label=..count..), vjust = -1) +
  theme(element_blank()) +
  xlab("Previously Drafted") +
  ylab("Number of Picks") +
  labs(title = "2020 Draft Picks That Had Been Drafted Previously", caption = "Data: baseballr, Graph: @RobertFrey40")

####MLB Team Breakdown HS v College####
team_school_type <- function(df,team) {
  ggplot(df %>% filter(team_name == team),aes(x=reorder(School_Type,School_Type,
                                                        function(x)-length(x)))) +
    geom_bar(aes(fill=School_Type),stat="count") +
    geom_text(stat="count",aes(label=..count..), vjust = -1) +
    theme(element_blank(),legend.position = "none") +
    xlab("Class") +
    ylab("Number of Picks") +
    labs(title = "2020 Draft Picks by School Class Type", subtitle = paste0(team," Selections"),caption = "Data: baseballr, Graph: @RobertFrey40")
}  

team_school_type(d_20,"St. Louis Cardinals")

####MLB Team Breakdown by School Division####
team_school_class <- function(df, team) {
  ggplot(df %>% filter(team_name == team),aes(x=reorder(School_Class,School_Class,
                                                        function(x)-length(x)))) +
    geom_bar(aes(fill=School_Class),stat="count") +
    geom_text(stat="count",aes(label=..count..), vjust = -1) +
    theme(element_blank(), legend.position = 'none') +
    xlab("Class") +
    ylab("Number of Picks") +
    labs(title = "2020 Draft Picks by School Class", subtitle = paste0(team," Selections"),caption = "Data: baseballr, Graph: @RobertFrey40")
} 

team_school_class(d_20,"Chicago Cubs")

####MLB Team Breakdown by Position####
team_position <- function(df, team) {
  ggplot(df %>% filter(team_name == team),aes(x=reorder(Pos,Pos,
                                                        function(x)-length(x)))) +
    geom_bar(aes(fill=Pos),stat="count") +
    geom_text(stat="count",aes(label=..count..), vjust = -1) +
    theme(element_blank(), legend.position = 'none') +
    xlab("Position") +
    ylab("Number of Picks") +
    labs(title = "2020 Draft Picks by Position", subtitle = paste0(team," Selections"),caption = "Data: baseballr, Graph: @RobertFrey40")
}

team_position(d_20,"San Francisco Giants")