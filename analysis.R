library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(scales)
library(RColorBrewer)


# data cleaning ####

# download CT grant data at: 
# https://www.csde.state.ct.us/public/dgm/grantreports1/HPayMain.aspx
# select the following before submitting query:
# section a: all years
# section b: all grants
# section c: all
# section d: all grantees
# section 3: all drgs
# submit and download as .csv

# load data
edu.grants <- read.csv("data/histpay.csv")

# remove empty column
edu.grants$X <- NULL

# gather year columns into single variable
edu.grants <- edu.grants %>%
    gather(year, amount, pay1990:pay2014)

# clean year column
edu.grants$year <- as.numeric(str_replace_all(edu.grants$year,"pay",""))

# filter to show years 2006 and later.

edu.grants <- edu.grants %>%
    filter(year >= 2009)

# load resident student data
# data obtained from Connecticut State Department of Education
res.students <- read.csv("data/resStudents.csv", stringsAsFactors = FALSE)

# load alliance district info
# data obtained from Connecticut State Department of Education
alliance.dist <- read.csv("data/allianceDistricts.csv")

# exploratory analysis####

# select columns for ecs and stimulus funding
edu.grant.summary <- edu.grants %>%
    group_by(year, grant_name) %>%
    summarise(amount = sum(amount, na.rm = TRUE)) %>%
    filter(amount >0) %>%
    group_by(year) %>%
    arrange(desc(amount))

# plot summary 
ggplot(edu.grant.summary, aes(x = year, y = amount, color = grant_name))+
    geom_line()

# filter grants to ECS, AARA, special education, priority school district, common core and choice
edu.grant.select <- edu.grants %>%
    filter(grant_name == "ECS/Alliance Dist Grants" |
               grant_name == "ARRA Stabilization-Ed Grants"|
               grant_name == "Sp. Ed. - Excess Cost"|
               grant_name == "Priority School Districts"|
               grant_name == "Charter Schools"|
               grant_name == "High Qulty Schls & Cmn Core Implem"|
               grant_name == "Commissioner's Network"|
               grant_name == "Talent Development"|
               grant_name == "Education Jobs Fund"|
               grant_name == "Open Choice Program"|
               grant_name == "Interdistrict Cooperative"|
               grant_name == "Sheff Settlement"|
               grant_name == "Magnet School") 

# select grant summary 
edu.select.summary <- edu.grant.select %>%
    group_by(year, grant_name) %>%
    summarise(amount = sum(amount, na.rm = TRUE))

# plot selected grants
ggplot(edu.select.summary, aes(x = year, y = amount, fill = grant_name))+
    geom_bar(stat = "identity")

# final analysis####

# select ecs grants
ecs.grants <- edu.grants %>%
    filter(grant_name == "ECS/Alliance Dist Grants" |
               grant_name == "ARRA Stabilization-Ed Grants"|
               grant_name == "ARRA Stabilization-Gov Serv")

# summarise ecs grants
ecs.summary <- ecs.grants %>% 
    group_by(year, grant_name) %>%
    summarise(amount = sum(amount, na.rm = TRUE))

# plot ecs grants
ggplot(ecs.summary, aes(x = year, y = amount, fill = grant_name))+
    geom_bar(stat = "identity", aes(order=desc(grant_name)))+
    scale_y_continuous(label = dollar)+
    scale_fill_manual(values = c("springgreen3", "springgreen4", "steelblue2"),
                      name = "Grant Name")+
    labs(x = "", y = "")+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.text = element_text(size =8),
          axis.text.x = element_text(size = 10))

# save plot
ggsave(filename = "ecssummary.png", height = 5, width = 7, units= "in")

# summarise ecs by town
ecs.town <- ecs.grants %>%
    filter(grant_name == "ECS/Alliance Dist Grants", year >=2011)

# change year labels
ecs.town$year <- paste("ecs", ecs.town$year, sep = "")

# change col names
colnames(ecs.town) <- c("code", "name", "drg", "grant_name", "grant_type", "year", "amount")

# spread data
ecs.town.wide <- ecs.town %>%
    spread(year, amount)

# merge with resident student data
ecs.town.enrl <- left_join(ecs.town.wide, res.students, by = "code")

# muatate
ecs.town.enrl <- ecs.town.enrl %>%
    mutate(ecs.change1114 = ecs2014 - ecs2011,
           pp.change1114 = ecs.change1114 / fiscTotal, 
           frpl.pct = fiscFRPL/fiscTotal,
           pp.2014 = ecs2014 / fiscTotal)

# merge with alliance district indicator
ecs.town.enrl <- left_join(ecs.town.enrl, alliance.dist, by = "code")

# plot ecs change

ggplot(ecs.town.enrl, aes(x = reorder(town.x, -ecs.change1114), y = ecs.change1114, fill = alliance.status))+
    geom_bar(stat = "identity")+
    scale_y_continuous(label = dollar)+
    scale_fill_manual(values = c("steelblue2","springgreen3"))+
    coord_flip()+
    theme_bw()+
    theme(legend.position = "none",
          axis.text.x = element_text(angle = 45,hjust =1, vjust = 1, size = 10),
          axis.text.y = element_text(size = 8))

# save plot
ggsave(filename = "ecschange.png", height = 18, width = 7, units= "in")


# plot ecs per-pupil change

ggplot(ecs.town.enrl, aes(x = reorder(town.x, -pp.change1114), y = pp.change1114, fill = alliance.status))+
    geom_bar(stat = "identity")+
    scale_y_continuous(label = dollar)+
    scale_fill_manual(values = c("steelblue2","springgreen3"))+
    labs(y = "2011-2014 Change in Funding Per-Pupil", x = "")+
    coord_flip()+
    theme_bw()+
    theme(legend.position = "none",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 8))

# save plot
ggsave(filename = "perpupilchange.png", height = 18, width = 7, units= "in")

# plot ecs per-pupil change vs frpl

ggplot(ecs.town.enrl, aes(x = frpl.pct, y = pp.2014,size = fiscTotal, color = alliance.status))+
    scale_color_manual(values = c("steelblue4","springgreen3"))+
    geom_point(alpha = .6)+
    scale_size_area(max_size = 20)+
    scale_y_continuous(label = dollar)+
    scale_x_continuous(label = percent)+
    labs(x = "Percentage of FRPL Students", y = "2014 ECS Funding Per Pupil")+
    theme_bw()+
    theme(legend.position = "none",
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 8))