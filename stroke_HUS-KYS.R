### Neurocenter Stroke Infograph ###

### Load data ----
if(TRUE){
  library(dplyr)
  library(tidyverse)
  # HUS
  d1 <- readr::read_csv2("data/stroke_EAKR.csv")
  d1$age_class <- factor(d1$age_class)
  d1$patients <- as.numeric(d1$patients)
  d1 %>% rename(year = Year, hospital = Hospital, stroke_class = Stroke_class) -> d1
  d1$stroke_class <- factor(d1$stroke_class, 
                            labels = c("Aneurysmaattinen \n lukinkalvonalainen \n verenvuoto", 
                                       "Verenvuoto \n aivokudokseen", 
                                       "Aivoinfarkti\n / TIA", 
                                       "Muut \n aivoverisuonisairaudet"))
  # KYS
  d2 <- readr::read_csv2("data/stroke_summary_no_gender.csv")
  d2$age_class <- factor(d2$age_class)
  d2$patients[d2$patients=="<10"] <- "10"
  d2$patients <- as.numeric(d2$patients)
  d2$stroke_class <- factor(d2$stroke_class, 
                            labels = c("Aneurysmaattinen \n lukinkalvonalainen \n verenvuoto", 
                                       "Verenvuoto \n aivokudokseen", 
                                       "Aivoinfarkti\n / TIA", 
                                       "Muut \n aivoverisuonisairaudet"))
  
  
  d <- rbind(d1, d2)
  rm(list = c("d1", "d2"))
}

### Color codes and fonts -----
library(extrafont)
font_import() # Import all fonts
fonts() # Print list of all fonts
if(TRUE){
  colors_hospital <- c("HUS" = "#02adb1", "KYS" = "#3ea9f5")
  text_black <- "#000000"
  text_white <- "#FFFFFF"
  normal_blue <- "#4936FF"
  deep_violet <-  "#29015F"
  indigo <- "#4F00A5"
  cool_grey <-  "#EAEAEA"
  highlight_green <- "#6BFF6F"
  light_blue <- "#37FFFB"
  sky_blue <- "#40C3FF"
  myfont <- "Impact"
}

# Configure Themes ----
library(hrbrthemes)
mycolor_theme <- function() {
  theme(
    plot.background = element_rect(fill = deep_violet, colour = deep_violet),
    panel.background = element_rect(fill = deep_violet),
    # panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = text_white, family = "Impact"),
    plot.title = element_text(colour = text_white, face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = text_white, face = "bold", size = 13, family = "Impact"),
    panel.grid.major.x = element_line(colour = text_white),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = normal_blue),
    axis.ticks = element_line(colour = text_white)
  )
}
mycolor_theme2 <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Impact", colour = text_white, size = 10),
    legend.background = element_rect(fill = deep_violet),
    legend.key = element_rect(fill = deep_violet, colour = deep_violet),
    legend.text = element_text(family = "Impact", colour = text_white, size = 10),
    plot.background = element_rect(fill = deep_violet, colour = deep_violet),
    panel.background = element_rect(fill = deep_violet),
    # panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = text_white, family = "Impact"),
    plot.title = element_text(colour = text_white, face = "bold", size = 18, vjust = 1, family = "Impact"),
    axis.title = element_text(colour = text_white, face = "bold", size = 13, family = "Impact"),
    panel.grid.major.y = element_line(colour = text_white),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Impact", colour = "white"),
    strip.background = element_rect(fill = normal_blue),
    axis.ticks = element_line(colour = text_white)
  )
}

# Graphs -----

#### PLOT0: Cases in 10 years -----
d1 <- d %>%
  dplyr::group_by(hospital) %>%
  dplyr::summarise(cases = round(sum(patients), -2)) 
p0 <- ggplot(data = d1[order(d1$cases),], aes(x="", y=cases, fill=hospital)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_x_discrete(breaks=NULL) +
  scale_fill_manual(values = colors_hospital) +
  geom_text(aes(label=paste0(hospital, "\n", cases)), 
            position = position_stack(vjust = 0.5),
            color = "white",
            size=10) +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot")+
  labs(x="", y="", title="Tapausten lukumäärä", ) 
p0 <- p0 + mycolor_theme() + guides(fill = FALSE)
p0

#### PLOT0B: trendi -----
d1 <- d %>%
  filter(hospital == "KYS") %>% 
  dplyr::group_by(year, hospital) %>%
  dplyr::summarise(patients = round(sum(patients), -1)) 
d2 <- d %>%
  filter(hospital == "HUS") %>% 
  dplyr::group_by(year, hospital) %>%
  dplyr::summarise(patients = round(sum(patients), -1)) 
dplot <- rbind(d1, d2)

p0b <- ggplot(data = dplot, aes(x=year, y=patients, fill = hospital)) +
  geom_bar(stat="identity", position = "dodge") +
  geom_text(aes(label = patients), vjust=1, position=position_dodge(width=0.9), color = "white") +
  scale_x_continuous(n.breaks = 10) +
  scale_fill_manual(values = colors_hospital) +
  labs(x="Vuosi", y = "", title = "Potilaat") + mycolor_theme() + guides(fill = FALSE) +
  guides(fill = guide_legend(title = "" )) +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot",
        legend.position = "top",
        legend.background = element_rect(fill=deep_violet),
        legend.title = element_text(colour = "white", size = 10, face = "bold"),
        legend.text = element_text(colour = "white", size = 10)) #NEW parameter
p0b


#### PLOT1: Patients by timeline -----
d1 <- d %>% 
  dplyr::group_by(year, stroke_class) %>% 
  dplyr::summarise(n=sum(patients))
p1 <- ggplot(data = d1) + 
  geom_line(aes(x=year, y=n), color = highlight_green) +
  scale_x_continuous(breaks=seq(2010,2019,2)) +
  labs(y = "", x="vuosi", title = "", legend="") +
  facet_grid(. ~ stroke_class)
p1 <- p1 + mycolor_theme2()
p1

#### PLOT1B: Patients by timeline per hospital and classes-----
d1 <- d %>% 
  filter(hospital=="KYS") %>% 
  dplyr::group_by(year, stroke_class,hospital) %>% 
  dplyr::summarise(n=sum(patients))
d2 <- d %>% 
  filter(hospital=="HUS") %>% 
  dplyr::group_by(year, stroke_class,hospital) %>% 
  dplyr::summarise(n=sum(patients))
dplot <- rbind(d1,d2)

p1b <- ggplot(data = dplot) + 
  geom_line(aes(x=year, y=n, color = hospital)) +
  scale_x_continuous(breaks=seq(2010,2019,2)) +
  scale_fill_manual(values = colors_hospital) +
  labs(y = "", x="vuosi", title = "", legend="") +
  facet_grid(rows = vars(hospital), cols =  vars(stroke_class)) + 
  mycolor_theme2()
p1b


#### PLOT1C: Patients timeline per hospital -----
p1c <- ggplot(data = dplot) + 
  geom_line(aes(x=year, y=n, color = hospital), size = 1) +
  scale_x_continuous(breaks=seq(2010,2019,2)) +
  scale_color_manual(values = colors_hospital) +
  labs(y = "", x="vuosi", title = "", legend="") +
  facet_grid(cols =  vars(stroke_class)) +
  guides(color=FALSE)+
  mycolor_theme2()
p1c



#### PLOT2: Stroke classes -----
d %>% 
  group_by(stroke_class) %>% 
  summarise(patients = sum(patients))

d2 <- d %>% 
  filter(hospital == "KYS") %>% 
  group_by(stroke_class) %>% 
  summarise(patients = sum(patients)) %>% 
  mutate(percentage = round(100 * patients / sum(patients)),
         hospital = "KYS")
d2$stroke_class <- factor(d2$stroke_class ,levels = d2$stroke_class[order(d2$percentage, decreasing = FALSE)] )
d1 <- d %>% 
  filter(hospital == "HUS") %>% 
  group_by(stroke_class) %>% 
  summarise(patients = sum(patients)) %>% 
  mutate(percentage = round(100 * patients / sum(patients)),
         hospital = "HUS")
dplot <- rbind(d1,d2)

p2 <- ggplot(data = dplot[order(dplot$percentage),], aes(x = stroke_class, y = percentage, fill = hospital)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0,100,10), limits = c(0, 80)) +
  scale_fill_manual(values = colors_hospital) +
  # geom_text(aes(label = paste(percentage, "")), nudge_y = 3, color = "white") +
  labs(x="", y = "Osuus (%)", title = "Osuudet luokittain") +
  guides(fill=FALSE)
  # guides(fill = guide_legend(title = "" )) +
  # theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
  #       plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
  #       plot.caption.position =  "plot",
  #       legend.position = "bottom",
  #       legend.background = element_rect(fill=deep_violet),
  #       legend.title = element_text(colour = "white", size = 10, face = "bold"),
  #       legend.text = element_text(colour = "white", size = 10)) #NEW parameter
p2 <- p2 + mycolor_theme() 
p2

#### PLOT3: Total Age distribution ----
p3 <- ggplot(data = d, aes(x=age_class, y=patients)) +
  geom_bar(stat = "identity", fill = highlight_green) +
  # coord_polar() +
  # facet_grid(. ~ Stroke_class) +
  ylab("") +
  xlab("") +
  ggtitle("Age distribution")
p3 <- p3 + mycolor_theme2()
p3


#### PLOT4: Age distribution per classs ----
### Hospitals combined
d %>% 
  group_by(stroke_class, age_class) %>% 
  summarise(patients = sum(patients)) %>% 
  group_by(stroke_class) %>% 
  mutate(n = sum(patients),
         pct = 100 * patients / n) -> d3

p4 <- ggplot(data = d3, aes(x=age_class, y=pct)) +
  geom_bar(stat = "identity", fill = highlight_green) + 
  # coord_polar() + 
  facet_grid(. ~ stroke_class) +
  ylab("") + 
  xlab("") + 
  ggtitle("Ikäjakauma (%)") + mycolor_theme2()
p4

# #### PLOT4b: Age distribution per classs and hospital classes----
# ### Hospitals combined
# d %>% 
#   group_by(stroke_class, age_class, hospital) %>% 
#   summarise(patients = sum(patients)) %>% 
#   group_by(stroke_class) %>% 
#   mutate(n = sum(patients),
#          pct = 100 * patients / n) -> d3
# 
# p4b <- ggplot(data = d3, aes(x=age_class, y=pct, fill= hospital)) +
#   geom_bar(stat = "identity", position = "dodge") + 
#   scale_fill_manual(values = colors_hospital) +
#   facet_grid(. ~ stroke_class) +
#   ylab("") + 
#   xlab("") + 
#   ggtitle("Ikäjakauma (%)") + mycolor_theme2()
# p4b

#### PLOT5: Age distribution per class and hospital ----
d %>%
  filter(hospital == "HUS") %>%
  group_by(stroke_class, age_class) %>%
  summarise(patients = sum(patients)) %>%
  group_by(stroke_class) %>%
  mutate(n = sum(patients),
         pct = 100 * patients / n,
         hospital = "HUS") -> d3a
d %>%
  filter(hospital == "KYS") %>%
  group_by(stroke_class, age_class) %>%
  summarise(patients = sum(patients)) %>%
  group_by(stroke_class) %>%
  mutate(n = sum(patients),
         pct = 100 * patients / n,
         hospital = "KYS") -> d3b
d3 <- rbind(d3a, d3b)

p5 <- ggplot(data = d3, aes(x=age_class, y=pct, fill= hospital)) +
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = colors_hospital) +
  facet_grid(. ~ stroke_class) +
  ylab("") + 
  xlab("") + 
  ggtitle("Ikäjakauma (%)") +
  guides(fill = guide_legend(title = "" )) +
  theme(plot.caption = element_text(hjust = 0, face= "italic"), #Default is hjust=1
        plot.title.position = "plot", #NEW parameter. Apply for subtitle too.
        plot.caption.position =  "plot",
        legend.position = "bottom",
        legend.background = element_rect(fill=deep_violet),
        legend.title = element_text(colour = "white", size = 10, face = "bold"),
        legend.text = element_text(colour = "white", size = 10)) + 
  mycolor_theme2()
p5


#### PLOT6: Trend per hospital-----
d1 <- d %>% 
  filter(hospital == "HUS") %>% 
  dplyr::group_by(year, hospital) %>% 
  dplyr::summarise(n=sum(patients))
d2 <- d %>% 
  filter(hospital == "KYS") %>% 
  dplyr::group_by(year, hospital) %>% 
  dplyr::summarise(n=sum(patients))
dplot <- rbind(d1,d2)

p6 <- ggplot(data = dplot) + 
  geom_line(aes(x=year, y=n, group=hospital, color = hospital)) +
  scale_x_continuous(breaks=seq(2010,2019,2)) +
  scale_fill_manual(values = colors_hospital) +
  labs(y = "", x="vuosi", title = "", legend="") 
p6 <- p6 + mycolor_theme2()
p6



# Generate Infographic in PNG/PDF Format ----
library(grid)
library(useful)
filename <- "./infograph_stroke.png"
if(TRUE){
  png(filename = filename, width = 10, height = 20, units = "in", res = 500)
  # pdf(filename, width = 10, height = 20)
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(5, 3)))
  grid.rect(gp = gpar(fill = deep_violet, col = deep_violet))
  grid.text("Stroke HUS & KYS-alueilla \n vuosina 2010-2019", 
            y = unit(1, "npc"), 
            x = unit(0.5, "npc"), 
            vjust = 1.1, 
            hjust = .5, 
            gp = gpar(fontfamily = "Impact", 
                      col = "white", 
                      cex = 5, 
                      alpha = 1)
  )
  # grid.text("Neurocenter Finland", 
  #           y = unit(0.94, "npc"), 
  #           gp = gpar(fontfamily = "Impact", 
  #                     col = normal_blue, 
  #                     cex = 6.4,
  #                     alpha = 0.3))
  # grid.text("BY J. Miettinen", vjust = 0, y = unit(0.92, "npc"), gp = gpar(fontfamily = "Impact", col = text_white, cex = 0.8))
  # grid.text("CUSTOM TITLE", vjust = 0, y = unit(0.913, "npc"), gp = gpar(fontfamily = "Impact", col = text_white, cex = 0.8))
  # grid.text("www.neurocenterfinland.fi", vjust = 0, y = unit(0.906, "npc"), gp = gpar(fontfamily = "Impact", col = text_white, cex = 0.8))
  print(p0b, vp = vplayout(2, 1:3))
  print(p2, vp = vplayout(3, 1:3))
  print(p1c, vp = vplayout(4, 1:3))
  print(p5, vp = vplayout(5, 1:3))
  
  # grid.text("Tapaukset", 
  #           vjust = 0, 
  #           y = unit(0.79, "npc"), 
  #           gp = gpar(fontfamily = "Impact", 
  #                     col = text_white, 
  #                     cex = 12) )
  
  # grid.rect(gp = gpar(fill = deep_violet, col = deep_violet), 
  #           x = unit(0.5, "npc"), 
  #           y = unit(0.82, "npc"), 
  #           width = unit(1, "npc"), 
  #           height = unit(0.11, "npc"))
  
  grid.text("Infograafi", 
            y = unit(0.845, "npc"),
            x = unit(0.5, "npc"),
            vjust = .5,
            hjust = .5,
            gp = gpar(fontfamily = "Impact",
                      col = cool_grey, cex = 13, alpha = 0.25)
            )
  
  grid.text("Infograafi stroke tapauksista KYS:n ja HUS:n alueilla vuosien 2010-2019 aikana. Stroke tapaukset on lajiteltu neljään luokkaan:\n aneurysmaattinen lukinkalvonalainen verenvuoto, verenvuoto aivokudokseen, aivoinfarkti/TIA ja muut aivoverisuonisairaudet.",
            vjust = 1, 
            hjust = 0, 
            x = unit(0.01, "npc"), 
            y = unit(0.88, "npc"), 
            gp = gpar(fontfamily = "Impact", col = text_white, cex = 1.1))
  
  # grid.text("Kuvaus", 
  #           vjust = 0, 
  #           hjust = 0, 
  #           x = unit(0.01, "npc"), 
  #           y = unit(0.86, "npc"), 
  #           gp = gpar(fontfamily = "Impact", col = "white", cex = 1.2))
  
  grid.text(paste(
    "Data:", 
    "Lisätietoa:",
    "Lähdekoodi:",
    "Tekijät:", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.807, "npc"), gp = gpar(fontfamily = "Impact", col = text_white, cex = 0.8))
  
  grid.text(paste(
    "Stroke-tapausten potilaat, jotka hakeutuneet päivystykseen vuosilta 2010-2019",
    "http://www.neurocenterfinland.fi",
    "http://github.com/",
    "Paula Tanni (HUS), Eric Le Tortorec (KYS) & Jani Miettinen (Neurocenter Finland)", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.807, "npc"), gp = gpar(fontfamily = "Impact", col = text_white, cex = 0.8))
  dev.off()
}
