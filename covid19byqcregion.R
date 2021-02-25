library(tidyverse)
library(lubridate)
library(pals)
library(scales)
library(extrafont)
library(forcats)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(gridtext)
library(zoo)
library(Cairo)

CairoWin()

# Just for having cool fonts to use for our viz

font_import() # I think it's only useful to do it once in your project, not sure
loadfonts(device="win")
fonts()
fonts <- fonttable()
windowsFonts()

# On remplace les noms trop longs et perdus dans l'encodage des colonnes (c'est le col.names)

bsl <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-bsl.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
sls <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-sls.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
qc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-qc.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
mcq <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-mcq.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
est <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-est.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
mtl <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-mtl.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
out <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-out.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
abt <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-abt.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
nor <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-nor.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
cot <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-cot.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
gsp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-gsp.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
cha <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-cha.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
lvl <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-lvl.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
lan <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-lan.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
lau <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-lau.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))
mtr <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/chart-mtr.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo", "Par_lien_épid", "Moyenne_mobile"))


# On combine le tout dans le même tableau de données (data frame)
# En prenant soin de remplacer les numéros donnés par les véritables régions dont il est question
# Notez que j'ai exclu le Nord-du-Québec en raison de sa faible population et de son faible nombre de cas

all <- bind_rows(bsl, sls, qc, mcq, est, mtl, out, abt, cot, gsp, cha, lvl, lan, lau, mtr, .id = "Region")
all$Region <- str_replace_all(all$Region, c("^1$" = "bsl", "^2$" = "sls", "^3$" = "qc", "^4$" = "mcq", "^5$" = "est", "^6$" = "mtl", "^7$" = "out", "^8$" = "abt", "^9$" = "cot", "^10$" = "gsp", "^11$" = "cha", "^12$" = "lvl", "^13$" = "lan", "^14$" = "lau", "^15$" = "mtr"))

all$Date <- str_trunc(all$Date, 10, side = "right", ellipsis = "") # On enlève l'inutile 00:00:00 à la fin
all$Date <- ymd(all$Date) # On met cette colonne dans le format de date

# Pour ce genre de visualisation, on aime ordonner le tout de sorte à ce que le plus grand nombre de cas soit au bas, jusqu'au plus petit nombre vers le haut

Rangs <- all %>%
  filter(Date == "2020-04-10") %>%
  arrange(En_labo + Par_lien_épid)

all$Region <- factor(all$Region, levels = Rangs$Region)

# Visualisons le tout - part proportionnelles

ggplot(all, aes(x = Date, y = En_labo + Par_lien_épid, fill = Region)) + 
  geom_area() +
  scale_fill_manual(values = as.vector(polychrome(15))) # Une palette de couleurs un peu plus aléatoire, autrement R met nos codes de couleurs des régions en dégradé, ce qui n'a pas vraiment rapport

# Supposons qu'on s'intéresse exclusivement à la proportion des choses

all2 <- all %>%
  group_by(Date, Region) %>%
  summarise(n = sum(En_labo + Par_lien_épid)) %>%
  mutate(percentage = n / sum(n))

# Revisualisons - nombres absolus

ggplot(all2, aes(x = Date, y = percentage, fill = Region)) + 
  geom_area(size = 1, color = "black") +
  scale_fill_manual(values = as.vector(polychrome(15))) 

# Flipped stacked bar chart

all <- all %>%
  mutate(tous_cas = En_labo + Par_lien_épid)

# Flipped stacked bar chart X axis reverse fix 
# Taken here, but had to do some edits in my ggplot call https://stackoverflow.com/questions/43625341/reverse-datetime-posixct-data-axis-in-ggplot
# Specifically, you reverse the date axis itself (most recent date now goes to the left) BEFORE rotating with coord_flip(), not the other way around 

c_trans <- function(a, b, breaks = b$breaks, format = b$format) {
  a <- as.trans(a)
  b <- as.trans(b)
  
  name <- paste(a$name, b$name, sep = "-")
  
  trans <- function(x) a$trans(b$trans(x))
  inv <- function(x) b$inverse(a$inverse(x))
  
  trans_new(name, trans, inverse = inv, breaks = breaks, format=format)
}

rev_date <- c_trans("reverse", "time")

ggplot(all, aes(x = as.POSIXct(Date), y = tous_cas, fill = Region)) +
  geom_col() +
  scale_fill_manual(values = as.vector(polychrome(15))) +
  scale_x_continuous(trans = rev_date) + 
  coord_flip()

### Combining regions in bigger (and somewhat arbitrary) groups so the chart is more digestible

# Régions : 
# Montréal & Laval
# Ceinture de Montréal (Laurentides, Lanaudière, Montérégie)
# Ouest (Abitibi, Outaouais)
# Centre (Mauricie, CDQ, Estrie)
# Capitale (Québec, Chaudière)
# Est (Bas, Gaspésie)
# Nord-Est (Sag, Lac, Côte-Nord)

bigger_regions <- c(
  "Montréal & Laval", # mtl lvl
  "Ceinture de Montréal (LAU, LAN, MTR)", # lan lau mtr
  "Ouest (A-T, OUT)", # abt out
  "Centre (CDQ, MAU, EST)", # mcq est
  "Grand Québec (QC, C-A)", # qc cha
  "Est (BSL, GSP)", # bsl gsp
  "Nord-Est (SG, LAC, C-N)" # sls cot
)

###### Multiple else if's - I'm pretty sure there's a better, more efficient way to do the following with the tidyverse probably, but that is what I could think about, and it works, so let's move on! :D

BigBigRegion <- character()

for (i in all$Region) {
  
  if (i == "bsl" || i == "gsp") {
    y <- "Est (BSL, GSP)"
    
  } else if (i == "mtl" || i == "lvl") {
    y <- "Montréal & Laval"
  
  } else if (i == "mtr" || i == "lau" || i == "lan") {
    y <- "Ceinture de Montréal (LAU, LAN, MTR)"  
      
  } else if (i == "abt" || i == "out") {
    y <- "Ouest (A-T, OUT)"

  } else if (i == "mcq" || i == "est") {
    y <- "Centre (CDQ, MAU, EST)"

  } else if (i == "qc" || i == "cha") {
    y <- "Grand Québec (QC, C-A)"

  } else if (i == "sls" || i == "cot") {
    y <- "Nord-Est (SG, LAC, C-N)"
  
  } else {
    y <- "WRONG"
    
  }
  ThatRegion <- y
  BigBigRegion <- c(BigBigRegion, ThatRegion)
}

all$BigRegion <- BigBigRegion

Rangs <- all %>%
  filter(Date == "2020-10-11") %>%
  group_by(BigRegion) %>%
  summarise(total = sum(En_labo + Par_lien_épid)) %>%
  arrange(desc(total))

all$BigRegion <- factor(all$BigRegion, levels = Rangs$BigRegion)

# For info : months were in English because so is my local system. To change in French : Sys.setlocale("LC_TIME", "French")

###########
# STACKED BAR CHARTS
###########

Sys.setlocale("LC_TIME", "French")

BigRegionPalette <- c("#14c891", "#337fd4", "#db40b2", "#ff9000", "#ffb9d8", "#76d6d3", "#eadd4a")

Plot1 <- ggplot(all, aes(x = as.POSIXct(Date), y = tous_cas, fill = BigRegion, color = BigRegion)) +
  geom_col(position = "stack") +
  scale_fill_manual(name = "Grande région", values = BigRegionPalette) +
  scale_color_manual(name = "Grande région", values = BigRegionPalette) +
  scale_x_continuous(trans = rev_date) +
  theme_minimal() +
  theme(
    text = element_text(family = "Myanmar Text"),
    axis.text.x = element_text(family = "Oswald Regular", color = "black", size = 30),
    axis.text.y = element_text(family = "Oswald Regular", color = "black", size = 30),
    axis.title = element_text(color = "#030093", face = "bold", size = 30),
    axis.ticks.length = unit(-1, "lines"),
    axis.line = element_blank(),
    plot.background = element_rect(fill = "white"),
    plot.title = element_text(color = "#030093", face = "bold", size = 40),
    plot.subtitle = element_text(color = "#030093", face = "bold", size = 32),
    legend.title = element_blank(),
    legend.text = element_text(face = "bold", color = "#030093", size = 20),
    legend.key.size = unit(2, "cm"),
    legend.position = c(0.6, 0.6),
    legend.margin = margin(2, 5, 5, 5, "pt"),
    panel.grid = element_blank(),
    plot.caption = element_text(size = 20),
    plot.margin = unit(c(0.5,0.5,0.5,0.5), "in")
  ) + 
  labs(
    x = NULL,
    y = "Nombre de cas",
    title = "Évolution du nombre de cas confirmés de COVID-19 au Québec",
    subtitle = "Selon la grande région et la date de déclaration des cas",
    caption = "Source : Institut national de santé publique du Québec (INSPQ)"
  ) +
  coord_flip()

# proportional stacked, position = "fill" in geom_col

head(all)


##########
# CONTRIBUTION TIMELINE

# Even though I just went to lengths to do just that, I am not a big fan of absolute numbers. I'll always expect Montreal to have much more cases most days than the Côte-Nord, so showing both as they are isn't very informative
# Still, we were able to see that, in the first wave, Montreal and Laval were pretty much running the show, while in the second, everybody has been joining the party.

# Anyway, what I'd like to know is how many cases of infection there have been across regions but compared to their share of population of the whole province, which is a bit different than calculating the number of cases per capita, or per 100 000 pop, although on a chart, both relative numbers and their respective trends may look alike

##########




### Excel file containing regional population data from 2019 - got rid of all unnecessary white space and extra years in Excel

pop2019ra <- read.csv(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/covid19byqcregion/RA_totalclean2019.csv", header = FALSE)

pop2019ra$CodeRegion <- str_replace_all(pop2019ra$V1, c("^1$" = "bsl", "^2$" = "sls", "^3$" = "qc", "^4$" = "mau", "^5$" = "est", "^6$" = "mtl", "^7$" = "out", "^8$" = "abt", "^9$" = "cot", "^10$" = "nor", "^11$" = "gsp", "^12$" = "cha", "^13$" = "lvl", "^14$" = "lan", "^15$" = "lau", "^16$" = "mtr", "^17$" = "ctr"))

# Summing up populations into bigger regional populations

pop2019_bigreg <- rbind(
  "Montréal & Laval" = pop2019ra$V3[pop2019ra$CodeRegion == "mtl"] + pop2019ra$V3[pop2019ra$CodeRegion == "lvl"],
  "Ceinture de Montréal (LAU, LAN, MTR)" = pop2019ra$V3[pop2019ra$CodeRegion == "lau"] + pop2019ra$V3[pop2019ra$CodeRegion == "lan"] + pop2019ra$V3[pop2019ra$CodeRegion == "mtr"],
  "Ouest (A-T, OUT)" = pop2019ra$V3[pop2019ra$CodeRegion == "abt"] + pop2019ra$V3[pop2019ra$CodeRegion == "out"],
  "Centre (CDQ, MAU, EST)" = pop2019ra$V3[pop2019ra$CodeRegion == "ctr"] + pop2019ra$V3[pop2019ra$CodeRegion == "mau"] + pop2019ra$V3[pop2019ra$CodeRegion == "est"],
  "Grand Québec (QC, C-A)" = pop2019ra$V3[pop2019ra$CodeRegion == "qc"] + pop2019ra$V3[pop2019ra$CodeRegion == "cha"],
  "Est (BSL, GSP)" = pop2019ra$V3[pop2019ra$CodeRegion == "bsl"] + pop2019ra$V3[pop2019ra$CodeRegion == "gsp"],
  "Nord-Est (SG, LAC, C-N)" = pop2019ra$V3[pop2019ra$CodeRegion == "sls"] + pop2019ra$V3[pop2019ra$CodeRegion == "cot"]
    )

# Calculating the percentage of a big region's population, its share among Quebec's total population as of 2019
# Also for the individual regions themselves, while we're at it

pop2019_bigreg <- cbind(pop2019_bigreg, pop2019_bigreg / sum(pop2019ra$V3))
pop2019_bigreg <- data.frame(rownames(pop2019_bigreg), pop2019_bigreg, check.names = FALSE)
colnames(pop2019_bigreg) <- c("BigRegion", "Pop", "PartPop")

pop2019ra$PartPop <- pop2019ra$V3 / sum(pop2019ra$V3)

# Making sure my big region strings are the same across datasets, which should give me 7 "TRUE" if so

sort(as.vector(unique(all$BigRegion))) == sort(unique(rownames(pop2019_bigreg)))

# Summing up all of the province's cases for each date

SommeDates <- all %>%
  group_by(Date) %>%
  summarise(TotalProvince = sum(tous_cas))

##### EVERYTHING ABOVE IS GOOD


all$TotalProvinceCeJourLa <- SommeDates$TotalProvince
all$PartDesCas <- all$tous_cas / all$TotalProvinceCeJourLa

# Some NaN resulted from this latter operation

all$PartDesCas[is.na(all$PartDesCas)] <- 0

# So I can't seem to find an easier, efficient, tidyverse way to calculate and plot the difference between the big regions' shares of all the provincial population and their daily share of COVID-19 cases in all the province

PartPopBigRegion <- character()

for (i in all$Region) {
  
  if (i == "bsl" || i == "gsp") {
    y <- pop2019_bigreg$PartPop[pop2019_bigreg$BigRegion == "Est (BSL, GSP)"]
    
  } else if (i == "mtl" || i == "lvl") {
    y <- pop2019_bigreg$PartPop[pop2019_bigreg$BigRegion == "Montréal & Laval"] 
    
  } else if (i == "mtr" || i == "lau" || i == "lan") {
    y <- pop2019_bigreg$PartPop[pop2019_bigreg$BigRegion == "Ceinture de Montréal (LAU, LAN, MTR)"]   
    
  } else if (i == "abt" || i == "out") {
    y <- pop2019_bigreg$PartPop[pop2019_bigreg$BigRegion == "Ouest (A-T, OUT)"]
    
  } else if (i == "mcq" || i == "est") {
    y <- pop2019_bigreg$PartPop[pop2019_bigreg$BigRegion == "Centre (CDQ, MAU, EST)"] 
    
  } else if (i == "qc" || i == "cha") {
    y <- pop2019_bigreg$PartPop[pop2019_bigreg$BigRegion == "Grand Québec (QC, C-A)"] 
    
  } else if (i == "sls" || i == "cot") {
    y <- pop2019_bigreg$PartPop[pop2019_bigreg$BigRegion == "Nord-Est (SG, LAC, C-N)"] 
    
  } else {
    y <- "WRONG"
    
  }
  
  SomeData <- y
  PartPopBigRegion <- c(PartPopBigRegion, SomeData)
}

all$PartPopBigRegion <- as.numeric(PartPopBigRegion)

### SAME OPERATION, BUT FOR THE SMALLER INDIVIDUAL REGIONS

PartPopRegion <- character()

for (i in all$Region) {
  
  if (i == "bsl") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "bsl"]
    
  } else if (i == "sls") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "sls"]
    
  } else if (i == "qc") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "qc"]
    
  } else if (i == "mcq") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "mau"] + pop2019ra$PartPop[pop2019ra$CodeRegion == "ctr"]
    
  } else if (i == "est") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "est"]
    
  } else if (i == "mtl") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "mtl"]
    
  } else if (i == "out") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "out"]
    
  } else if (i == "abt") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "abt"]
    
  } else if (i == "cot") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "cot"]
    
  } else if (i == "gsp") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "gsp"]
    
  } else if (i == "cha") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "cha"]
    
  } else if (i == "lvl") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "lvl"]
    
  } else if (i == "lan") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "lan"]
    
  } else if (i == "lau") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "lau"]
    
  } else if (i == "mtr") {
    y <- pop2019ra$PartPop[pop2019ra$CodeRegion == "mtr"]
      
  } else {
    y <- "WRONG"
    
  }
  
  SomeOtherData <- y
  PartPopRegion <- c(PartPopRegion, SomeOtherData)
}

all$PartPopRegion <- as.numeric(PartPopRegion)

### PLOTTING : CONTRIBUTION TIMELINE

# Reordering our regions according to their bigger regional group - with the forcats package

all$Region <- fct_relevel(all$Region, c("abt", "out", "mtl", "lvl", "lan", "lau", "mtr", "mcq", "est", "qc", "cha", "sls", "cot", "bsl", "gsp"))

NomsRegions <- c(
  `abt` = "Abitibi-Témiscamingue",
  `out` = "Outaouais",
  `mtl` = "Montréal",
  `lvl` = "Laval",
  `lan` = "Lanaudière",
  `lau` = "Laurentides",
  `mtr` = "Montérégie",
  `mcq` = "Mauricie & Centre-du-Québec",
  `est` = "Estrie",
  `qc` = "Québec",
  `cha` = "Chaudière-Appalaches",
  `sls` = "Saguenay-Lac-Saint-Jean",
  `cot` = "Côte-Nord",
  `bsl` = "Bas-Saint-Laurent",
  `gsp` = "Gaspésie-Îles-de-la-Madeleine"
)

all$RegionFull <- NomsRegions[all$Region]
all$CasMoinsPop <- all$PartDesCas - all$PartPopRegion

WaveAnnotate <- data.frame(Date = c(as.Date("2020-05-01"), as.Date("2020-12-01")), CasMoinsPop = 0.4, Region = factor("abt"), label = c("Première vague", "Deuxième vague"))

Sys.setlocale("LC_TIME", "French")

Plot2 <- ggplot(all, aes(x = Date, y = CasMoinsPop)) +
  geom_col(aes(fill = BigRegion), width = 1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y", limit = c(as.Date("2020-03-20"), as.Date("2021-02-22"))) +
  geom_hline(aes(yintercept = 0, color = BigRegion), size = .5) + 
  geom_text(data = WaveAnnotate, label = WaveAnnotate$label, size = 4) +
  scale_y_continuous(labels = scales::percent, breaks = c(0, 0.2, 0.4), limits = c(0, 0.5)) +
  scale_fill_manual(values = BigRegionPalette) +
  scale_color_manual(values = BigRegionPalette) +
  theme_minimal() +
  theme(
    text = element_text(family = "Myanmar Text"),
    axis.text.x = element_text(family = "Oswald Regular", color = "black", size = 14),
    axis.text.y = element_text(family = "Oswald Regular", color = "#7d7d7d"),
    axis.title = element_text(color = "#030093", face = "bold"),
    axis.title.y = element_text(size = 12),
    plot.background = element_rect(fill = "transparent"),
    plot.title = element_text(color = "#030093", face = "bold", size = 14),
    plot.subtitle = element_text(color = "#030093", face = "bold", size = 12),
    panel.grid = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    panel.spacing.y = unit(0, "lines")
  ) + 
  labs(
    x = NULL,
    y = "Proportion des cas c. proportion de la population",
    title = "La surreprésentation de chacune des régions dans les nouveaux cas d'infection à la COVID-19",
    subtitle = "Selon la région et la date de déclaration des cas",
    caption = "Source : Institut national de santé publique du Québec (INSPQ)"
  ) + 
  facet_grid(
    rows = vars(Region)
    ) +
  geom_label(aes(label = RegionFull), x = Inf, y = 0.3, hjust = 1, vjust = 1, label.size = NA, fill = "transparent", size = 4) +
  annotate("rect", xmin = as.Date("2020-03-20"), xmax = as.Date("2020-06-14"), ymin = -Inf, ymax = Inf, alpha = .05, fill = "blue") +
  annotate("rect", xmin = as.Date("2020-09-09"), xmax = as.Date("2021-02-22"), ymin = -Inf, ymax = Inf, alpha = .05, fill = "blue") 


### PLOTTING : CUMUL DES CAS, HOSPITALISATIONS, SOINS INTENSIFS ET DÉCÈS

# CUMULATIF DES CAS CONFIRMÉS

bsl_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-bsl.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
sls_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-sls.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
qc_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-qc.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
mcq_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-mcq.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
est_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-est.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
mtl_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-mtl.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
out_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-out.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
abt_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-abt.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
nor_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-nor.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
cot_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-cot.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
gsp_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-gsp.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
cha_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-cha.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
lvl_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-lvl.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
lan_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-lan.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
lau_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-lau.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))
mtr_cc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-cas-mtr.csv", header = TRUE, sep = ",", col.names = c("Date", "En_labo_cc", "Par_lien_épid_cc", "Cas_actifs"))

all_cc <- bind_rows(bsl_cc, sls_cc, qc_cc, mcq_cc, est_cc, mtl_cc, out_cc, abt_cc, nor_cc, cot_cc, gsp_cc, cha_cc, lvl_cc, lan_cc, lau_cc, mtr_cc, .id = "Region")

all_cc$Region <- str_replace_all(all_cc$Region, c("^1$" = "bsl", "^2$" = "sls", "^3$" = "qc", "^4$" = "mcq", "^5$" = "est", "^6$" = "mtl", "^7$" = "out", "^8$" = "abt", "^9$" = "nor", "^10$" = "cot", "^11$" = "gsp", "^12$" = "cha", "^13$" = "lvl", "^14$" = "lan", "^15$" = "lau", "^16$" = "mtr"))

all_cc$Date <- str_trunc(all_cc$Date, 10, side = "right", ellipsis = "") # On enlève l'inutile 00:00:00 à la fin
all_cc$Date <- ymd(all_cc$Date) # On met cette colonne dans le format de date

all_cc <- all_cc %>% 
  mutate(Total_cc = En_labo_cc + Par_lien_épid_cc)

# CUMULATIF DES DÉCÈS

bsl_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-bsl.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
sls_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-sls.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
qc_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-qc.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
mcq_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-mcq.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
est_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-est.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
mtl_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-mtl.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
out_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-out.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
abt_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-abt.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
nor_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-nor.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
cot_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-cot.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
gsp_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-gsp.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
cha_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-cha.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
lvl_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-lvl.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
lan_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-lan.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
lau_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-lau.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))
mtr_dc <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/cumul-deces-mtr.csv", header = TRUE, sep = ",", col.names = c("Date", "CHSLD", "RPA", "DomInc", "RI_Autre"))

all_dc <- bind_rows(bsl_dc, sls_dc, qc_dc, mcq_dc, est_dc, mtl_dc, out_dc, abt_dc, nor_dc, cot_dc, gsp_dc, cha_dc, lvl_dc, lan_dc, lau_dc, mtr_dc, .id = "Region")

all_dc$Region <- str_replace_all(all_dc$Region, c("^1$" = "bsl", "^2$" = "sls", "^3$" = "qc", "^4$" = "mcq", "^5$" = "est", "^6$" = "mtl", "^7$" = "out", "^8$" = "abt", "^9$" = "nor", "^10$" = "cot", "^11$" = "gsp", "^12$" = "cha", "^13$" = "lvl", "^14$" = "lan", "^15$" = "lau", "^16$" = "mtr"))

all_dc$Date <- str_trunc(all_dc$Date, 10, side = "right", ellipsis = "") # On enlève l'inutile 00:00:00 à la fin
all_dc$Date <- ymd(all_dc$Date) # On met cette colonne dans le format de date

all_dc <- all_dc %>% 
  mutate(Total_dc = CHSLD + RPA + DomInc + RI_Autre)

# HOSPITALISATIONS 

bsl_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-bsl.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
sls_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-sls.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
qc_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-qc.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
mcq_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-mcq.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
est_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-est.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
mtl_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-mtl.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
out_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-out.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
abt_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-abt.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
nor_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-nor.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
cot_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-cot.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
gsp_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-gsp.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
cha_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-cha.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
lvl_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-lvl.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
lan_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-lan.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
lau_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-lau.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))
mtr_hosp <- read.table(file = "C:/Users/moukm/Google Drive/SCARUFEL-COM/Corps Rona Vie Russe/hosp-mtr.csv", header = TRUE, sep = ",", col.names = c("Date", "Hors_SI", "Aux_SI"))

all_hosp <- bind_rows(bsl_hosp, sls_hosp, qc_hosp, mcq_hosp, est_hosp, mtl_hosp, out_hosp, abt_hosp, nor_hosp, cot_hosp, gsp_hosp, cha_hosp, lvl_hosp, lan_hosp, lau_hosp, mtr_hosp, .id = "Region")

all_hosp$Region <- str_replace_all(all_hosp$Region, c("^1$" = "bsl", "^2$" = "sls", "^3$" = "qc", "^4$" = "mcq", "^5$" = "est", "^6$" = "mtl", "^7$" = "out", "^8$" = "abt", "^9$" = "nor", "^10$" = "cot", "^11$" = "gsp", "^12$" = "cha", "^13$" = "lvl", "^14$" = "lan", "^15$" = "lau", "^16$" = "mtr"))

all_hosp$Date <- str_trunc(all_hosp$Date, 10, side = "right", ellipsis = "") # On enlève l'inutile 00:00:00 à la fin
all_hosp$Date <- ymd(all_hosp$Date) # On met cette colonne dans le format de date

all_hosp <- all_hosp %>% 
  mutate(Total_hosp = Hors_SI + Aux_SI)

# Joining all three datasets but by selecting only the columns we want

all_chart3 <- left_join(all_cc, all_dc, by = c("Region", "Date")) %>% 
  left_join(., all_hosp, by = c("Region", "Date")) %>%
  select(Region, Date, Cas_actifs, Total_cc, Total_dc, Hors_SI, Aux_SI, Total_hosp)

# To calculate the figures per 100 000 pop, I need the absolute population per region of course
# Here I just adapted my previous for loop from above

PopRegion <- character()

for (i in all_chart3$Region) {
  
  if (i == "bsl") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "bsl"]
    
  } else if (i == "sls") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "sls"]
    
  } else if (i == "qc") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "qc"]
    
  } else if (i == "mcq") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "ctr"] + pop2019ra$V3[pop2019ra$CodeRegion == "mau"]
    
  } else if (i == "est") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "est"]
    
  } else if (i == "mtl") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "mtl"]
    
  } else if (i == "out") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "out"]
    
  } else if (i == "abt") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "abt"]
    
  } else if (i == "nor") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "nor"]
    
  } else if (i == "cot") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "cot"]
    
  } else if (i == "gsp") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "gsp"]
    
  } else if (i == "cha") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "cha"]
    
  } else if (i == "lvl") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "lvl"]
    
  } else if (i == "lan") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "lan"]
    
  } else if (i == "lau") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "lau"]
    
  } else if (i == "mtr") {
    y <- pop2019ra$V3[pop2019ra$CodeRegion == "mtr"]
    
  } else {
    y <- "WRONG"
    
  }
  
  SomeGoodData <- y
  PopRegion <- c(PopRegion, SomeGoodData)
}

all_chart3$PopRegion <- as.numeric(PopRegion)

library(zoo) # For rollsum

all_chart3 <- all_chart3 %>%
  mutate(
    CumulHosp = rollsum(Total_hosp, 1:365, fill = NA, align = "right"), 
    Cumul_SI = rollsum(Aux_SI, 1:365, fill = NA, align = "right")
    )

# Preparing the chart

BigRegion3 <- character()

for (i in all_chart3$Region) {
  
  if (i == "bsl" || i == "gsp") {
    y <- "Est (BSL, GSP)"
    
  } else if (i == "mtl" || i == "lvl") {
    y <- "Montréal & Laval"
    
  } else if (i == "mtr" || i == "lau" || i == "lan") {
    y <- "Ceinture de Montréal (LAU, LAN, MTR)"
    
  } else if (i == "abt" || i == "out") {
    y <-"Ouest (A-T, OUT)"
    
  } else if (i == "mcq" || i == "est") {
    y <- "Centre (CDQ, MAU, EST)"
    
  } else if (i == "qc" || i == "cha") {
    y <- "Grand Québec (QC, C-A)"
    
  } else if (i == "sls" || i == "cot") {
    y <- "Nord-Est (SG, LAC, C-N)"
    
  } else if (i == "nor") {
    y <- "Nouveau-Québec"
    
  } else {
    y <- "WRONG"
    
  }
  
  SomeData3 <- y
  BigRegion3 <- c(BigRegion3, SomeData3)
}

all_chart3$BigRegion3 <- BigRegion3

# Theming

theme3 <- theme_minimal() +
  theme(
    text = element_text(family = "Myanmar Text"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(family = "Oswald Regular", color = "#7d7d7d"),
    axis.title = element_text(color = "#030093", face = "bold"),
    plot.title = element_text(color = "#030093", face = "bold"),
    plot.subtitle = element_text(color = "#030093", face = "bold"),
    panel.grid = element_blank(),
    legend.position = "none",
    strip.background = element_blank(),
    axis.title.x = element_blank(),
    strip.text.x = element_text(color = "#030093", face = "bold"),
    axis.title.y = element_text(margin = margin(c(0.1,0.1,0.1,0.1), unit = "in"))
  )

ggplotcall <- ggplot(all_chart3, aes(x = Date))

# So as glenn in boston proposed in Stack Overflow, I insisted on the reordering directly within the faceting instead, and it works, because doing it with fct_reorder or factor separately before that didn't work for some reason

regiongrid <- facet_grid(~factor(
  Region, levels = c("abt", "out", "mtl", "lvl", "lan", "lau", "mtr", "mcq", "est", "qc", "cha", "sls", "cot", "bsl", "gsp", "nor"), 
  labels = c("Abitibi-T.",
             "Outaouais",
             "Montréal",
             "Laval",
             "Lanaudière",
             "Laurentides",
             "Montérégie",
             "Mauricie-CDQ",
             "Estrie",
             "Québec",
             "Chaudière-App.",
             "Saguenay-LSJ",
             "Côte-Nord",
             "Bas-St-Laurent",
             "Gaspésie-Îles",
             "Nord-du-Québec")))

#all_chart3$BigRegion3 <- factor(all_chart3$BigRegion3, levels = c(Rangs$BigRegion, "Nouveau-Québec"))
all_chart3$BigRegion3 <- factor(all_chart3$BigRegion3, levels = c("Ouest (A-T, OUT)", "Montréal & Laval", "Ceinture de Montréal (LAU, LAN, MTR)", "Centre (CDQ, MAU, EST)", "Grand Québec (QC, C-A)", "Nord-Est (SG, LAC, C-N)", "Est (BSL, GSP)", "Nouveau-Québec"))



BigRegionPalette3pale <- c("#9effe1", "#c1deff", "#ffb6ec", "#ffd8a6", "#ffe6f1", "#bffffd", "#fff9b8", "#e9e9e9")

BigRegionPalette3pale_extra <- c("#9effe1", "#c1deff", "#ffb6ec", "#ffd8a6", "#ffe6f1", "#bffffd", "#fff9b8")

colorscale <- scale_color_manual(values = c(BigRegionPalette, "gray"))
colorscale2 <- scale_fill_manual(values = BigRegionPalette3pale)

colorscale_extra <- scale_color_manual(values = BigRegionPalette)
colorscale2_extra <- scale_fill_manual(values = BigRegionPalette3pale_extra)

# Deploying the charts

ggcc <- ggplotcall + 
  geom_area(aes(y = Total_cc / PopRegion * 100000, fill = BigRegion3, color = BigRegion3), size = 1) +
  colorscale +
  colorscale2 + 
  theme3 +
  theme(plot.subtitle = element_text(hjust = 0.5)) +
  labs(
    y = "Cas confirmés",
    subtitle = "Par 100 000 habitants, du 24 février 2020 au 23 février 2021"
    ) +
  regiongrid

ggdc <- ggplotcall + 
  geom_area(aes(y = Total_dc / PopRegion * 100000, fill = BigRegion3, color = BigRegion3), size = 1) +
  colorscale +
  colorscale2 +
  theme3 +
  theme(
    strip.text.x = element_blank()
  ) +
  labs(
    y = "Décès",
    caption = "Source : Institut national de santé publique du Québec (INSPQ)"
    ) +
  regiongrid

gghosp <- ggplotcall + 
  geom_area(aes(y = CumulHosp / PopRegion * 100000, fill = BigRegion3, color = BigRegion3), size = 1) +
  colorscale_extra +
  colorscale2_extra +
  theme3 +
  theme(
    strip.text.x = element_blank()
  ) +
  labs(y = "Hospitalisations") +
  regiongrid

ggsi <- ggplotcall + 
  geom_area(aes(y = Cumul_SI / PopRegion * 100000, fill = BigRegion3, color = BigRegion3), size = 1) +
  colorscale_extra +
  colorscale2_extra +
  theme3 +
  theme(
    strip.text.x = element_blank()
  ) +
  labs(y = "Soins Intensifs") +
  regiongrid
  


Plot3 <- grid.arrange(ggcc, gghosp, ggsi, ggdc, ncol = 1, 
             top = textGrob("Évolution du cumulatif des cas confirmés, des hospitalisations et des décès", 
                            gp = gpar(
                              fontsize = 16, fontfamily = "Myanmar Text", fontface = "bold", col = "#030093")
                            )
             )







##############################
ggplot(all_chart3, aes(x = Date)) + 
  geom_line(aes(y = Total_cc / PopRegion * 100000)) +
  geom_line(aes(y = Total_dc / PopRegion * 100000)) + 
  geom_line(aes(y = Total_hosp / PopRegion * 100000)) +
  geom_line(aes(y = Aux_SI / PopRegion * 100000)) +
  facet_grid(cols = vars(Region))