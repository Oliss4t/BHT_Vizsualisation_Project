library(kohonen)
library("viridis")
setwd('~/Documents/M/Semester 2/Visualisation')
happy <- read.csv("output.csv")


happy2015 <- happy[1:158,c(2,5,7,8,9,10, 11, 12)]

happy2015_mat <- as.matrix(happy2015)


# Add a colour scale 

#######
# Remove country names to leave only numberic values
happy2015_mat <- as.matrix(happy2015[,-1])
happy_grid <- somgrid(xdim = 7, ydim = 7, topo = "hexagonal")
happy_SOM_model <- som(X = happy2015_mat, grid=happy_grid)

#######
# Plot counts

plot(happy_SOM_model, type="counts")

# Plot heat map

plot(happy_SOM_model, type = "property", property = getCodes(happy_SOM_model)[, 1],
     main = colnames(happy2015)[2])

# See here for general stuff on colours https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
library("RColorBrewer")
plot(happy_SOM_model, type="quality", palette.name=pal)
pal <- function(n) viridis(n)
plot(happy_SOM_model, shape="straight", palette.name=pal)


# Try organise them in a grid
plot(happy_SOM_model, shape="straight", palette.name=pal)

library(RColorBrewer)
brewer.pal(n = 7, name = 'Set1')

plot(happy_SOM_model, type="mapping", pch=20,
     col = brewer.pal(n = 7, name = 'Set1')[as.integer(as.factor(happy2015$region))],
     shape = "round", lwd = 3)

lgd.txt = c("Europe & Central Asia", "North America", "East Asia & Pacific", 
            "Middle East & North Africa", "Latin America & Caribbean", 
            "Sub-Saharan Africa", "South Asia")
legend("left", inset= -.05, title=" ",
       lgd.txt, fill=brewer.pal(n = 7, name = 'Set1'),
       horiz=FALSE, cex=0.6,
       text.width = NA, bty="n")


















# THIS LEAVES ONLY COUNTRY CODES
#plot(happy_SOM_model, type = "mapping", labels = happy[,'Region'], family='EmojiOne')

# Try get country flags
library(countrycode)
library(ggimage)

library(emojifont)

# install.packages("devtools")
devtools::install_github("hadley/emo")


if(!require('emo')) {
  install.packages('emo')
  library('emo')
}

# Now try add image to each country name

# This works apart from NA flags
for (i in 1:nrow(happy2015)){
  tryCatch(happy2015[i, 'flags'] <- emo::ji(happy2015[i,1]),
           error = function(e){
             message(paste("An error occurred for item", i, happy2015[[i,1]],":\n"), e)
             
           })
}

happy2015[, 'region'] <- ' '
#FOR LOOP TO ADD REGION

for (i in 1:nrow(happy2015)){
  tryCatch(happy2015[i, 'region'] <- countrycode(happy2015[i,1], origin = 'country.name', destination = 'region'),
           error = function(e){
             message(paste("An error occurred for item", i, happy2015[[i,1]],":\n"), e)
             
           })
}

#TRY CONVERT TO PNGs https://stackoverflow.com/questions/59606453/how-do-i-convert-an-utf8-encoded-emoji-character-to-an-image

devtools::install_github( "ThinkRstat/utf8splain")
library(utf8splain)
emojis <- happy2015[,'flags']
rune <- sapply(emojis, function(x) utf8splain::runes(x)$rune)
emojiurl <- paste0("https://abs.twimg.com/emoji/v2/72x72/", tolower(rune), ".png")

#for (i in seq_along(emojiurl)) {
#download.file(emojiurl[i], paste0(rune[i], ".png"), method = "curl")
#}
library("ggplot2")
library("ggimage")
d <- data.frame(x = rnorm(10),
                y = rnorm(10),
                image = sample(c("https://www.r-project.org/logo/Rlogo.png",
                                 "https://jeroenooms.github.io/images/frink.png"),
                               size=10, replace = TRUE)
)
ggplot(d, aes(x, y)) + geom_image(aes(image=image), size=.05)
ggplot(d, aes(x, y)) + geom_image(aes(image=image), size=.05)



library(tidyverse)  # for data wrangling
library(stringr)    # for string manipulations
library(kohonen)    # implements self organizing maps
library(ggforce) 


som_grid <- happy_SOM_model[[4]]$pts %>%
  as_tibble %>% 
  mutate(id=row_number())
som_grid

som_pts <- tibble(id = happy_SOM_model[[2]],
                  dist = happy_SOM_model[[3]],
                  type = happy2015$region)

som_pts <- som_pts %>% left_join(som_grid,by="id")






### Happiness

happy <- read.csv('https://raw.githubusercontent.com/Oliss4t/BHT_Vizsualisation_Project/main/data/preprocessed_data_2018_no_nan.csv')


#View(happy)


library(ggplot2)
library('plotly')
library('viridis')
library(dplyr)
library(ggpubr)
# Add marginal rugs
p <- ggplot(happy, 
            aes(x=Economy..GDP.per.Capita., y=Happiness.Score, color=Region, text=Country)) +
  geom_point(aes(size = Total.alcohol.consumption.per.capita..liters.of.pure.alcohol..projected.estimates..15..years.of.age.,)) +
  scale_color_manual(values = rainbow(10)) +
  scale_size_continuous(range = c(0, 3), name='')
ggplotly(p, tooltip = c("text"))

# Alcohol and happiness
p <- ggplot(happy, 
            aes(x=Total.alcohol.consumption.per.capita..liters.of.pure.alcohol..projected.estimates..15..years.of.age., y=Happiness.Score, color=Region, text=Country)) +
  geom_smooth(aes(group=as.factor(Region)), method = "lm", se=FALSE) + 
  geom_point(aes(size = Total.alcohol.consumption.per.capita..liters.of.pure.alcohol..projected.estimates..15..years.of.age.,)) +
  scale_color_manual(values = rainbow(10)) +
  scale_size_continuous(range = c(0, 3), name='')
ggplotly(p, tooltip = c("text"))

# Smoking and happiness

key <- highlight_key(happy, ~Region)

# There seems to be a negative correlation here when observed through sub-region
p <- ggplot(happy, 
            aes(x=Prevalence.of.current.tobacco.use....of.adults., y=Happiness.Score, color=Region, text=Country, group = Region)) +
  geom_smooth(aes(group=as.factor(Region)), method = "lm", se=FALSE) + 
  geom_point(aes(size = Total.alcohol.consumption.per.capita..liters.of.pure.alcohol..projected.estimates..15..years.of.age.,)) +
  scale_color_manual(values = rainbow(10, alpha=1)) +
  scale_size_continuous(range = c(0, 3), name='')
gg <- ggplotly(p, tooltip = c("text"))

# Dimming won't work for some reason
highlight(gg, on = "plotly_hover", off = "plotly_deselect", opacityDim = .8)




#ATTEMPT THIS ONE WORKS !!!

#create a SharedData object for use in the ggplot below, group by 'groups' 
d <- highlight_key(happy, ~Region)

#create a normal ggplot to fit your needs, but use the SharedData object as data for the chart
p <-ggplot( d, aes(x = Prevalence.of.current.tobacco.use....of.adults., y = Happiness.Score, group = Region, color = Region, text = Country)) + 
  labs(y= "Happiness Score", x = "Tobacco Use (%)", title = "Smoking and Happiness") + 
  geom_smooth(aes(group=as.factor(Region)), method = "lm", se=FALSE, size=0.5) + 
  geom_point(aes(size = Economy..GDP.per.Capita.)) +
  theme_bw() + 
  scale_color_manual(values = rainbow(10, alpha=0.6)) +
  scale_size_continuous(range = c(0, 10), name='') +
  stat_cor(aes(label = ..rr.label..), color = rainbow(10), geom = "label")

#now ggplotly the newly created ggplot, and add text for the tooltips as needed
gg <- ggplotly(p, tooltip = "text") %>% 
  highlight(on = 'plotly_click', off = 'plotly_doubleclick', 
            opacityDim = .05)

#set the highlight-options to your liking, and plot...
highlight( gg, on = "plotly_click", off = "plotly_doubleclick", opacityDim = .05)

# First, make the lines invisible (because no groups are highlighted)
# Remove the line legend; add the point legend
invisible(
  lapply(1:length(gg$x$data),
         function(j){
           print(j)
           nm <- gg$x$data[[j]]$name
           md <- gg$x$data[[j]]$mode
           if(md == "lines") {
             gg$x$data[[j]]$visible <<- FALSE
             gg$x$data[[j]]$showlegend <<- FALSE
           } else {
             gg$x$data[[j]]$visible <<- TRUE
             gg$x$data[[j]]$showlegend <<- TRUE
           }
         }
  ))

gg %>% htmlwidgets::onRender(
  "function(el, x){
    v = [] /* establish outside of the events; used for both */
    for (i = 0; i < 22; i++) {  /*1st 11 are lines; 2nd 11 are points */
      if(i < 12){
        v[i] = false;
      } else {
        v[i] = true;
      }
    }
    console.log(x);
    el.on('plotly_click', function(d) {
      cn = d.points[0].curveNumber - 10;  /*if [8] is the lines, [18] is the points*/
      v2 = JSON.parse(JSON.stringify(v)); /*create a deep copy*/
      v2[cn, d.points[0].curveNumber] = true;
      update = {visible: v2};
      Plotly.restyle(el.id, update); /* in case 1 click to diff highlight */
    });
    el.on('plotly_doubleclick', function(d) {
        console.log('out ', d);
        update = {visible: v}
        console.log('dbl click ' + v);
        Plotly.restyle(el.id, update);
    });
  }")

#############
# Internet and happiness
p <- ggplot(happy, 
            aes(x=Individuals.using.the.Internet....of.population., y=Happiness.Score, color=Region, text=Country)) +
  geom_point(aes(size = Total.alcohol.consumption.per.capita..liters.of.pure.alcohol..projected.estimates..15..years.of.age.,)) +
  geom_smooth(aes(group=as.factor(Region)), method = "lm", se=FALSE) + 
  scale_color_manual(values = rainbow(10)) +
  scale_size_continuous(range = c(0, 3), name='')
ggplotly(p, tooltip = c("text"))



#TIME SERIES

full <- read.csv('https://raw.githubusercontent.com/Oliss4t/BHT_Vizsualisation_Project/main/data/preprocessed_data.csv')

p <-  ggplot(full, aes(Year, Happiness.Score, color = Region, text = Country)) +
  geom_point(na.rm=TRUE, size=3, pch=18) +
  scale_color_manual(values = rainbow(11)) +
  geom_smooth(aes(group=as.factor(Region)), method = "lm", se=FALSE)


hh <- ggplotly(p, tooltip = "text")


# MAP

library(tmap)
data("World")

tm_shape(World) +
  tm_polygons("HPI")

tm_fill(col="VACANTPERCAP", 
        palette = viridisLite::turbo(10),
        style="cont", legend.reverse = TRUE,
        title = "Vacants/1000") +
  tm_borders(col = "black") +
  tm_layout(frame = FALSE, scale = 1.3)

tmap_mode("view")

tm_shape(World) +
  tm_polygons("HPI")

# https://datavizpyr.com/how-to-make-us-state-and-county-level-maps-in-r/

# Interesting that tobacco is nearly random over all countries, but highly correlated in richer areas
summary(lm(happy[happy$Region=='Southeast Asia', ]$Happiness.Score ~ happy[happy$Region=='Southeast Asia', ]$Prevalence.of.current.tobacco.use....of.adults.))[8]

summary(lm(happy[happy$Region=='Western Europe', ]$Happiness.Score ~ happy[happy$Region=='Western Europe', ]$Prevalence.of.current.tobacco.use....of.adults.))[8]

summary(lm(happy[happy$Region=='North America and ANZ', ]$Happiness.Score ~ happy[happy$Region=='North America and ANZ', ]$Prevalence.of.current.tobacco.use....of.adults.))[8]
