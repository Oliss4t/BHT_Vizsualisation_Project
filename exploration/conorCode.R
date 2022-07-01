library(kohonen)
library("viridis")
full2 <- read.csv('https://raw.githubusercontent.com/Oliss4t/BHT_Vizsualisation_Project/main/data/preprocessed_data.csv')
happy2015 <- full2[1:158,c(2,5,7,8,9,10, 11, 12)]

happy2015_mat <- as.matrix(happy2015)


# Add a colour scale 

#######
# Remove country names to leave only numberic values
happy2015_mat <- as.matrix(happy2015[,2:6])

happy_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
happy_SOM_model <- som(X = happy2015_mat, grid=happy_grid)

# See here for general stuff on colours https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
library("RColorBrewer")
pal <- function(n) viridis(n)
plot(happy_SOM_model, shape="straight", palette.name=pal)

brewer.pal(n = 7, name = 'Set1')

plot(happy_SOM_model, type="mapping", pch=20,
     col = brewer.pal(n = 7, name = 'Set1')[as.integer(as.factor(happy2015$Region))],
     shape = "round", lwd = 3)

lgd.txt = c("Europe & Central Asia", "North America", "East Asia & Pacific", 
            "Middle East & North Africa", "Latin America & Caribbean", 
            "Sub-Saharan Africa", "South Asia")
legend("left", inset= .03, title=" ",
       lgd.txt, fill=brewer.pal(n = 7, name = 'Set1'),
       horiz=FALSE, cex=0.6,
       text.width = NA, bty="n")
