#figures-side, fig.show="hold", out.width="50%"
correlation_categories_without_happy <- c("Economy","Social","Health","Freedom","Corruption","Generosity","Positive","Negative","Government","Alcohol","Population","Tobacco","Internet")

par(mar = c(4, 4, .1, .1))
pca <- prcomp(scaled_data_factors[,correlation_categories_without_happy],scale = TRUE)
pca$rotation[,1:2]

pc1_exp <- round((pca$sdev[1]^2/ sum(pca$sdev^2))*100,2)
pc2_exp <- round((pca$sdev[2]^2/ sum(pca$sdev^2))*100,2)

happiness_category <- round(not_scaled_data_factors$Happiness)

region_category <- as.factor(data_2018$Region)

p2 <- ggplot(newdata, aes(Health, Happiness, colour = Region)) +
  geom_point(alpha = 0.9, show.legend = FALSE, size = 2) +
  scale_colour_manual(values = rainbow(10))


pal <- function(n) viridis(n)
plot(happy_SOM_model, shape="straight", palette.name=pal(10))


pca_plot_data <- data.frame(pca$x[,1:2])
pca_plot_data$happiness <- as.factor(happiness_category)
pca_plot_data$code <- data_2018$Code

plot(pca$x, xlab= paste("PC1: ", pc1_exp, "%") , ylab= paste("PC2: ", pc2_exp,"%"),    pch=16,col =happiness_category, ) #type="n", 
text(pca$x[,1],pca$x[,2]-0.1, labels =data_2018$Code, cex=0.6)
legend("topright", legend=sort(unique(happiness_category), decreasing = TRUE), pch=16,col = sort(unique(happiness_category), decreasing = TRUE))


require(ggalt)

ggplot(pca_plot_data, aes(x= PC1, y= PC2, colour=happiness, label=code ))+
  scale_color_viridis(discrete=TRUE) +
  geom_point(size=14,shape=16) +
  geom_text(hjust=+0.5, vjust=+0.5, size=4, show.legend = FALSE, col="white") +
  stat_ellipse(aes(x=PC1, y=PC2, color=happiness, group=happiness),type = "norm",size=2) +
  xlab(paste("PC1: ", pc1_exp, "%")) +
  ylab(paste("PC2: ", pc2_exp, "%"))




ggplot(pca_plot_data, aes(x= PC1, y= PC2, colour=happiness, label=code ))+
  scale_color_viridis(discrete=TRUE) +
  geom_point(size=14,shape=16) +
  geom_text(hjust=+0.5, vjust=+0.5, size=4, show.legend = FALSE, col="white") +
  #stat_ellipse(aes(x=PC1, y=PC2, color=happiness, group=happiness),type = "norm",size=2) +
  geom_encircle(aes(group=happiness), color="black")+ 
  xlab(paste("PC1: ", pc1_exp, "%")) +
  ylab(paste("PC2: ", pc2_exp, "%"))


library(ggforce)


ggplot(pca_plot_data, aes(PC1, PC2)) +
  geom_mark_ellipse(aes(fill = happiness, label = happiness)) +
  geom_point()


ggplot(pca_plot_data, aes(x= PC1, y= PC2)) + 
  geom_voronoi_tile(aes(fill = happiness, group = happiness)) + 
  geom_voronoi_segment() +
  geom_point()



ggplot(pca_plot_data, aes(PC1, PC2)) +
  geom_delaunay_tile(alpha = 0.3) + 
  geom_delaunay_segment2(aes(colour = happiness, group = -1), size = 2,
                         lineend = 'round')


scale_color_viridis()


pca_plot <- ggplot(pca_plot_data, aes(x= PC1, y= PC2, colour=happiness, label=code ))+
  scale_color_viridis(end = 0.95, discrete=TRUE) +
  ggforce::geom_mark_ellipse(inherit.aes = FALSE ,aes(x=PC1, y=PC2, group = happiness, color = happiness,  label = happiness)
                             , tol = 0.001, alpha=0.1) +
  geom_point(size=14,shape=16) +
  geom_text(hjust=+0.5, vjust=+0.5, size=4, show.legend = FALSE, col="white") +
  xlab(paste("PC1: ", pc1_exp, "%")) +
  ylab(paste("PC2: ", pc2_exp, "%")) +

  guides(colour=guide_legend(title="Happiness- \n score")) +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.8)+
  geom_vline(xintercept=0, linetype="dashed", color= "black", size = 0.8)+
  ggtitle("PCA colored by happiness") +
  theme(axis.title.y = element_text(size = 18, family = "sans"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        axis.text.x = element_text(colour ="black", size = 16, family = "sans"),
        axis.text.y = element_text(colour ="black", size = 16, family = "sans"),
        axis.title.x = element_text(colour = "black", size = 18, family = "sans"),
        panel.background = element_blank(),
        plot.title = element_text(size = 20, family = "sans", margin=margin(b = 20, unit = "pt")),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
        )

pca_plot
  
ggsave(file="pcaplot.svg", plot=pca_plot, width=20, height=16)



biplot <-ggbiplot(pca, obs.scale=1, var.scale=1, groups=as.factor(happiness_category),  circle=TRUE) +
  scale_color_viridis(end = 0.95, discrete=TRUE) +
  geom_point(aes(colour=as.factor(happiness_category)), size = 12, shape=16) + #plotting over to increase the point size
  geom_text(aes(label=pca_plot_data$code),hjust=+0.5, vjust=+0.5, size=4, show.legend = FALSE, col="white") +
  guides(colour=guide_legend(title="Happiness- \n score")) +
  theme_minimal() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 0.8)+
  geom_vline(xintercept=0, linetype="dashed", color= "black", size = 0.8)+
  ggtitle("Biplot colored by happiness") +
  theme(axis.title.y = element_text(size = 18, family = "sans"),
        legend.position = "right",
        legend.title = element_text(size = 12, family = "sans"),
        axis.text.x = element_text(colour ="black", size = 16, family = "sans"),
        axis.text.y = element_text(colour ="black", size = 16, family = "sans"),
        axis.title.x = element_text(colour = "black", size = 18, family = "sans"),
        panel.background = element_blank(),
        plot.title = element_text(size = 20, family = "sans", margin=margin(b = 20, unit = "pt")),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
  )


ggsave(file="biplot.svg", plot=biplot, width=20, height=16)






    
# Plotting the output of FactoMineR's PCA using ggplot2
#
# load libraries
library(FactoMineR)
library(ggplot2)
library(scales)
library(grid)
library(plyr)
library(gridExtra)
#
# start with a clean slate
rm(list=ls(all=TRUE)) 
#
# load example data from the FactoMineR package
data(decathlon)
#
# compute PCA
res.pca <- PCA(decathlon, quanti.sup = 11:12, quali.sup=13, graph = FALSE)
#
# extract some parts for plotting
PC1 <- res.pca$ind$coord[,1]
PC2 <- res.pca$ind$coord[,2]
labs <- rownames(res.pca$ind$coord)
PCs <- data.frame(cbind(PC1,PC2))
rownames(PCs) <- labs
#
# Just showing the individual samples...
ggplot(PCs, aes(PC1,PC2, label=rownames(PCs))) + 
  geom_text() 
#
# Now get supplementary categorical variables
cPC1 <- res.pca$quali.sup$coor[,1]
cPC2 <- res.pca$quali.sup$coor[,2]
clabs <- rownames(res.pca$quali.sup$coor)
cPCs <- data.frame(cbind(cPC1,cPC2))
rownames(cPCs) <- clabs
colnames(cPCs) <- colnames(PCs)
#
# Put samples and categorical variables (ie. grouping
# of samples) all together
p <- ggplot() + theme(aspect.ratio=1) + theme_bw(base_size = 20) 
# no data so there's nothing to plot...
# add on data 
p <- p + geom_text(data=PCs, aes(x=PC1,y=PC2,label=rownames(PCs)), size=4) 
p <- p + geom_text(data=cPCs, aes(x=cPC1,y=cPC2,label=rownames(cPCs)),size=10)
p # show plot with both layers
#
# clear the plot
dev.off()
#
# Now extract variables
#
vPC1 <- res.pca$var$coord[,1]
vPC2 <- res.pca$var$coord[,2]
vlabs <- rownames(res.pca$var$coord)
vPCs <- data.frame(cbind(vPC1,vPC2))
rownames(vPCs) <- vlabs
colnames(vPCs) <- colnames(PCs)
#
# and plot them
#
pv <- ggplot() + theme(aspect.ratio=1) + theme_bw(base_size = 20) 
# no data so there's nothing to plot
# put a faint circle there, as is customary
angle <- seq(-pi, pi, length = 50) 
df <- data.frame(x = sin(angle), y = cos(angle)) 
pv <- pv + geom_path(aes(x, y), data = df, colour="grey70") 
#
# add on arrows and variable labels
pv <- pv + geom_text(data=vPCs, aes(x=vPC1,y=vPC2,label=rownames(vPCs)), size=4) + xlab("PC1") + ylab("PC2")
pv <- pv + geom_segment(data=vPCs, aes(x = 0, y = 0, xend = vPC1*0.9, yend = vPC2*0.9), arrow = arrow(length = unit(1/2, 'picas')), color = "grey30")
pv # show plot 
#
# clear the plot
dev.off()
#
# Now put them side by side
#
library(gridExtra)
grid.arrange(p,pv,nrow=1)
# 
# Now they can be saved or exported...
#
# tidy up by deleting the plots
#
dev.off()






library("ggbiplot")
data(wine)
wine.pca <- prcomp(wine,scale.=TRUE)
g<-ggbiplot(pca, obs.scale=1, var.scale=1, groups=as.factor(happiness_category),  circle=TRUE, labels =pca_plot_data$code )
g
g<-g+scale_color_discrete(name="")
g<-theme(legend.discription="horiz",legend.posion="top")
g
print(g)






library(kohonen)
library("viridis")
library("RColorBrewer")

som_2018 <- not_scaled_data_factors[,correlation_categories_without_happy]
som_mat <- as.matrix(som_2018)

col_palette <- viridis(7)[-1]

happy_grid <- somgrid(xdim = 6, ydim = 6, topo = "hexagonal")
happy_SOM_model <- som(X = som_mat, grid=happy_grid)

legend_categories= c(unique(sort(happiness_category)))

pal <- function(n) viridis(n)

mm <- cbind(c(1,1,1),c(2,3,4))
widths <- c(3,2); heights <- rep(1,1,1)
layout(mm, widths=widths, heights=heights)

plot(happy_SOM_model, shape="straight", palette.name=pal,)
plot(happy_SOM_model, type = "dist.neighbours", shape="straight")
plot(happy_SOM_model, type="mapping", pch=20, col = col_palette[as.integer(happiness_category)-2],shape = "straight",lw=5)
legend("right", inset= .03, title="Happiness-scores", legend=legend_categories, fill=col_palette, horiz=FALSE, cex=1, bty="n")
plot(happy_SOM_model, type="mapping", cex=0, col = col_palette[as.integer(happiness_category)-2],shape = "straight")
text( som2pts(happy_SOM_model)[,1]+ runif(95)*0.5-0.25, som2pts(happy_SOM_model)[,2]+ runif(95)*0.5-0.25 ,labels=pca_plot_data$code, cex=0.6)




som_2018 <- not_scaled_data_factors[,correlation_categories_without_happy]
som_mat <- as.matrix(som_2018)

col_palette <- viridis(7)[-1]

som2pts <- function(x){
  stopifnot("kohonen" %in% class(x))
  x$grid$pts[x$unit.classif,]
}

happy_grid <- somgrid(xdim = 6, ydim = 6, topo = "hexagonal")
happy_SOM_model <- som(X = som_mat, grid=happy_grid)

legend_categories= c(unique(sort(happiness_category)))

pal <- function(n) viridis(n)

mm <- cbind(c(1,1),c(2,3))
widths <- c(3,2); heights <- rep(1,1)
layout(mm, widths=widths, heights=heights)

plot(happy_SOM_model, shape="straight", palette.name=pal,)
plot(happy_SOM_model, type="mapping", pch=20, col = col_palette[as.integer(happiness_category)-2],shape = "straight",lw=5)
legend("right", inset= .000003, title="Happiness-scores", legend=legend_categories, fill=col_palette, horiz=FALSE, cex=1, bty="n")
plot(happy_SOM_model, type="mapping", cex=0, col = col_palette[as.integer(happiness_category)-2],shape = "straight")
text( som2pts(happy_SOM_model)[,1]+ runif(95)*0.5-0.25, som2pts(happy_SOM_model)[,2]+ runif(95)*0.5-0.25 ,labels=pca_plot_data$code, cex=0.6)






som_2018 <- scaled_data_factors[,correlation_categories_without_happy]
som_mat <- as.matrix(som_2018)

col_palette <- viridis(7)[-1]

som2pts <- function(x){
  stopifnot("kohonen" %in% class(x))
  x$grid$pts[x$unit.classif,]
}

happy_grid <- somgrid(xdim = 6, ydim = 6, topo = "hexagonal")
happy_SOM_model <- som(X = som_mat, grid=happy_grid)

legend_categories= c(unique(sort(happiness_category)))

pal <- function(n) viridis(n)

mm <- cbind(c(1,1,1),c(2,3,4))
widths <- c(3,2); heights <- rep(1,1,1)
layout(mm, widths=widths, heights=heights)

plot(happy_SOM_model, shape="straight", palette.name=pal,)
plot(happy_SOM_model, type = "dist.neighbours", shape="straight")
plot(happy_SOM_model, type="mapping", pch=20, col = col_palette[as.integer(happiness_category)-2],shape = "straight",lw=5)
legend("right", inset= .003, title="Happiness-scores", legend=legend_categories, fill=col_palette, horiz=FALSE, cex=1, bty="n")
plot(happy_SOM_model, type="mapping", cex=0, col = col_palette[as.integer(happiness_category)-2],shape = "straight")
text( som2pts(happy_SOM_model)[,1]+ runif(95)*0.5-0.25, som2pts(happy_SOM_model)[,2]+ runif(95)*0.5-0.25 ,labels=pca_plot_data$code, cex=0.6)


