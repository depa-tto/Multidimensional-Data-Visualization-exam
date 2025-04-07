library(dplyr)
library(tidyverse)
library(readr)
library(ggcorrplot)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(formattable)
library(Gifi)


# data
final_data <- read_csv("https://raw.githubusercontent.com/depa-tto/Multidimensional-Data-Visualization-exam/refs/heads/main/dataset_raw.csv")
head(final_data)
final_data <- final_data[,-1]

# descriptive analysis
rho <- cor(final_data)
ggcorrplot(rho, hc.order = TRUE, outline.color = "white", ggtheme = ggplot2::theme_minimal(),
           colors = c("#1B80BF", "white", "#FC4E07"), lab = T, legend.title = 'Correlation') 

# pca
pca <- prcomp(final_data, scale = TRUE)
summary(prcomp(final_data, scale = TRUE))

# eingenvalues/eigenvectors, the eigenvectors are scale so the SS = 1
eigenvectors <- pca$loadings
eigenvalues <- pca$sdev * pca$sdev


# scree plot
plot(pca, type = 'l', main = 'Variance explained by PCA')
abline(h=1, lwd=3, col="#FC4E07")

fviz_eig(pca, addlabels=TRUE, barfill="#1B80BF", barcolor ="black",
              linecolor ="#FC4E07") + theme_classic()

# biplot
fviz_pca_biplot(pca, label="var", repel = TRUE, geom = "point", invisible ="ind") + 
  geom_label_repel(label = colnames(final_data), max.overlaps=Inf, size = 3) + theme_classic()

# plot of the loadings
fviz_pca_var(pca, title = 'Loadings', xlab = 'Component 1', ylab = 'Component 2', 
             geom = c("point"), col.var = "black", alpha.var = 1, col.circle = "black") + 
             geom_label_repel(force = 25,label = colnames(final_data), max.overlaps = Inf, size = 3) + 
             theme_classic()


fviz_pca_var(pca, title = 'Loadings', xlab = 'Component 1', ylab = 'Component 2',
             col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE, legend.title = 'Contribution') + theme_classic()

# principal components scores
pca$x
components <- pca$x[,1:2]
head(components)

# ggplot(as.data.frame(components), aes(PC1, PC2, col = middle_data$`Type of Travel`, fill = middle_data$`Type of Travel`)) + 
# stat_ellipse(geom = 'polygon', col = 'black', alpha = 0.5) + geom_point(shape = 20, col = 'black')


# MANUAL METHOD

n <- nrow(final_data)
p <- ncol(final_data)

# mean and std:
mean <- colMeans(final_data)
sigma <- apply(final_data, 2, sd)
descriptive <- round(cbind(mean, sigma),2)
colnames(descriptive) <-c ("Mean", "Sigma")
descriptive

formattable(as.data.frame(descriptive), caption = "Fig. 1.1: descriptive analysis, mean and standard deviation of each variable",
            align = c ("r", "r", "r"), list('Mean' = color_tile("transparent", "lightblue"))) 

# PCA start from correlation matrix
# calculate matrix R:
rho <- cor(final_data)
round(rho,3)

# calculate eigenvalues and eigenvectors:
eigen(rho)
autoval <- eigen(rho)$values
autovec <- eigen(rho)$vectors

# From the eigenvalue analysis, the only main components of relevance are the first and second 
# (the others, in fact, have eigenvalues, therefore variance, less than 1). 
# Let's see what happens if you choose the number of principal components based on the percentage of
# explained variance:
pvarsp = autoval/p
pvarspcum = cumsum(pvarsp)
tab <- round(cbind(autoval,pvarsp*100,pvarspcum*100),3)
colnames(tab) <-c ("Eigenvelues", "Variance(%)","Cumulative variance(%)")
tab


sign_formatter_gt1 <- formatter("span", style = x ~ style(color = ifelse(x > 1, "green", 
                                                                     ifelse(x < 1, "#FC4E07", "black"))))

formattable(as.data.frame(tab), caption = "Fig. 1.3: eigenvalues and variance explained",
            align = c ("r", "r", "r"), 
            list('Eigenvelues' = sign_formatter_gt1, 'Variance(%)' = color_tile("transparent", "lightblue"),
            'Cumulative variance(%)' = color_tile("transparent", "lightblue")))


# Use Scree Diagram to select the components:
plot(autoval, type="b", main = "Fig. 1.4: scree plot", xlab = "Number of Component", ylab = "Eigenvalues")
abline(h=1, lwd=3, col="#FC4E07")

# We select three components 
# Interpret the principal components selected by their coefficient vectors:
eigen(rho)$vectors[,1:3]

# Matrix of the components, obtained by multiplying the eigenvector by the root of the respective eigenvalue 
comp <- round(cbind(eigen(rho)$vectors[,1]*sqrt(autoval[1]), eigen(rho)$vectors[,2]*sqrt(autoval[2]), 
                    eigen(rho)$vectors[,3]*sqrt(autoval[3])), 3)
rownames(comp) <- row.names(descriptive)
colnames(comp) <- c('Component 1','Component 2', 'Component 3')
comp

# The sum of the squares of the values of each row of the component matrix is the respective 'communality', 
# The communality is the sum of the squared component loadings up to the number of components you extract.
communality <- comp[,1]^2 + comp[,2]^2 + comp[,3]^2
comp <- round(cbind(comp, communality),3)
colnames(comp) <-c ('Component 1','Component 2', 'Component 3', 'Communality')
comp

sign_formatter_gt0 <- formatter("span", style = x ~ style(color = ifelse(x > 0, "green", 
                                                                     ifelse(x < 0, "#FC4E07", "black"))))

formattable(as.data.frame(comp), align = c ("r", "r", "r"), 
            list('Component 1' = sign_formatter_gt0,
            'Component 2' = sign_formatter_gt0,
            'Component 3' = sign_formatter_gt0,
            'Communality' = color_tile("transparent", "lightblue")))



# Calculate the scores for the selected components and graph them:
final_data.scale <- scale(final_data, T, T)
score <- final_data.scale %*% autovec[,1:3]

# normalized scores changed sign (non-normalized scores divided by square root of the respective eigenvalue)
# score chart
scorez <- round(cbind(-score[,1]/sqrt(autoval[1]),-score[,2]/sqrt(autoval[2]),
                      -score[,3]/sqrt(autoval[3])),2)

# plot(scorez, main="Scores plot",
#     xlab="comp1",ylab="comp2")
# text(scorez, rownames(final_data))
# abline(v=0,h=0,col="red")
# Loadings plot
plot(comp[,1:3], main = "Loadings plot",
     xlab="comp1", ylab="comp2", xlim=range(-1,1))
text(comp, rownames(comp))
abline(v = 0, h = 0, col = "#FC4E07")



####################################################################################################

# We select two components 
# Interpret the principal components selected by their coefficient vectors:
eigen(rho)$vectors[,1:2]

# Matrix of the components, obtained by multiplying the eigenvector by the root of the respective eigenvalue 
comp <- round(cbind(eigen(rho)$vectors[,1]*sqrt(autoval[1]), eigen(rho)$vectors[,2]*sqrt(autoval[2])),3)
rownames(comp) <- row.names(descriptive)
colnames(comp) <-c ("Component 1","Component 2")
comp

# The sum of the squares of the values of each row of the component matrix is the respective 'communality', 
# The communality is the sum of the squared component loadings up to the number of components you extract.
communality <- comp[,1]^2+comp[,2]^2
comp <- round(cbind(comp, communality), 3)
colnames(comp) <-c ("Component 1", "Component 2","Communality")
comp

sign_formatter_gt0 <- formatter("span", style = x ~ style(color = ifelse(x > 0, "green", 
                                                                         ifelse(x < 0, "#FC4E07", "black"))))

formattable(as.data.frame(comp), align = c ("r", "r", "r"), list('Component 1' = sign_formatter_gt0,
                                                                 'Component 2' = sign_formatter_gt0,
                                                                 'Communality' = color_tile("transparent", "lightblue")))



# Calculate the scores for the selected components and graph them:

final_data.scale <- scale(final_data, T, T)
score <- final_data.scale%*%autovec[,1:2]

# normalized scores changed sign (non-normalized scores divided by square root of the respective eigenvalue)
# score chart

scorez<-round(cbind(-score[,1]/sqrt(autoval[1]),-score[,2]/sqrt(autoval[2])),2)
# plot(scorez, main="Scores plot",
# xlab="comp1",ylab="comp2")
# text(scorez, rownames(final_data))
# abline(v=0,h=0,col="red")

# Loadings plot

plot(comp[,1:2], main="Loadings plot",
     xlab = "comp1", ylab = "comp2", xlim = range(-1,1))
text(comp, rownames(comp))
abline(v=0,h=0,col="#FC4E07")



x <- final_data %>% select(`Seat comfort`, `On-board service`, `Baggage handling`,
                      `Food and drink`,`Inflight wifi service`,`Inflight entertainment`) %>% 
  summarise(
    Seat = mean(`Seat comfort`, na.rm = TRUE),
    Cabin = mean(`On-board service`, na.rm = TRUE),
    Food = mean(`Food and drink`, na.rm = TRUE),
    Ground = mean(`Baggage handling`, na.rm = TRUE),
    Inflight = mean(`Inflight entertainment`, na.rm = TRUE),
    Wifi = mean(`Inflight wifi service`, na.rm = TRUE)
  )


formattable(as.data.frame(x), caption = "Fig. 2.5: means of the airline company",
            align = c ("r", "r", "r"), list('Cabin' = color_tile("transparent", "lightblue"),
                                            'Seat' = color_tile("transparent", "lightblue"),
                                            'Food' = color_tile("transparent", "lightblue"),
                                            'Ground' = color_tile("transparent", "lightblue"),
                                            'Inflight' = color_tile("transparent", "lightblue"),
                                            'Wifi' = color_tile("transparent", "lightblue")))








