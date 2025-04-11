library(dplyr)
library(tidyverse)
library(readr)
library(ggcorrplot)
library(factoextra)
library(ggplot2)
library(ggrepel)
library(formattable)
library(Gifi)
library(plotly)
library(htmlwidgets)


# data
final_data <- read_csv("https://raw.githubusercontent.com/depa-tto/Multidimensional-Data-Visualization-exam/refs/heads/main/dataset_raw.csv")
head(final_data)
target <- final_data[, 1]
final_data <- final_data[, -1]

# descriptive analysis
rho <- cor(final_data)
ggcorrplot(rho,
  hc.order = TRUE, outline.color = "white", ggtheme = ggplot2::theme_minimal(),
  colors = c("#1B80BF", "white", "#FC4E07"), lab = T, legend.title = "Correlation"
)

# pca
pca <- prcomp(final_data, scale = TRUE)
summary(prcomp(final_data, scale = TRUE))

# eingenvalues/eigenvectors, the eigenvectors are scale so the SS = 1
eigenvectors <- pca$loadings
eigenvalues <- pca$sdev * pca$sdev


# scree plot
plot(pca, type = "l", main = "Variance explained by PCA")
abline(h = 1, lwd = 3, col = "#FC4E07")

fviz_eig(pca,
  addlabels = TRUE, barfill = "#4169E1", barcolor = "black",
  linecolor = "#041C34"
) + theme_classic()

# biplot
fviz_pca_biplot(pca, label = "var", repel = TRUE, geom = "point", invisible = "ind") +
  geom_label_repel(label = colnames(final_data), max.overlaps = Inf, size = 3) + theme_classic()



ind_coord <- as.data.frame(pca$x[, 1:2])
ind_coord$Country <- target$Country
p <- fviz_pca_biplot(pca,
  label = "var",
  repel = TRUE,
  geom.ind = "point",
  col.ind = "#041C34",
  col.var = "#4169E1",
  invisible = "none"
) + theme_classic()
p + geom_text_repel(
  data = ind_coord,
  aes(x = PC1, y = PC2, label = Country),
  size = 3
)


# plot of the loadings
fviz_pca_var(pca,
  title = "Loadings", xlab = "Component 1", ylab = "Component 2",
  geom = c("point"), col.var = "black", alpha.var = 1, col.circle = "black"
) +
  geom_label_repel(force = 25, label = colnames(final_data), max.overlaps = Inf, size = 3) +
  theme_classic()


fviz_pca_var(pca,
  title = "Loadings", xlab = "Component 1", ylab = "Component 2",
  col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
  repel = TRUE, legend.title = "Contribution"
) + theme_classic()

# principal components scores
pca$x
components <- pca$x[, 1:2]
head(components)

# ggplot(as.data.frame(components), aes(PC1, PC2, col = middle_data$`Type of Travel`, fill = middle_data$`Type of Travel`)) +
# stat_ellipse(geom = 'polygon', col = 'black', alpha = 0.5) + geom_point(shape = 20, col = 'black')


# MANUAL METHOD

n <- nrow(final_data)
p <- ncol(final_data)

# mean and std:
mean <- colMeans(final_data)
sigma <- apply(final_data, 2, sd)
descriptive <- round(cbind(mean, sigma), 2)
colnames(descriptive) <- c("Mean", "Sigma")
descriptive


# PCA start from correlation matrix
# calculate matrix R:
rho <- cor(final_data)
round(rho, 3)

# calculate eigenvalues and eigenvectors:
eigen(rho)
autoval <- eigen(rho)$values
autovec <- eigen(rho)$vectors

# From the eigenvalue analysis, the only main components of relevance are the first and second
# (the others, in fact, have eigenvalues, therefore variance, less than 1).
# Let's see what happens if you choose the number of principal components based on the percentage of
# explained variance:
pvarsp <- autoval / p
pvarspcum <- cumsum(pvarsp)
tab <- round(cbind(autoval, pvarsp * 100, pvarspcum * 100), 3)
colnames(tab) <- c("Eigenvelues", "Variance(%)", "Cumulative variance(%)")
tab


sign_formatter_gt1 <- formatter(
  "span",
  style = function(x) {
    col <- ifelse(x > 1, "green",
      ifelse(x < 1, "#FC4E07", "black")
    )
    sprintf("color: %s", col)
  }
)

formattable(as.data.frame(tab),
  align = c("r", "r", "r"),
  list(
    "Eigenvelues" = sign_formatter_gt1,
    "Variance(%)" = color_tile("transparent", "lightblue"),
    "Cumulative variance(%)" = color_tile("transparent", "lightblue")
  )
)


# Use Scree Diagram to select the components:
plot(autoval, type = "b", main = "Scree plot", xlab = "Number of Component", ylab = "Eigenvalues")
abline(h = 1, lwd = 3, col = "#FC4E07")

# We select two components
# Interpret the principal components selected by their coefficient vectors:
eigen(rho)$vectors[, 1:3]

# Matrix of the components, obtained by multiplying the eigenvector by the root of the respective eigenvalue
comp <- round(cbind(
  -eigen(rho)$vectors[, 1] * sqrt(autoval[1]), eigen(rho)$vectors[, 2] * sqrt(autoval[2])
), 3)
rownames(comp) <- row.names(descriptive)
colnames(comp) <- c("Component 1", "Component 2")
comp

# The sum of the squares of the values of each row of the component matrix is the respective 'communality',
# The communality is the sum of the squared component loadings up to the number of components you extract.
communality <- comp[, 1]^2 + comp[, 2]^2
comp <- round(cbind(comp, communality), 3)
colnames(comp) <- c("Component 1", "Component 2", "Communality")
comp


sign_formatter_gt0 <- formatter(
  "span",
  style = function(x) {
    col <- ifelse(x > 0, "green",
      ifelse(x < 0, "#FC4E07", "black")
    )
    sprintf("color: %s", col)
  }
)


formattable(as.data.frame(comp),
  align = c("r", "r", "r"),
  list(
    "Component 1" = sign_formatter_gt0,
    "Component 2" = sign_formatter_gt0,
    "Communality" = color_tile("transparent", "lightblue")
  )
)



# Calculate the scores for the selected components and graph them:
final_data.scale <- scale(final_data, T, T)
score <- final_data.scale %*% autovec[, 1:3]

# normalized scores changed sign (non-normalized scores divided by square root of the respective eigenvalue)
# score chart
scorez <- round(cbind(
  -score[, 1] / sqrt(autoval[1]), -score[, 2] / sqrt(autoval[2]),
  -score[, 3] / sqrt(autoval[3])
), 2)

# plot(scorez, main="Scores plot",
#     xlab="comp1",ylab="comp2")
# text(scorez, rownames(final_data))
# abline(v=0,h=0,col="red")
# Loadings plot
plot(comp[, 1:3],
  main = "Loadings plot",
  xlab = "comp1", ylab = "comp2", xlim = range(-1, 1)
)
text(comp, rownames(comp))
abline(v = 0, h = 0, col = "#FC4E07")



##################################################################################
# three components
# We select three components
# Interpret the principal components selected by their coefficient vectors:
eigen(rho)$vectors[, 1:3]

# Matrix of the components, obtained by multiplying the eigenvector by the root of the respective eigenvalue
comp <- round(cbind(
  -eigen(rho)$vectors[, 1] * sqrt(autoval[1]), eigen(rho)$vectors[, 2] * sqrt(autoval[2]),
  eigen(rho)$vectors[, 3] * sqrt(autoval[3])
), 3)
rownames(comp) <- row.names(descriptive)
colnames(comp) <- c("Component 1", "Component 2", "Component 3")
comp

# The sum of the squares of the values of each row of the component matrix is the respective 'communality',
# The communality is the sum of the squared component loadings up to the number of components you extract.
communality <- comp[, 1]^2 + comp[, 2]^2 + comp[, 3]^2
comp <- round(cbind(comp, communality), 3)
colnames(comp) <- c("Component 1", "Component 2", "Component 3", "Communality")
comp


formattable(as.data.frame(comp),
  align = c("r", "r", "r"),
  list(
    "Component 1" = sign_formatter_gt0,
    "Component 2" = sign_formatter_gt0,
    "Component 3" = sign_formatter_gt0,
    "Communality" = color_tile("transparent", "lightblue")
  )
)



# Calculate the scores for the selected components and graph them:
final_data.scale <- scale(final_data, T, T)
score <- final_data.scale %*% autovec[, 1:3]

# normalized scores changed sign (non-normalized scores divided by square root of the respective eigenvalue)
# score chart
scorez <- round(cbind(
  - score[, 1] / sqrt(autoval[1]), score[, 2] / sqrt(autoval[2]),
  score[, 3] / sqrt(autoval[3])
), 2)

# plot(scorez, main="Scores plot",
#     xlab="comp1",ylab="comp2")
# text(scorez, rownames(final_data))
# abline(v=0,h=0,col="red")
# Loadings plot
plot(comp[, 1:3],
  main = "Loadings plot",
  xlab = "comp1", ylab = "comp2", xlim = range(-1, 1)
)
text(comp, rownames(comp))
abline(v = 0, h = 0, col = "#FC4E07")



####################################################################################

loadings_df <- as.data.frame(comp[, 1:3])
colnames(loadings_df) <- c("PC1", "PC2", "PC3")
loadings_df$Feature <- rownames(comp)

label_offset <- 1.15
fig_loadings <- plot_ly()

for (i in seq_len(nrow(loadings_df))) {
  fig_loadings <- fig_loadings %>%
    add_trace(
      type = "scatter3d",
      mode = "lines",
      x = c(0, loadings_df$PC1[i]),
      y = c(0, loadings_df$PC2[i]),
      z = c(0, loadings_df$PC3[i]),
      line = list(color = "#4169E1", width = 4),
      showlegend = FALSE
    )
}

fig_loadings <- fig_loadings %>%
  add_trace(
    type = "scatter3d",
    mode = "text",
    x = loadings_df$PC1 * label_offset,
    y = loadings_df$PC2 * label_offset,
    z = loadings_df$PC3 * label_offset,
    text = loadings_df$Feature,
    textfont = list(size = 12, color = "black"),
    showlegend = FALSE
  )

fig_loadings <- fig_loadings %>%
  layout(
    title = "3D Loadings Plot",
    scene = list(
      xaxis = list(title = "Component 1"),
      yaxis = list(title = "Component 2"),
      zaxis = list(title = "Component 3")
    )
  )

fig_loadings

saveWidget(fig_loadings, file = "3D_loadings.html", selfcontained = TRUE)




scorez_df <- as.data.frame(scorez)
scorez_df$Country <- target$Country

fig_full <- plot_ly()

fig_full <- fig_full %>%
  add_trace(
    data = scorez_df,
    x = ~V1, y = ~V2, z = ~V3,
    type = "scatter3d", mode = "markers+text",
    text = ~Country,
    textposition = "top center",
    marker = list(size = 4, color = "#4169E1"),
    name = "Scores"
  )

for (i in seq_len(nrow(loadings_df))) {
  fig_full <- fig_full %>%
    add_trace(
      type = "scatter3d", mode = "lines+text",
      x = c(0, loadings_df[i, 1]),
      y = c(0, loadings_df[i, 2]),
      z = c(0, loadings_df[i, 3]),
      line = list(color = "black", width = 4),
      text = loadings_df$Feature[i],
      textposition = "top center",
      showlegend = FALSE
    )
}

fig_full <- fig_full %>%
  layout(
    title = "3D PCA Biplot (Scores + Loadings)",
    scene = list(
      xaxis = list(title = "Component 1"),
      yaxis = list(title = "Component 2"),
      zaxis = list(title = "Component 3")
    )
  )

fig_full


saveWidget(fig_full, file = "3D_full.html", selfcontained = TRUE)
