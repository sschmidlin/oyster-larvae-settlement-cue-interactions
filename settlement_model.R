library(ggplot2)

# Figure A: additive model

# Define a vector with all possible values of the positive cue (from 0 to 1)
A=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the positive cue
func1 <- function(x) {
  x
}
A=sapply(A, func1)
# Define a correction factor for the positive cue
k1=4

# Define a vector with all possible values of the positive cue (from 0 to -1)
B=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the negative cue
func2 <- function(x) {
  x
}
B=sapply(B, func2)
summary(B)
# Define a correction factor for the positive cue
k2=4

# Generate a matrix with all possible combinations of positive and negative cue values
L = matrix(nrow = length(A), ncol = length(B))

# Run the model
for (i in 1:length(A)) {
  for (j in 1:length(B)) {
    L[i,j] = 1 / (1 + exp(-(A[i]*k1 - B[j]*k2))) # first cue positive, second cue negative
    #L[i,j] = 1 / (1 + exp(-(A[i]*k1 + B[j]*k2))) # both cues positive
  }
}

# Change the format of the result matrix for printing
df <- reshape2::melt(L)

# Print the model result
figureA = ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", mid = "lightblue", high = "green", midpoint=0.5) +
  labs(x = "Strength of negative cue", y = "Strength of positive cue", fill="Probability of settlement") +
  coord_fixed() +
  geom_contour(aes(z=value), colour = "white") +
  ggtitle("Equal ranking: Weighted choice") +
  theme_minimal()

                    
# Figure B: ranking model, optimistic scenario

# Define a vector with all possible values of the positive cue (from 0 to 1)
A=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the positive cue
func1 <- function(x) {
  exp(x)
}
A=sapply(A, func1)
# Define a correction factor for the positive cue
k1=1
 
# Define a vector with all possible values of the positive cue (from 0 to -1)
B=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the negative cue
func2 <- function(x) {
  x
}
B=sapply(B, func2)
summary(B)
# Define a correction factor for the positive cue
k2=2
                    
# Generate a matrix with all possible combinations of positive and negative cue values
L = matrix(nrow = length(A), ncol = length(B))
                    
# Run the model
for (i in 1:length(A)) {
  for (j in 1:length(B)) {
    L[i,j] = 1 / (1 + exp(-(A[i]*k1 - B[j]*k2))) # first cue positive, second cue negative
                        #L[i,j] = 1 / (1 + exp(-(A[i]*k1 + B[j]*k2))) # both cues positive
  }
}
                    
# Change the format of the result matrix for printing
df <- reshape2::melt(L)
                    
# Print the model result
figureB = ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", mid = "lightblue", high = "green", midpoint=0.5) +
  labs(x = "Strength of negative cue", y = "Strength of positive cue", fill="Probability of settlement") +
  coord_fixed() +
  geom_contour(aes(z=value), colour = "white") +
  ggtitle("Highly ranked pos. cue: Optimistic choice") +
  theme_minimal()


# Figure C: ranking model, pessimistic scenario

# Figure B: ranking model

# Define a vector with all possible values of the positive cue (from 0 to 1)
A=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the positive cue
func1 <- function(x) {
  log(x)
}
A=sapply(A, func1)
# Define a correction factor for the positive cue
k1=2

# Define a vector with all possible values of the positive cue (from 0 to -1)
B=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the negative cue
func2 <- function(x) {
  x
}
B=sapply(B, func2)
summary(B)
# Define a correction factor for the positive cue
k2=2

# Generate a matrix with all possible combinations of positive and negative cue values
L = matrix(nrow = length(A), ncol = length(B))

# Run the model
for (i in 1:length(A)) {
  for (j in 1:length(B)) {
    L[i,j] = (1 / (1 + exp(-(A[i]*k1 - B[j]*k2))))*2 # first cue positive, second cue negative
    #L[i,j] = 1 / (1 + exp(-(A[i]*k1 + B[j]*k2))) # both cues positive
  }
}

# Change the format of the result matrix for printing
df <- reshape2::melt(L)

# Print the model result
figureC = ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", mid = "lightblue", high = "green", midpoint=0.5) +
  labs(x = "Strength of negative cue", y = "Strength of positive cue", fill="Probability of settlement") +
  coord_fixed() +
  geom_contour(aes(z=value), colour = "white") +
  ggtitle("Highly ranked neg. cue: Pessimistic choice") +
  theme_minimal()


# Figure D: two positive cues
# Figure B: ranking model

# Define a vector with all possible values of the positive cue (from 0 to 1)
A=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the positive cue
func1 <- function(x) {
  x
}
A=sapply(A, func1)
# Define a correction factor for the positive cue
k1=4

# Define a vector with all possible values of the positive cue (from 0 to -1)
B=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the negative cue
func2 <- function(x) {
  x
}
B=sapply(B, func2)
summary(B)
# Define a correction factor for the positive cue
k2=4

# Generate a matrix with all possible combinations of positive and negative cue values
L = matrix(nrow = length(A), ncol = length(B))

# Run the model
for (i in 1:length(A)) {
  for (j in 1:length(B)) {
    #L[i,j] = 1 / (1 + exp(-(A[i]*k1 - B[j]*k2))) # first cue positive, second cue negative
    L[i,j] = 1 / (1 + exp(-(A[i]*k1 + B[j]*k2))) # both cues positive
  }
}

# Change the format of the result matrix for printing
df <- reshape2::melt(L)

# Print the model result
figureD = ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", mid = "lightblue", high = "green", midpoint=0.5) +
  labs(x = "Strength of first positive cue", y = "Strength of second cue", fill="Probability of settlement") +
  coord_fixed() +
  geom_contour(aes(z=value), colour = "white") +
  ggtitle("Two additive positive cues") +
  theme_minimal()

library(gridExtra)
pdf("Figure7.pdf", width = 12, height = 8)
grid.arrange(figureA, figureB, figureC, figureD)
dev.off()                  
                    
getwd()                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
# Old code. Do not run.                    

library(plotly)

plot_ly(df, x = Var2, y = Var1, z = value, type = "surface", 
        colors = colorRamp(c("darkblue", "lightblue", "green")), 
        colorbar = list(title = "Probability of settlement"), 
        contours = list(z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "limegreen", project = list(z = TRUE)))) %>%
  layout(scene = list(xaxis = list(title = "Strength of negative cue"), 
                      yaxis = list(title = "Strength of positive cue"), 
                      zaxis = list(title = "Value")), 
         title = "3D Surface Plot with Probability of Settlement")



library(ggplot2)

# Define a vector with all possible values of the positive cue (from 0 to 1)
A=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the positive cue
func1 <- function(x) {
  x
}
A=sapply(A, func1)
# Define a correction factor for the positive cue
k1=1

# Define a vector with all possible values of the positive cue (from 0 to -1)
B=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the negative cue
func2 <- function(x) {
  x
}
B=sapply(B, func2)
summary(B)
# Define a correction factor for the positive cue
k2=1

# Define a vector with all possible values of the positive cue (from 0 to -1)
C=seq(from = 0, to = 1, by = 0.01)
# Define a function (linear, exponential, etc.) for the negative cue
func3 <- function(x) {
  x
}
C=sapply(C, func3)
summary(C)
# Define a correction factor for the positive cue
k3=1

# Generate a matrix with all possible combinations of positive and negative cue values
L <- array(NA, dim = c(length(A), length(B), length(C)))

# Run the model
for (i in 1:length(A)) {
  for (j in 1:length(B)) {
    for (k in 1:length(C)) {
    L[i,j,k] = 1 / (1 + exp(-(A[i]*k1 - B[j]*k2 + C[k]*k3))) # first cue positive, second cue negative
    #L[i,j] = 1 / (1 + exp(-(A[i]*k1 + B[j]*k2))) # both cues positive
    }
  }
}

# Change the format of the result matrix for printing
require(reshape2)
L.matrix <- aperm(L, c(1, 3, 2))
L.melted <- melt(L.matrix, varnames = c("A", "C", "B"), value.name = "value")


library(plotly)
library(tidyr)

L.df <- data.frame(expand.grid(A = A, B = B, C = C))
L.df$value <- as.vector(L)

L.mat <- matrix(L.df$value, nrow = length(B), ncol = length(A), byrow = TRUE)


# Create a 3D scatter plot with color-coded points
fig <- plot_ly(data = L.df, x = ~A, y = ~B, z = ~C, color = ~value,
               colors = colorRamp(c("blue", "green", "yellow")),
               marker = list(size = 2)) %>% 
  add_surface(z = L.mat[, , 1], colors = "gray", opacity = 0.5) %>% # add surface at 0-0-0
  add_surface(z = L.mat[, , ncol(L.mat)], colors = "gray", opacity = 0.5) # add surface at 1-1-1

# Show the plot
fig


# Create a 3D array L
A <- c(1, 2, 3)
B <- c(4, 5, 6)
C <- c(7, 8, 9)

L <- array(NA, dim = c(length(A), length(B), length(C)))

for (i in 1:length(A)) {
  for (j in 1:length(B)) {
    for (k in 1:length(C)) {
      L[i, j, k] <- A[i] + B[j] + C[k]
    }
  }
}

# Convert L to a long format using tidyr::gather
L.df <- data.frame(expand.grid(A = A, B = B, C = C))
L.df$value <- as.vector(L)

# Create a 3D scatter plot with color-coded points
fig <- plot_ly(data = L.df, x = ~A, y = ~B, z = ~C, color = ~value,
               colors = colorRamp(c("blue", "green", "yellow")),
               marker = list(size = 1)) %>%
  add_markers()

# Show the plot
fig



s3d <- scatterplot3d(x = L.melted$A, y = L.melted$B, z = L.melted$C, color = L.melted$value,
                     main = "3D Scatter Plot", pch = 16, type = "h", lwd = 0.1)

# Add color bar to the plot
s3d$points3d(x = NA, y = NA, z = NA, col = L.melted$value, pch = 16)
s3d$legend3d("topright", legend = "Value", col = s3d$color.palette(L.melted$value), pch = 16)

plot(s3d)

plot_ly(L.melted, x = A, y = B, z = value, type = "surface", 
        colors = colorRamp(c("darkblue", "lightblue", "green")), 
        colorbar = list(title = "Probability of settlement"), 
        contours = list(z = list(show = TRUE, usecolormap = TRUE, highlightcolor = "limegreen", project = list(z = TRUE)))) %>%
  layout(scene = list(xaxis = list(title = "importance of negative cue"), 
                      yaxis = list(title = "importance of positive cue"), 
                      zaxis = list(title = "Value")), 
         title = "3D Surface Plot with Probability of Settlement")


# Print the model result
ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "darkblue", mid = "lightblue", high = "green", midpoint=0.5) +
  labs(x = "importance of negative cue", y = "importance of positive cue", fill="Probability of settlement") +
  coord_fixed() +
  geom_contour(aes(z=value), colour = "white") +
  theme_minimal()

