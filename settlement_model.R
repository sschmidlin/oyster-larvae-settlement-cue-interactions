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
ggplot(df, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(x = "importance of second positive cue", y = "importance of first positive cue", fill="Probability of settlement") +
  coord_fixed() +
  geom_contour(aes(z=value), colour = "white") +
  theme_minimal()
