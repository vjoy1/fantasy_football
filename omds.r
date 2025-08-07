# Load libraries
library(profootballref)
library(ggplot2)
library(MASS)
library(readr)
library(plotly)
library(scatterplot3d)

# Retrieve data for 2024 using profootballref
stats2024 <- profootballref::gen_tables(year = 2024)

# Print number of NA in each column and column names
print(colSums(is.na(stats2024)))
print(colnames(stats2024))

# Convert columns to appropriate types (numeric where possible)
stats2024_converted <- readr::type_convert(as.data.frame(stats2024))
print(colSums(is.na(stats2024_converted)))

# Remove players with less than 10 games
stats2024_converted <- stats2024_converted[stats2024_converted$Games_G > 10, ]

# Define columns to remove from numeric data
remove_columns <- c("RK", "Age", "Games_G", "Games_GS", "Rushing_Y/A",
                    "Receiving_Y/R", "Scoring_2PM", "Scoring_2PP",
                    "Fantasy_FantPt", "Fantasy_PPR", "Fantasy_DKPt",
                    "Fantasy_FDPt", "Fantasy_VBD", "Fantasy_PosRank",
                    "Fantasy_OvRank")

# Identify numeric columns and select those not in remove_columns
all_numeric <- sapply(stats2024_converted, is.numeric)
numeric_names <- names(stats2024_converted)[all_numeric]
selected_columns <- setdiff(numeric_names, remove_columns)
numeric_cols <- stats2024_converted[, selected_columns, drop = FALSE]

# Print length of numeric columns (number of variables)
print(length(numeric_cols))

# Remove rows with any NA values
complete_idx <- complete.cases(numeric_cols)
numeric_cols <- numeric_cols[complete_idx, , drop = FALSE]
stats2024_filtered <- stats2024_converted[complete_idx, , drop = FALSE]

# Remove rows that are entirely zeros in numeric_cols
zero_rows <- rowSums(numeric_cols != 0) == 0
if(any(zero_rows)) {
  print("Rows with all zeros:")
  print(numeric_cols[zero_rows, ])
}
numeric_cols <- numeric_cols[!zero_rows, , drop = FALSE]
stats2024_filtered <- stats2024_filtered[!zero_rows, , drop = FALSE]

# Remove duplicate rows
duplicate_rows <- duplicated(numeric_cols)
print("Duplicate rows:")
print(numeric_cols[duplicate_rows, ])
numeric_cols <- numeric_cols[!duplicate_rows, ]
stats2024_filtered <- stats2024_filtered[!duplicate_rows, ]

# Print number of entries after cleaning
print(nrow(numeric_cols))

# Scale the numeric columns
numeric_cols <- scale(numeric_cols)

# Compute distance matrix using Canberra distance
dist_matrix <- dist(numeric_cols, method = "canberra")

# DColour
position_colors <- c("QB" = "red", "RB" = "blue",
                     "WR" = "purple", "TE" = "green", "FB" = "yellow")

# 1) 2D isoMDS
mds_result_2d <- isoMDS(dist_matrix, k = 2)
cat("Number of iterations for 2D isoMDS:", mds_result_2d$iters, "\n")
cat("2D isoMDS stress:", mds_result_2d$stress, "\n")
cat("Number of datapoints in 2D isoMDS:", nrow(mds_result_2d$points), "\n")

mds_df_2d <- data.frame(
  Pos = stats2024_filtered$FantPos,
  X = mds_result_2d$points[, 1],
  Y = mds_result_2d$points[, 2]
)

# 2D Plot with ggplot2 using same style as the 3D plot
p2d <- ggplot(mds_df_2d, aes(x = X, y = Y, color = Pos)) +
  geom_point(size = 3, alpha = 0.8) +
  scale_color_manual(values = position_colors) +
  labs(
    title = "2D oMDS Plot of NFL Player Data (2024)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2",
    color = "Position"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

print(p2d)
ggsave("./figures/omds_2024_canberra_2d.png",
       plot = p2d, width = 8, height = 6, dpi = 300)

# 2) 2D with RANK

mds_result_2d <- isoMDS(dist_matrix, k = 2)
cat("Number of iterations for 2D isoMDS:", mds_result_2d$iters, "\n")
cat("2D isoMDS stress:", mds_result_2d$stress, "\n")
cat("Number of datapoints in 2D isoMDS:", nrow(mds_result_2d$points), "\n")

# Pos Rnak
mds_df_2d <- data.frame(
  Pos = stats2024_filtered$FantPos,
  PosRank = stats2024_filtered$Fantasy_PosRank,  # ensure this column exists
  X = mds_result_2d$points[, 1],
  Y = mds_result_2d$points[, 2]
)

# Colour
position_colors <- c("QB" = "red", "RB" = "blue",
                     "WR" = "purple", "TE" = "green", "FB" = "yellow")

p2d <- ggplot(mds_df_2d, aes(x = X, y = Y, color = Pos, size = PosRank)) +
  geom_point(alpha = 0.8) +
  scale_color_manual(values = position_colors) +
  scale_size_continuous(
    trans = "reverse",
    range = c(2, 20),
    breaks = c(1, 5, 10, 50, 100, 200, 300),
  ) +
  labs(
    title = "2D oMDS Plot of NFL Player Data (2024)",
    x = "MDS Dimension 1",
    y = "MDS Dimension 2",
    color = "Position",
    size = "PosRank"
  ) +
  theme_minimal(base_family = "sans") +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

print(p2d)
ggsave("./figures/omds_2d_rank_3d.png", plot = p2d, width = 8,
       height = 6, dpi = 300)

# Create a data frame combining the 2D MDS coordinates with each player's rank
mds_2d_with_rank <- data.frame(
  Pos  = stats2024_filtered$FantPos,     # Player position
  Rank = stats2024_filtered$Fantasy_PosRank,     # Player rank (numeric)
  X    = mds_result_2d$points[, 1],      # MDS Dimension 1
  Y    = mds_result_2d$points[, 2]       # MDS Dimension 2
)

# Define colors for each position
position_colors <- c("QB" = "red", "RB" = "blue",
                     "WR" = "purple", "TE" = "green", "FB" = "yellow")

# Map each row's position to the corresponding color
point_colors <- position_colors[mds_2d_with_rank$Pos]

# Use scatterplot3d to make a 3D plot
library(scatterplot3d)

png("./figures/omds_2d_rank_3d.png", width = 800, height = 600, res = 150)
scatterplot3d(
  x = mds_2d_with_rank$X, 
  y = mds_2d_with_rank$Y, 
  z = mds_2d_with_rank$Rank,
  color = point_colors, 
  pch = 16,
  xlab = "MDS Dim 1",
  ylab = "MDS Dim 2",
  zlab = "PosRank",
  main = "3D Plot of MDS Dimensions (X, Y) vs. Rank (Z)",
  cex.symbols = 0.8,
  cex.lab = 0.8,
  cex.axis = 0.7,
  cex.main = 0.9
)

# Add a legend
legend("topright", legend = names(position_colors),
       col = unname(position_colors), pch = 16, cex = 0.8)
dev.off()

# 3) 3D isoMDS
mds_result_3d <- isoMDS(dist_matrix, k = 3)
cat("3D isoMDS stress:", mds_result_3d$stress, "\n")
cat("Number of datapoints in 3D isoMDS:", nrow(mds_result_3d$points), "\n")

mds_df_3d <- data.frame(
  Pos = stats2024_filtered$FantPos,
  X = mds_result_3d$points[, 1],
  Y = mds_result_3d$points[, 2],
  Z = mds_result_3d$points[, 3]
)

# For the 3D plot, make sure to use the correct variable names
point_colors <- position_colors[mds_df_3d$Pos]

png("./figures/omds_3d_plot.png", width = 800, height = 600, res = 150)
scatterplot3d(
  mds_df_3d$X, mds_df_3d$Y, mds_df_3d$Z,
  color = point_colors, pch = 16,
  xlab = "MDS Dimension 1", ylab = "MDS Dimension 2", zlab = "MDS Dimension 3",
  main = "3D Ordinal MDS Plot of NFL Player Data (2024)",
  cex.symbols = 0.8,  # Point size
  cex.lab = 0.8,      # Axis label size
  cex.axis = 0.7,     # Tick label size
  cex.main = 0.9      # Title size
)
legend("topright", legend = names(position_colors),
       col = unname(position_colors), pch = 16, cex = 0.8)
dev.off()
