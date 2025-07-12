
rm(list=ls())
dir = Sys.info()[7]
setwd(paste0("C:/Users/", dir, "/Documents/GitHub/Restoration/Stabilization/"))

library(dplyr)
library(ggplot2)
library(ggridges)
library(tidyr)
library(grid)


#LOAD DATA
colony<-read.csv("mockdata.csv")

colony$Time <- factor(colony$Time, levels = c("B", "A"))

colony$ShapeGroup <- with(colony, paste(Treatment, IvC, sep = "_"))

shape_values <- c(
  "Boulder_I" = 17,  # filled triangle
  "Boulder_C" = 2,   # open triangle
  "Mesh_I" = 15,     # filled square
  "Mesh_C" = 0       # open square
)

color_values <- c(
  "Boulder_I" = "#008080",  # teal
  "Boulder_C" = "#008080",  # teal
  "Mesh_I" = "#800080",     # purple
  "Mesh_C" = "#800080"      # purple
)

p <- ggplot(colony, aes(x = Time, y = Density, color = ShapeGroup, shape = ShapeGroup)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = Density - SE, ymax = Density + SE), width = 0.1) +
  facet_wrap(~Treatment) +
  scale_shape_manual(values = shape_values) +
  scale_color_manual(values = color_values) +
  labs(x = "Time", y = "Mean Wild Colony Density") +
  theme_bw() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 16),
    legend.title = element_blank(),
    legend.position = "none"
  )

# Use a graphics device to capture both ggplot + grid overlay and add vertical line between facets
jpeg("Mock_BACI.jpg", width = 8, height = 5, units = "in", res = 300)

# Draw the ggplot
print(p)

# Add the vertical dashed line between facets
grid.lines(x = unit(0.525, "npc"), y = unit(c(0.05, 0.95), "npc"),
           gp = gpar(lty = "dashed", col = "gray30", lwd = 1))

# Close the graphics device
dev.off()


#Calculate treatment effects
# Create the data
df <- tribble(
  ~Treatment, ~IvC, ~Time, ~Density, ~SE,
  "Boulder", "I", "B", 3.5, 0.6,
  "Boulder", "C", "B", 2.2, 0.2,
  "Boulder", "I", "A", 15.0, 0.9,
  "Boulder", "C", "A", 2.1, 0.3,
  "Mesh", "I", "B", 1.9, 0.8,
  "Mesh", "C", "B", 2.8, 0.3,
  "Mesh", "I", "A", 4.1, 0.9,
  "Mesh", "C", "A", 2.1, 0.3
)

# Compute BACIPS treatment effect for each Treatment
effects <- df %>%
  select(-SE)%>%
  pivot_wider(names_from = c(IvC, Time), values_from = Density) %>%
  mutate(
    treatment_effect = (`I_A` - `C_A`) - (`I_B` - `C_B`)
  ) %>%
  select(Treatment, treatment_effect)

# Print result
effects

#Note- it's generally not best practice to make direct quantitative ratio comparisons (e.g., "5 times more effective") 
#between treatment effects in BACIPS-style analyses—unless those effects are:Statistically independent #Have similar scales and variances
#Include a formal uncertainty analysis (e.g., confidence intervals)


#Create dataframe for BACI paired series figure
# Create the data frame
data <- data.frame(
  Months = rep(c(0, 6, 12, 18, 24), each = 3),
  Treatment = rep(c("Control", "Mesh", "Boulder"), times = 5),
  Density = c(
    1.8, 2.0, 1.6,   # Month 0
    2.1, 1.5, 3.2,   # Month 6
    2.2, 1.9, 4.0,   # Month 12
    2.1, 2.3, 4.8,   # Month 18
    2.2, 3.2, 8.0    # Month 24
  ),
  SE = c(
    0.15, 0.15, 0.15,   # Month 0
    0.2,  0.2,  0.2,    # Month 6
    0.2,  0.2,  0.25,   # Month 12
    0.25, 0.25, 0.3,    # Month 18
    0.2,  0.25, 0.3     # Month 24
  )
)



# Set factor levels to control legend order
data$Treatment <- factor(data$Treatment, levels = c("Control", "Mesh", "Boulder"))

# Plot
p2<-ggplot(data, aes(x = Months, y = Density, group = Treatment)) +
  geom_smooth(
    data = subset(data, Months >= 6 & Treatment != "Control"),
    aes(color = Treatment),
    method = "lm", se = FALSE, linetype = "solid", size = 1
  )+
  geom_point(aes(shape = Treatment, fill = Treatment), size = 3, color = "black") +
  geom_errorbar(aes(ymin = pmax(Density - SE, 0), ymax = Density + SE), width = 0.5) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "gray40", size = 1) +
  scale_shape_manual(values = c(21, 22, 24)) +  # Shapes that accept fill
  scale_color_manual(values = c("#800080", "#008080")) +
  scale_fill_manual(values = c("white", "#800080", "#008080")) +
  labs(x = "Months", y = "Wild Colony Density") +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.text = element_text(size = 14),
    axis.title = element_text(size = 16, face = "bold"),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", size = 16),
    legend.title = element_blank(),
    legend.position = c(0.05, 0.95),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 14),
    legend.key.size = unit(1.2, "lines")
  )

# Use a graphics device to capture both ggplot + grid overlay and add vertical line between facets
jpeg("Mock_BACI_pairedseries.jpg", width = 8, height = 6, units = "in", res = 300)

# Draw the ggplot
print(p2)

# Close the graphics device
dev.off()

