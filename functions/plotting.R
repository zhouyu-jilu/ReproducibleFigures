#for boxplots and other plots

species_colours <- c("Adelie" = "darkorange", 
                     "Chinstrap" = "purple", 
                     "Gentoo" = "cyan4")


plot_boxplot <- function(data, 
                         x_column, 
                         y_column, 
                         x_label, 
                         y_label, 
                         colour_mapping) {
  
  # First remove NA values
  data <- data %>%
    drop_na({{ y_column }})
  
  # Now make the plot
  ggplot(data = data, 
         aes(
           x = {{ x_column }}, 
           y = {{ y_column }}, 
           color = {{ x_column }})) +  # Use {{ }} for x and y columns
    geom_boxplot(
      width = 0.3, 
      show.legend = FALSE) +
    geom_beeswarm(aes(color = species), 
                  show.legend = FALSE) +
    scale_color_manual(
      values = colour_mapping) +  # Use color_mapping input here
    labs(
      x = x_label, 
      y = y_label) +  # Use provided x and y labels
    theme_bw()
}

plot_boxplot(penguins_clean, 
             species, flipper_length_mm, 
             "Species", "Flipper Length (mm)", 
             species_colours)

#bad plot code
sex_numeric <- as.numeric(factor(penguins_clean$sex)) - 1

badplot <- scatterplot3d(x = penguins_clean$flipper_length_mm,
                         y = penguins_clean$body_mass_g,
                         z = sex_numeric,
                         color = rainbow(nrow(penguins_clean)),
                         main = "Bad Penguin Plot",
                         xlab = "flipper Length ",
                         ylab = "bodymass ",
                         zlab = "Sex ",
                         angle = 70,
                         scale.y = 0.7,
                         box = FALSE,
                         axis.scales = list(x = list(at = seq(170, 230, by = 13)),
                                            y = list(at = seq(2500, 6500, by = 873)),
                                            z = list(at = c(0, 1), labels = c("Female", "Male"))))