# required plotting function

plot_culmen_figure <- function(data){
  data %>% 
    ggplot(aes(x = body_mass_g, y = culmen_depth_mm)) +
    geom_point(alpha = 0.5, colour = "#366D94") +
    geom_smooth(method = "lm", level = 0.95, colour = "#366D94") +
    ylim(12, 17.5) +
    labs(x = "Body mass (g)",
         y = "Culmen depth (mm)",
         title = bquote("The relationship between body mass (g) \n and culmen depth (mm) in Gentoo penguins " *italic("Pygoscelis papua")),
         subtitle = "A scatterplot with regression line at 95% confidence",
         caption = "Points refer to the Gentoo species sampled. Equation of line provided on graph.") +
    stat_regline_equation(label.x=5500, label.y=13.2, colour = "#366D94") +
    stat_cor(aes(label=..rr.label..), label.x=5500, label.y=12.9, colour = "#366D94") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          plot.margin = margin(1.5,1,1,1, "cm"))
}

# function for saving as a PNG

save_culmen_plot_png <- function(penguins_culmen, 
                                 filename, size, res, scaling){
  agg_png(filename, width = 20, 
          height = 15,
          units = "cm", 
          res = res, 
          scaling = scaling)
  culmen_scatter <- plot_culmen_figure(penguins_culmen)
  print(culmen_scatter)
  dev.off()
}

# function for saving as an SVG

save_culmen_plot_svg <- function(penguins_flippers, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  culmen_scatterplot <- plot_culmen_figure(penguins_culmen)
  print(culmen_scatterplot)
  dev.off()
}

