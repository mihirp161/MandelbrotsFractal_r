library(mandelbrot)
library(ggplot2)

# fractal parameters
mb <- mandelbrot::mandelbrot(
                 xlim = c(-0.8395, -0.83245),
                 ylim = c(0.204, 0.207), 
                 resolution = 1200L,
                 iterations = 5000
                 )

# vaccination heatmap palette
cols <- c(
  grDevices::colorRampPalette(c("#e7f0fa", "#c9e2f6", "#95cbee",
                     "#0099dc", "#4ab04a", "#ffd73e"))(20),
  grDevices::colorRampPalette(c("#eec73a", "#e29421", "#e29421",
                     "#f05336","#ce472e"), bias=2)(100),
  "gray0")
# coerce into dataframe
df <- as.data.frame(mb)

# create and save the plot
ggplot2::ggplot(df, aes(x = x, y = y, fill = value)) +
  ggplot2::geom_raster(interpolate = TRUE) + 
  ggplot2::theme_void() +
  ggplot2::scale_fill_gradientn(colours = cols, guide = "none")

ggplot2::ggsave("404_fractal.png")
dev.off()