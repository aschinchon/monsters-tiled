# Load in packages
library(tidyverse)
library(imager)

# Point to the place where your image is stored
file <- "frankenstein.jpg"

# Load and convert to grayscale
load.image(file) %>%
  grayscale() -> img

# The image is summarized into s x s squares 
s <- 14

# Resume pixels using mean: this decreases drastically the resolution of the image
img %>% 
  as.data.frame() %>%
  mutate(x = cut(x, round(dim(img)[1]/s, 0), labels = FALSE),
         y = cut(y, round(dim(img)[2]/s, 0), labels = FALSE)) %>%
  group_by(x, y) %>%
  summarise(value = mean(value)) -> df

# Create new variable to be used to define size and color of the lines of tiles
df %>% mutate(z = cut(value, breaks = 20, labels = FALSE)) -> df

# Initialize plot 
plot <- ggplot()

# Resulting plot will be build with 20 layers: one layer per each different value of z 
for (i in 1:20){
  sub_data = df %>% filter(z==i)
  plot <- plot + geom_tile(aes(x, y),
                           size = 2*i/(20-1)-2/(20-1),
                           fill = "darkred",
                           col = paste0("gray", round(((100-5)*i)/(20-1)+5-(100-5)/(20-1), 0)),
                           data = df %>% filter(z==i))
}

# Last tweaks
plot +
  coord_fixed() +
  scale_y_reverse() +
  theme_void() -> plot

plot

ggsave("frankenstein_tiled.png", plot, height =  8 , width =  6)

