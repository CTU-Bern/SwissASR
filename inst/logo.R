
library(emojifont)
load.fontawesome()
icon <- fontawesome("fa-wheelchair")

library(hexSticker)
library(ggplot2)

cross <- tibble::tribble(
  ~x, ~y,
  -.5, -1.6,
  .5, -1.6,
  .5, -.5,
  1.6, -.5,
  1.6, .5,
  .5, .5,
  .5, 1.6,
  -.5, 1.6,
  -.5, .5,
  -1.6, .5,
  -1.6, -.5,
  -.5, -.5,
  -.5, -1.6
)

icon <- ggplot() +
  geom_polygon(data = cross, aes(x = x, y = y), fill = "white") +
  geom_text(aes(x = 0, y = 0, label = fontawesome("fa-heartbeat")),
            family = "fontawesome-webfont",
            size = 20, col = CTUtemplate::unibeRed()) +
  # geom_text(aes(x = 0, y = 0, label = fontawesome("fa-clipboard")),
  #           family = "fontawesome-webfont",
  #           size = 20) +
  # geom_fontawesome(alias = "fa-clipboard",
  #           size = 20) +
  theme_void() +
  theme_transparent() +
  theme(aspect.ratio = 1)


s <- sticker(icon, package="",
             s_x=1, s_y=1, s_width=1.5, s_height=1.5,
             filename="man/figures/logo.png",
             h_fill = colorRampPalette(c("white", CTUtemplate::unibeRed()))(6)[3],
             h_color = CTUtemplate::unibeRed(),
             h_size = 2,
             url = "SwissASR",
             u_size = 12,
             u_x = 1,
             u_y = 0.15
)
s



