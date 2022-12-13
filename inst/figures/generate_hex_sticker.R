# create hex sticker

library(hexSticker)
library(here)
imgurl <- here("inst","figures","naloxone_kit_logo.png")
s <- sticker(imgurl, package="BENNU", p_size=20, s_x=1, s_y=.75, s_width=.6,
             p_color="#595959", h_fill = "#fafafa",
             h_size=2,h_color= "#cc0000",spotlight=T,
             filename="inst/figures/hex_sticker.png")
plot(s)
