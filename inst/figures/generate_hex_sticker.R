# create hex sticker

library(hexSticker)
library(here)
imgurl <- here("inst","figures","naloxone_kit_logo.png")
s <- sticker(imgurl, package="bennu", p_size=20, s_x=1, s_y=.75, s_width=.6,
             filename="inst/figures/hex_sticker.png")
