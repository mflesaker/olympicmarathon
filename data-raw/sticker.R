library(hexSticker)
library(magick)

imgurl <- here::here("man/figures/figure1.png")


olympicmarathon_sticker <- sticker(imgurl,
        package = "olympicmarathon", h_color = "#001C56", p_color = "#FFFFFF", h_fill = "#432860",
        p_x = 1, p_y = 1.4,
        p_size = 5, s_x = 0.95, s_y = .8, s_width = 0.7, s_height = 0.4, h_size = 2, u_size = 4,
        filename = here::here("data-raw", "hex_olympicmarathon.png"),
        white_around_sticker = TRUE) +
  geom_url(url = "https://github.com/mflesaker/olympicmarathon",
           size = 1.0)

save_sticker(here::here("data-raw", "hex_olympicmarathon.png"), olympicmarathon_sticker)
