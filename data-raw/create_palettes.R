## code to prepare `disco_palettes` dataset goes here


# CURRENT PALETTES ARE FOR TESTING ONLY, NOT ACTUAL PALETTES
disco_palettes <- list(
  "default" = c(
    GreenYellow = "#ADFF2F", Maroon = "#800000",
    MediumPurple = "#9370DB", SandyBrown = "#F4A460",
    MediumVioletRed = "#C71585", Gold = "#FFD700",
    LightSkyBlue = "#87CEFA", Burlywood = "#DEB887",
    SpringGreen = "#00FF7F", SlateGray = "#708090",
    Yellow = "#FFFF00", LawnGreen = "#7CFC00"
  ),
  "placeholder" = c(
    Teal = "#008080", Maroon = "#800000",
    DarkSalmon = "#E9967A", DarkGray = "#A9A9A9",
    SteelBlue = "#4682B4", OrangeRed = "#FF4500",
    MediumSeaGreen = "#3CB371", PowderBlue = "#B0E0E6"
  )
)

usethis::use_data(disco_palettes, overwrite = TRUE)
