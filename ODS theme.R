library(extrafont)
library(ggplot2)



ods.green <- "#4a7729"
ods.blue <- "#004c97"
ods.red <- "#AF1E2D"
ods.gray <- "#a9a9a9"
ods.dark.gray <- "#998C7C"
ods.light.gray <- "#eeeeee"
ods.white <- "#ffffff"

## Define sizes 

ods.text.size <- 20

# Import fonts

font_import(pattern="Roboto")

# Define themes

ods.base.theme <- theme(
  panel.background = element_rect(fill = "white"),
  plot.background = element_blank(),
  text = element_text(size=ods.text.size, family="Roboto"),
  title = element_text(size=ods.text.size, family="Roboto"),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  legend.position = "none"
)

ods.map.theme <- theme(
  axis.line = element_blank(),
  axis.line.y = element_blank(),
  axis.title.y = element_blank(),
  axis.title.x = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks = element_blank(),
  panel.grid = element_blank(),
  panel.border = element_blank(),
  plot.background = element_blank(),
  panel.background = element_blank(),
  legend.position = "bottom",
  legend.title = element_blank()
)

ods.bar.theme <- ods.base.theme + theme(
  # axis.text.x = element_blank()
  axis.text.y = element_text(margin = margin(t = 0, r = -15, b = 0, l = 0)),
  panel.grid.major.x = element_line(color = ods.light.gray)
)

ods.bar.theme.faceted <- ods.base.theme + theme(
  strip.background = element_blank(),
  strip.text = element_text(face = "bold", size = ods.text.size, hjust = .05),
  legend.position = "none",
  panel.grid.major.y = element_blank(),
  panel.grid.major.x = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank()
)


ods.column.theme <- ods.base.theme + theme(
  axis.text.x = element_text(size=ods.text.size, family="SourceSansPro-Regular"),
  axis.text.y = element_text(size=ods.text.size, family="SourceSansPro-Regular"),
  axis.title = element_text(size=ods.text.size, family="SourceSansPro-Regular"),
  # axis.title.y = element_text(size=ods.text.size, family="SourceSansPro-Regular"),
  panel.grid.major.y = element_line(color = ods.light.gray)
)
