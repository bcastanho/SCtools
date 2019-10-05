library(tidyverse)
library(hexSticker)

set.seed(42)
x <- sample(rnorm(25, 1:4), 10)

years <- 1:20

output <- rnorm(year %*% t(x)) %>%
	matrix(ncol = 10) %>% 
	as_data_frame() %>% 
	cbind(years = years) %>% 
	gather(country, value, -years) %>%
	mutate(value = ifelse(country == "V1" & years > 10, value + 1.2, value)) %>% 
	mutate(color = ifelse(country == "V1", "black", "grey")) 

sctoolshex <- output %>% 
	ggplot(aes(years, value, group = country, color = color))+
	geom_line(size = .25)+
	geom_vline(xintercept = 10, lty = "dashed", size = .5)+
	geom_hline(yintercept = 0, size = .1, color = "black")+
	scale_color_manual(values = c("black", "grey"))+
	theme_minimal()+
	labs(
		x = "Year",
		y = NULL
	)+
	theme(legend.position = "none") +
	annotate(geom = "text",x = 3.8,y = 1.9, label = "SC", size = 10, hjust = 0)+
	annotate(geom = "text",x = 4.0,y = .9, label = expression(italic("tools")), 
					 size = 8, hjust = 0, parse = FALSE)+
	theme(axis.line.x = element_blank(),
				axis.ticks.x = element_blank(),
				axis.text = element_blank(),
				axis.title = element_blank(),
				panel.grid = element_blank())+
	scale_x_continuous(limits = c(-1,20.1))+
	scale_y_continuous(limits = c(-4,3))
sctoolshex

sticker(sctoolshex,
				package = "",
				filename = "man/figures/hexsticker.png",
				p_size = 8,
				s_x = .85,           # subplot position
				s_y = .85,           # subplot position
				s_width = 2,          # subplot width
				s_height = 2,         # subplot height
				p_color = "#00a2b2",
				h_fill = "#f5f5f5",
				h_size = 2,           # no border
				h_color = "#000000"
)
