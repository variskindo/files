#-------------------------------------------------------------------------
#LIBRERIAS Y DATA-FRAME mpg
#-------------------------------------------------------------------------
library(tidyverse)
mpg
View(mpg)
?mpg

#--------------------------------------------------------------------------
#ggplot
#--------------------------------------------------------------------------
ggplot(data = mpg) + geom_point(mapping = aes(x = displ, y = hwy))
#
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))
#
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, size = class))
#
#Left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))
#Right
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy, shape = class))
#
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy), color = "blue")
#
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_wrap(~ class, nrow = 3)
#
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) + 
  facet_grid(drv ~ cyl)
#
# left
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy))
# right
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy))
#
ggplot(data = mpg) + 
  geom_smooth(mapping = aes(x = displ, y = hwy, linetype = drv))

##
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
#     
ggplot(data = mpg) +
  geom_smooth(mapping = aes(x = displ, y = hwy, group = drv))
#   
ggplot(data = mpg) +
  geom_smooth(
    mapping = aes(x = displ, y = hwy, color = drv),
    show.legend = FALSE
  )
#
ggplot(data = mpg) + 
  geom_point(mapping = aes(x = displ, y = hwy)) +
  geom_smooth(mapping = aes(x = displ, y = hwy))
#
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point() + 
  geom_smooth()
#
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth()
#
#
ggplot(data = mpg, mapping = aes(x = displ, y = hwy)) + 
  geom_point(mapping = aes(color = class)) + 
  geom_smooth(data = filter(mpg, class == "subcompact"), se = FALSE)
#
ggplot(mpg, aes(displ, hwy)) +
geom_point() +
geom_smooth(span = 0.2)
#
ggplot(mpg, aes(displ, hwy)) +
geom_point() +
geom_smooth(span = 1)
#
library(mgcv)
ggplot(mpg, aes(displ, hwy)) +
geom_point() +
geom_smooth(method = "gam", formula = y ~ s(x))
#
library(MASS)
ggplot(mpg, aes(displ, hwy)) +
geom_point() +
geom_smooth(method = "lm")
#
ggplot(mpg, aes(drv, hwy)) +
geom_point()
#
ggplot(mpg, aes(drv, hwy)) + geom_jitter()
ggplot(mpg, aes(drv, hwy)) + geom_boxplot()
ggplot(mpg, aes(drv, hwy)) + geom_violin()
#
ggplot(mpg, aes(hwy)) + geom_histogram()
ggplot(mpg, aes(hwy)) + geom_freqpoly()
ggplot(mpg, aes(hwy)) +
geom_freqpoly(binwidth = 2.5)
ggplot(mpg, aes(hwy)) +
geom_freqpoly(binwidth = 1)
#
ggplot(mpg, aes(displ, colour = drv)) +
geom_freqpoly(binwidth = 0.5)
ggplot(mpg, aes(displ, fill = drv)) +
geom_histogram(binwidth = 0.5) +
facet_wrap(~drv, ncol = 1)
#
ggplot(mpg, aes(manufacturer)) +
geom_bar()
#
ggplot(economics, aes(date, unemploy / pop)) +
geom_line()
ggplot(economics, aes(date, uempmed)) +
geom_line()
#
ggplot(economics, aes(unemploy / pop, uempmed)) +
geom_path() +
geom_point()
year <- function(x) as.POSIXlt(x)$year + 1900
ggplot(economics, aes(unemploy / pop, uempmed)) +
geom_path(colour = "grey50") +
geom_point(aes(colour = year(date)))
#
ggplot(mpg, aes(cty, hwy)) +
geom_point(alpha = 1 / 3)
#
ggplot(mpg, aes(cty, hwy)) +
geom_point(alpha = 1 / 3) +
xlab("city driving (mpg)") +
ylab("highway driving (mpg)")
#
ggplot(mpg, aes(cty, hwy)) +
geom_point(alpha = 1 / 3) +
xlab(NULL) +
ylab(NULL)
#
ggplot(mpg, aes(drv, hwy)) +
geom_jitter(width = 0.25)
ggplot(mpg, aes(drv, hwy)) +
geom_jitter(width = 0.25) +
xlim("f", "r") +
ylim(20, 30)
ggplot(mpg, aes(drv, hwy)) +
geom_jitter(width = 0.25, na.rm = TRUE) +
ylim(NA, 30)
#
p <- ggplot(mpg, aes(displ, hwy, colour = factor(cyl))) +
geom_point()
print(p)
summary(p)
ggsave("plot.png", width = 5, height = 5)
#
#
qplot(displ, hwy, data = mpg)
qplot(displ, data = mpg)
qplot(displ, hwy, data = mpg, colour = "blue")
qplot(displ, hwy, data = mpg, colour = I("blue"))
#








plot(displ, hwy, data = mpg)
qplot(displ, data = mpg)
qplot(displ, hwy, data = mpg, colour = "blue")
qplot(displ, hwy, data = mpg, colour = I("blue"))
#








