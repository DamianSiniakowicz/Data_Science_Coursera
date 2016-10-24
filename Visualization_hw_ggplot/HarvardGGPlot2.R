# Harvard ggplot2 assignment

library(ggplot2)
econ_data <- read.csv("Rgraphics/dataSets/EconomistData.csv")

# part 4 : challenge
ggplot(data = econ_data, aes(x=CPI, y=HDI,color=Region)) + 
  scale_y_continuous(name="Human Development Index, 2011 (1=best)", breaks = seq(0.1,1.0,len=10), labels = as.character(seq(0.1,1.0,len=10)), limits = c(0.2,1.0)) + geom_point() + 
  stat_smooth(se = FALSE,method = "loess",span = 10.0,color="red",fullrange=TRUE) + 
  scale_x_continuous(name= "Corruption Perceptions Index, 2011 (10=least corrupt)", breaks = 0:10, labels = as.character(0:10),limits = c(1,10),) + 
  theme_gray() + 
  theme(panel.grid.minor=element_blank(), panel.grid.major.x =element_blank(), legend.position = "top", legend.direction="horizontal") + 
  ggtitle("Corruption and human development") + 
  scale_color_manual(name = "",values=c("blue","green","red","yellow","black","orange"), labels = c("Americas","Asia Pacific","East Europe / Central Asia", "West Europe", "Middle East & North Africa", "Sub-Saharan Africa"))


# part 1
#cpi_hdi <- ggplot(data = econ_data, aes(x=CPI,y=HDI))
#cpi_hdi + geom_point()
#cpi_hdi + geom_point(color="blue")
#cpi_hdi + geom_point(aes(color=Region))
#ggplot(data=econ_data) + geom_boxplot(aes(x=Region,y=CPI))
#ggplot(data=econ_data) + geom_boxplot(aes(x=Region,y=CPI)) + geom_point(aes(x=Region,y=CPI))
# better way
#ggplot(data = econ_data, aes(x = Region, y = CPI)) + geom_boxplot() + geom_point()

# part 2

#ggplot(data = econ_data, aes(x = CPI, y = HDI)) + geom_point()
#ggplot(data = econ_data, aes(x = CPI, y = HDI)) + geom_point() + stat_smooth()
#lin_mod <- predict(lm(HDI~CPI,data=econ_data))
#ggplot(data = econ_data, aes(x = CPI, y = HDI)) + geom_point() + stat_smooth(aes(y=lin_mod))
#LOESS2 <- predict(loess(HDI~CPI, data = econ_data, span = 0.25))
#ggplot(data = econ_data, aes(x = CPI, y = HDI)) + geom_point() + stat_smooth(aes(y=LOESS2))
#LOESS3 <- predict(loess(HDI~CPI, data = econ_data, span = .995))
#ggplot(data = econ_data, aes(x = CPI, y = HDI)) + geom_point() + stat_smooth(aes(y=LOESS3))

# part 3

#sggplot(data = econ_data, aes(x=CPI, y = HDI, color= Region)) + geom_point() 
#+ scale_x_continuous(name="Corruption Perception Index") 
#+ scale_y_continuous(name="Human Development Index") 
#+ scale_color_manual(name="Geographic Region",values=c("blue","green","red","yellow","black","orange"))





