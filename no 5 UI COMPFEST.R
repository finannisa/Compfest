library(ggplot2)

car_data <- read.csv('used_car_data.csv')

plot.default(car_data['Kilometers_Driven'], type='o',
             main='USED CARs DISTANCE TRAVELED', xlab='Index Position',
             ylab='Kilometers Driven by a Car',
             col='#FF7C6A')

#K-means
KM_Driven <- car_data$Kilometers_Driven
KM_Driven[KM_Driven.max()] = 0
PRICE <- car_data$Price

set.seed(100)
km_drv <- kmeans(x=c(KM_Driven, PRICE), centers=3, nstart=25)
km_drv

#Visualization
clusters <- km_drv$cluster

p <- ggplot(data.frame(KM_Driven, PRICE), aes(KM_Driven, PRICE,
                                       size=PRICE))
p + geom_point()
p + geom_point(aes(colour = factor(KM_Driven)))

#Conclusion
high_km <- c(KM_Driven[min(which(clusters==1))],
             KM_Driven[max(which(clusters==1))])
low_km <- c(KM_Driven[min(which(clusters==3))],
            KM_Driven[max(which(clusters==3))])
outlier <- KM_Driven[which(clusters==2)]
indicator <- c('From', "To")

data.frame("." = indicator, "Low_Km" = low_km, "High_Km" = high_km, "Outlier" = outlier)
