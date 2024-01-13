setwd("C:/Users/davin/Documents/PhD/Results/byonoy/151022/")
library(readxl)
library(growthcurver)
library(tidyverse)
library(reshape2)
library(openxlsx)
library(plater)
library(directlabels)
library(gtools)
library(gridExtra)
library(tictoc)
library(ggpubr)

#plater import
plated <- read_plate(
  file = "C:/Users/davin/Documents/PhD/Results/byonoy/151022/151022_layout.csv",
  well_ids_column = "Wells")

#take away OD column from the plater tibble and transpose
platerbind <- plated[,1:2] %>% t()
##x<-(t(platerbind))
##aggregate(x,)


####ggplotting####
#save formatting as a theme
mytheme<-theme(axis.text.x =element_text(size=7),
               axis.text.y =element_text(size=5),
               axis.title =element_blank(),
               axis.line = element_line(size=1.2),
               axis.ticks = element_line(size=1.2),
               plot.title = element_text(size=7),
               strip.text = element_text(size=5),
               strip.text.x = element_text(margin = margin(.05, 0, .05, 0, "cm"), size=8),
               legend.position = "none")

#####functionise plotting#####
ggplotify.bc<-function(p){ggplot(p, aes(Time,value, col=variable)) + 
    geom_point(size=1) + 
    geom_line() +
    xlab("Hours") + 
    ## ylab("Blank-corrected 0D600") +
    facet_wrap(~variable, ncol=3) +
    theme_bw() +
    mytheme +
    scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 8)) +
    scale_y_continuous(limits = c(-0.1,0.7), breaks = seq(-0.1, 0.7, by = 0.2))}



#####FUNCTIONS#####
#extract OD function
##extractify.OD <- function(f){
##  excel <-read_excel(f, col_names=FALSE, 
##                     range = "B2:M9", col_types = c("numeric", 
##                                                     "numeric", "numeric", "numeric", 
##                                                   "numeric", "numeric", "numeric", 
##                                                 "numeric", "numeric", "numeric", 
##                                               "numeric", "numeric"))
##  transv <- as.vector((t(excel)))
##  return(transv)}

extractify.OD <- function (f){
  csv <- read.csv(f)
  ods <- csv[c(1:8),c(2:13)]
  transv <- as.vector(t(ods))
  return(transv)
}



#####SORTFILES#####
#folder for files containing OD

sortfiles<-mixedsort(list.files(path ="C:/Users/davin/Documents/PhD/Results/byonoy/151022/", pattern = '151022_1_t'))
sortfiles

#tidy up function - REMEMBER TO CHANGE TIME POINTS
tidy.up<-function(newseries){
  Time<-c("0","13","14","15","16","17","18","19.25",
          "20","20.5","21","21.5","22","22.5","22.75",
          "24","34","35","36","37","37.5","38",
          "40")
  tns<-t(newseries)
  nice<-rbind(platerbind[2,], tns)
  samples<-as.data.frame(nice[c(1,3:nrow(nice)),], col.names=TRUE, stringsAsFactors=FALSE)
  colnames(samples)<-samples[1,]
  samples_clean<-samples[2:nrow(samples),]
  merged<-cbind(Time,samples_clean, stringsAsFactors=FALSE) 
  fornum <- merged[!is.na(names(merged))]
  num<- fornum %>% mutate_if(sapply(fornum, is.character), as.numeric)
  write.csv(num, "tidy_new.csv", row.names=FALSE) #######CHANGE FILE NAME
}


#check extractify
##length(sortfiles)
##extractify.OD(sortfiles[[22]])

####series####
#series dataframe for loop
series <- data.frame(well=c("A01","A02","A03","A04","A05","A06","A07","A08","A09","A10","A11","A12",
                            "B01","B02","B03","B04","B05","B06","B07","B08","B09","B10","B11","B12",
                            "C01","C02","C03","C04","C05","C06","C07","C08","C09","C10","C11","C12",
                            "D01","D02","D03","D04","D05","D06","D07","D08","D09","D10","D11","D12",
                            "E01","E02","E03","E04","E05","E06","E07","E08","E09","E10","E11","E12",
                            "F01","F02","F03","F04","F05","F06","F07","F08","F09","F10","F11","F12",
                            "G01","G02","G03","G04","G05","G06","G07","G08","G09","G10","G11","G12",
                            "H01","H02","H03","H04","H05","H06","H07","H08","H09","H10","H11","H12"))

#loop
sortfiles
for(f in sortfiles){series[[f]]<-cbind(extractify.OD(f))}
tidy.up(series)
tidied<-read.csv(file = "tidy_new.csv")



####Extraction, blank-correction and plotting#####---------------------------------

#####extraction#####

#extract sms
sm1<- select(tidied,1,ends_with("sm1"))
names(sm1)[names(sm1) == "blank_sm1"] <- "blank"

sm2<- select(tidied,1,ends_with("sm2"))
names(sm2)[names(sm2) == "blank_sm2"] <- "blank"

sm3<- select(tidied,1,ends_with("sm3"))
names(sm3)[names(sm3) == "blank_sm3"] <- "blank"

sm4<- select(tidied,1,ends_with("sm4"))
names(sm4)[names(sm4) == "blank_sm4"] <- "blank"

sm5<- select(tidied,1,ends_with("sm5"))
names(sm5)[names(sm5) == "blank_sm5"] <- "blank"

sm6<- select(tidied,1,ends_with("sm6"))
names(sm6)[names(sm6) == "blank_sm6"] <- "blank"

sm7<- select(tidied,1,ends_with("sm7"))
names(sm7)[names(sm7) == "blank_sm7"] <- "blank"

sm8<- select(tidied,1,ends_with("sm8"))
names(sm8)[names(sm8) == "blank_sm8"] <- "blank"

sm9<- select(tidied,1,ends_with("sm9"))
names(sm9)[names(sm9) == "blank_sm9"] <- "blank"

sm10<- select(tidied,1,ends_with("sm10"))
names(sm10)[names(sm10) == "blank_sm10"] <- "blank"

sm11<- select(tidied,1,ends_with("sm11"))
names(sm11)[names(sm11) == "blank_sm11"] <- "blank"

sm12<- select(tidied,1,ends_with("sm12"))
names(sm12)[names(sm12) == "blank_sm12"] <- "blank"

sm13<- select(tidied,1,ends_with("sm13"))
names(sm13)[names(sm13) == "blank_sm13"] <- "blank"

sm14<- select(tidied,1,ends_with("sm14"))
names(sm14)[names(sm14) == "blank_sm14"] <- "blank"

sm15<- select(tidied,1,ends_with("sm15"))
names(sm15)[names(sm15) == "blank_sm15"] <- "blank"

sm16<- select(tidied,1,ends_with("sm16"))
names(sm16)[names(sm16) == "blank_sm16"] <- "blank"





#####blank-correction#####
#note: [,3:ncol(df)-1] means 2nd column to 2nd to last column. [,3:(ncol(df)-1)] means 3rd column to 2nd to last column
sm1_bc<-sm1
sm1_bc[,2:(ncol(sm1_bc)-1)] = sm1_bc[,2:(ncol(sm1_bc)-1)] - sm1_bc[,ncol(sm1_bc)]
sm2_bc<-sm2
sm2_bc[,2:(ncol(sm2_bc)-1)] = sm2_bc[,2:(ncol(sm2_bc)-1)] - sm2_bc[,ncol(sm2_bc)]
sm3_bc<-sm3
sm3_bc[,2:(ncol(sm3_bc)-1)] = sm3_bc[,2:(ncol(sm3_bc)-1)] - sm3_bc[,ncol(sm3_bc)]
sm4_bc<-sm4
sm4_bc[,2:(ncol(sm4_bc)-1)] = sm4_bc[,2:(ncol(sm4_bc)-1)] - sm4_bc[,ncol(sm4_bc)]
sm5_bc<-sm5
sm5_bc[,2:(ncol(sm5_bc)-1)] = sm5_bc[,2:(ncol(sm5_bc)-1)] - sm5_bc[,ncol(sm5_bc)]
sm6_bc<-sm6
sm6_bc[,2:(ncol(sm6_bc)-1)] = sm6_bc[,2:(ncol(sm6_bc)-1)] - sm6_bc[,ncol(sm6_bc)]
sm7_bc<-sm7
sm7_bc[,2:(ncol(sm7_bc)-1)] = sm7_bc[,2:(ncol(sm7_bc)-1)] - sm7_bc[,ncol(sm7_bc)]
sm8_bc<-sm8
sm8_bc[,2:(ncol(sm8_bc)-1)] = sm8_bc[,2:(ncol(sm8_bc)-1)] - sm8_bc[,ncol(sm8_bc)]
sm9_bc<-sm9
sm9_bc[,2:(ncol(sm9_bc)-1)] = sm9_bc[,2:(ncol(sm9_bc)-1)] - sm9_bc[,ncol(sm9_bc)]
sm10_bc<-sm10
sm10_bc[,2:(ncol(sm10_bc)-1)] = sm10_bc[,2:(ncol(sm10_bc)-1)] - sm10_bc[,ncol(sm10_bc)]
sm11_bc<-sm11
sm11_bc[,2:(ncol(sm11_bc)-1)] = sm11_bc[,2:(ncol(sm11_bc)-1)] - sm11_bc[,ncol(sm11_bc)]
sm12_bc<-sm12
sm12_bc[,2:(ncol(sm12_bc)-1)] = sm12_bc[,2:(ncol(sm12_bc)-1)] - sm12_bc[,ncol(sm12_bc)]
sm13_bc<-sm13
sm13_bc[,2:(ncol(sm13_bc)-1)] = sm13_bc[,2:(ncol(sm13_bc)-1)] - sm13_bc[,ncol(sm13_bc)]
sm14_bc<-sm14
sm14_bc[,2:(ncol(sm14_bc)-1)] = sm14_bc[,2:(ncol(sm14_bc)-1)] - sm14_bc[,ncol(sm14_bc)]
sm15_bc<-sm15
sm15_bc[,2:(ncol(sm15_bc)-1)] = sm15_bc[,2:(ncol(sm15_bc)-1)] - sm15_bc[,ncol(sm15_bc)]
sm16_bc<-sm16
sm16_bc[,2:(ncol(sm16_bc)-1)] = sm16_bc[,2:(ncol(sm16_bc)-1)] - sm16_bc[,ncol(sm16_bc)]


#melting
sm1_melt <- melt(sm1_bc, id.vars = "Time")
sm2_melt <- melt(sm2_bc, id.vars = "Time")
sm3_melt <- melt(sm3_bc, id.vars = "Time")
sm4_melt <- melt(sm4_bc, id.vars = "Time")
sm5_melt <- melt(sm5_bc, id.vars = "Time")
sm6_melt <- melt(sm6_bc, id.vars = "Time")
sm7_melt <- melt(sm7_bc, id.vars = "Time")
sm8_melt <- melt(sm8_bc, id.vars = "Time")
sm9_melt <- melt(sm9_bc, id.vars = "Time")
sm10_melt <- melt(sm10_bc, id.vars = "Time")
sm11_melt <- melt(sm11_bc, id.vars = "Time")
sm12_melt <- melt(sm12_bc, id.vars = "Time")
sm13_melt <- melt(sm13_bc, id.vars = "Time")
sm14_melt <- melt(sm14_bc, id.vars = "Time")
sm15_melt <- melt(sm15_bc, id.vars = "Time")
sm16_melt <- melt(sm16_bc, id.vars = "Time")


#plotting
sm1_plot<-ggplotify.bc(sm1_melt)
sm2_plot<-ggplotify.bc(sm2_melt)
sm3_plot<-ggplotify.bc(sm3_melt)
sm4_plot<-ggplotify.bc(sm4_melt)
sm5_plot<-ggplotify.bc(sm5_melt)
sm6_plot<-ggplotify.bc(sm6_melt)
sm7_plot<-ggplotify.bc(sm7_melt)
sm8_plot<-ggplotify.bc(sm8_melt)
sm9_plot<-ggplotify.bc(sm9_melt)
sm10_plot<-ggplotify.bc(sm10_melt)
sm11_plot<-ggplotify.bc(sm11_melt)
sm12_plot<-ggplotify.bc(sm12_melt)
sm13_plot<-ggplotify.bc(sm13_melt)
sm14_plot<-ggplotify.bc(sm14_melt)
sm15_plot<-ggplotify.bc(sm15_melt)
sm16_plot<-ggplotify.bc(sm16_melt)


####ggarrange####


ggarrange(sm1_plot,sm2_plot,sm3_plot,
          sm7_plot,
          ncol = 2, nrow = 2)

ggarrange(sm1_plot,sm2_plot,sm3_plot,sm4_plot,sm5_plot,sm6_plot,
          sm7_plot,sm8_plot,
          ncol = 4, nrow = 2)



ggarrange(sm1_plot,sm2_plot,sm3_plot,sm4_plot,sm5_plot,sm6_plot,
          sm7_plot,sm8_plot,sm9_plot,sm10_plot,sm11_plot,sm12_plot,
          sm13_plot, sm14_plot, sm15_plot, sm16_plot,
          ncol = 4, nrow = 4)


View(tidied)







#make plot list for pdf generation
##plots<-list(sm1_plot,sm2_plot,sm3_plot,sm4_plot,sm5_plot,sm6_plot,sm7_plot,sm8_plot,sm9_plot,sm10_plot,sm11_plot,sm12_plot)


#individual plot generation in graphics window
##sm1_plot
##sm2_plot
##sm3_plot
##sm4_plot
##sm5_plot
##sm6_plot
##sm7_plot
##sm8_plot
##sm9_plot
##sm10_plot
##sm11_plot
##sm12_plot

##--------------------------------REAL-TIME SCRIPT END------------------------------##


#####GROWTHCURVER#### ================================================================================
##install.packages("growthcurver")
##library(growthcurver)

#Growth curves will be made for sm3-sm6 as sm1-sm3 was taken during exponential phase 
#so the curves are incomplete


#sm5-sm8
plate_sm5 <- SummarizeGrowthByPlate(sm5, bg_correct = "blank", plot_fit = TRUE,
                                    plot_file = "plate_sm5.pdf") 
write.xlsx(plate_sm5, 'plate_gc_sm5.xlsx')

plate_sm6 <- SummarizeGrowthByPlate(sm6, bg_correct = "blank", plot_fit = TRUE,
                                    plot_file = "plate_sm6.pdf") 
write.xlsx(plate_sm6, 'plate_gc_sm6.xlsx')

plate_sm7 <- SummarizeGrowthByPlate(sm7, bg_correct = "blank", plot_fit = TRUE,
                                    plot_file = "plate_sm7.pdf") 
write.xlsx(plate_sm7, 'plate_gc_sm7.xlsx')

plate_sm8 <- SummarizeGrowthByPlate(sm8, bg_correct = "blank", plot_fit = TRUE,
                                    plot_file = "plate_sm8.pdf") 
write.xlsx(plate_sm8, 'plate_gc_sm8.xlsx')



#####make models#####
models.sm5 <- lapply(sm5[2:ncol(sm5)], function(x) SummarizeGrowth(sm5$Time,x))
models.sm6 <- lapply(sm6[2:ncol(sm6)], function(x) SummarizeGrowth(sm6$Time,x))
models.sm7 <- lapply(sm7[2:ncol(sm7)], function(x) SummarizeGrowth(sm7$Time,x))
models.sm8 <- lapply(sm8[2:ncol(sm8)], function(x) SummarizeGrowth(sm8$Time,x))


#loop to get predictions
predicted.sm5 <-data.frame(time=sm5$Time)
for (i in names(sm5[2:ncol(sm5)])){
  predicted.sm5[[i]] <- predict(models.sm5[[i]]$model)}

predicted.sm6 <-data.frame(time=sm6$Time)
for (i in names(sm6[2:ncol(sm6)])){
  predicted.sm6[[i]] <- predict(models.sm6[[i]]$model)}

predicted.sm7 <-data.frame(time=sm7$Time)
for (i in names(sm7[2:ncol(sm7)])){
  predicted.sm7[[i]] <- predict(models.sm7[[i]]$model)}

predicted.sm8 <-data.frame(time=sm8$Time)
for (i in names(sm8[2:ncol(sm8)])){
  predicted.sm8[[i]] <- predict(models.sm8[[i]]$model)}




#melt predicted dataframe and add to observed table
mpred.sm5 <- melt(predicted.sm5, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_sm5 <- left_join(sm5_melt, mpred.sm5, by=c("Time"="time", "variable"="Sample"))
head(obspred_sm5)
mpred.sm6 <- melt(predicted.sm6, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_sm6 <- left_join(sm6_melt, mpred.sm6, by=c("Time"="time", "variable"="Sample"))
mpred.sm7 <- melt(predicted.sm7, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_sm7 <- left_join(sm7_melt, mpred.sm7, by=c("Time"="time", "variable"="Sample"))
mpred.sm8 <- melt(predicted.sm8, id.vars="time", variable.name="Sample", value.name="pred.od")
obspred_sm8 <- left_join(sm8_melt, mpred.sm8, by=c("Time"="time", "variable"="Sample"))




####ggplot growthcurver results####
ggplotify.curves <- function(g)
{ggplot(g, aes(x=Time, y=OD)) +
    geom_point(aes(y=value), alpha=0.5, color="blue") +
    geom_line(aes(y=pred.od), color="red") +
    facet_wrap(~variable, ncol=4) +
    theme_bw()}


#plotting - make sure plot is exported when done
#save all plots as their own ggplot
#https://stackoverflow.com/questions/12234248/printing-multiple-ggplots-into-a-single-pdf-multiple-plots-per-page

p5<- ggplotify.curves(obspred_sm5)
p6<- ggplotify.curves(obspred_sm6)
p7<- ggplotify.curves(obspred_sm7)
p8<- ggplotify.curves(obspred_sm8)

#put together all ggplots in a list
plots<- list(p5,p6,p7,p8)


#save all plots in one pdf with all plots on one page
ggsave(
  filename = "plotsall.pdf", 
  plot = marrangeGrob(plots, nrow=4, ncol=1), 
  width = 10, height = 10
)




####END####

#adding timepoints based on model
tt <- seq(0,50, length=101)
tt #50 hours, 0.5h intervals


####sm4####
dp125sm4.p.df<-data.frame(Time=tt, pred.od = predict(models.sm4$DP125_sm4$model, newdata = list(t=tt)))
dp126sm4.p.df<-data.frame(Time=tt, pred.od = predict(models.sm4$DP126_sm4$model, newdata = list(t=tt)))
bpsm4.p.df<-data.frame(Time=tt, pred.od = predict(models.sm4$BP_sm4$model, newdata = list(t=tt)))

predsm4<-data.frame(Time=tt, DP125_sm4=dp125sm4.p.df$pred.od, DP126_sm4=dp126sm4.p.df$pred.od, BP_sm4=bpsm4.p.df$pred.od)                   
meltsm4<-melt(predsm4, id.vars = "Time")
head(meltsm4)


#outerjoin - keeps all values even if there are missing ones
#https://www.programmingr.com/examples/r-dataframe/merge-data-frames/
totalsm4 <- merge(obspred_sm4,meltsm4,by=c("Time","variable"), all=TRUE)

#plot sm4
ggplot(totalsm4, aes(x=Time, y=OD)) +
  geom_point(aes(y=value.x), alpha=0.5, color="blue") +
  geom_line(aes(y=totalsm4$value.y), color="red") +
  facet_wrap(~variable, ncol=4) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 4)) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2)) +
  theme_bw()


####sm5####
dp125sm5.p.df<-data.frame(Time=tt, pred.od = predict(models.sm5$DP125_sm5$model, newdata = list(t=tt)))
dp126sm5.p.df<-data.frame(Time=tt, pred.od = predict(models.sm5$DP126_sm5$model, newdata = list(t=tt)))
bpsm5.p.df<-data.frame(Time=tt, pred.od = predict(models.sm5$BP_sm5$model, newdata = list(t=tt)))

predsm5<-data.frame(Time=tt, DP125_sm5=dp125sm5.p.df$pred.od, DP126_sm5=dp126sm5.p.df$pred.od, BP_sm5=bpsm5.p.df$pred.od)                   
meltsm5<-melt(predsm5, id.vars = "Time")
head(meltsm5)


#outerjoin - keeps all values even if there are missing ones
#https://www.programmingr.com/examples/r-dataframe/merge-data-frames/
totalsm5 <- merge(obspred_sm5,meltsm5,by=c("Time","variable"), all=TRUE)

#plot sm5
ggplot(totalsm5, aes(x=Time, y=OD)) +
  geom_point(aes(y=value.x), alpha=0.5, color="blue") +
  geom_line(aes(y=totalsm5$value.y), color="red") +
  facet_wrap(~variable, ncol=4) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 4)) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2)) +
  theme_bw()


####sm6####
dp125sm6.p.df<-data.frame(Time=tt, pred.od = predict(models.sm6$DP125_sm6$model, newdata = list(t=tt)))
dp126sm6.p.df<-data.frame(Time=tt, pred.od = predict(models.sm6$DP126_sm6$model, newdata = list(t=tt)))
bpsm6.p.df<-data.frame(Time=tt, pred.od = predict(models.sm6$BP_sm6$model, newdata = list(t=tt)))

predsm6<-data.frame(Time=tt, DP125_sm6=dp125sm6.p.df$pred.od, DP126_sm6=dp126sm6.p.df$pred.od, BP_sm6=bpsm6.p.df$pred.od)                   
meltsm6<-melt(predsm6, id.vars = "Time")
head(meltsm6)


#outerjoin - keeps all values even if there are missing ones
#https://www.programmingr.com/examples/r-dataframe/merge-data-frames/
totalsm6 <- merge(obspred_sm6,meltsm6,by=c("Time","variable"), all=TRUE)

#plot sm6
ggplot(totalsm6, aes(x=Time, y=OD)) +
  geom_point(aes(y=value.x), alpha=0.5, color="blue") +
  geom_line(aes(y=totalsm6$value.y), color="red") +
  facet_wrap(~variable, ncol=4) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 4)) +
  scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, by = 0.2)) +
  theme_bw()


####mean plot####
head(totalsm5)
nrow(totalsm4)
nrow(totalsm5)
nrow(totalsm6) #all have same number of rows

#cbind all
totalrep<-cbind(totalsm4,totalsm5[,5],totalsm6[,5])
totalmean<- totalrep %>% mutate(mean=rowMeans(totalrep[,5:7]))
head(totalmean) #good

#plot using sm6 just to see what it looks like
ggplot(totalsm6, aes(x=Time, y=OD)) +
  geom_point(aes(y=value.x), alpha=0.5, color="blue") +
  geom_line(aes(y=totalmean$mean), color="red") +
  facet_wrap(~variable, ncol=4) +
  scale_x_continuous(limits = c(0, 32), breaks = seq(0, 32, by = 4)) +
  scale_y_continuous(limits = c(0, 0.7), breaks = seq(0, 0.7, by = 0.1)) +
  theme_bw()



