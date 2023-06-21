#Load data
library(readr)
library(readxl)
airpresforr = read_excel("~/Desktop/Guam tides/AIRPRESSURE_MASTER.xlsx") 

#Replace 9999 with missing values
airpresforr$PRES[airpresforr$PRES==9999.0] <- NA #turns all 9999s into NAs
airpresforr = na.omit(airpresforr)
write.csv(airpresforr, file = "airpresforr_new1.csv")


airpresforr = read_csv("~/Desktop/airpresforr_new1.csv") #reload data after
#manually fixing DIFF column in excel
airpresforr$X1 =NULL

#The new dataframe will have 114937+784 = 115721 values
newairpres = data.frame(matrix(ncol = 8, nrow = 118000))
names(newairpres)[1] = "#YY"
names(newairpres)[2] = "MM"
names(newairpres)[3] = "DD"
names(newairpres)[4] = "hh"
names(newairpres)[5] = "mm"
names(newairpres)[6] = "DIFF"
names(newairpres)[7] = "PRES"
ctnew = 1


for(ctold in 1:114937){
  if (1000*floor(ctold/1000)==ctold){
    print(ctold[1])
  } # only debugs
  
  newairpres$`#YY`[ctnew]<-airpresforr$`#YY`[ctold]
  newairpres$MM[ctnew]<-airpresforr$MM[ctold]
  newairpres$DD[ctnew]<-airpresforr$DD[ctold]
  newairpres$hh[ctnew]<-airpresforr$hh[ctold]
  newairpres$mm[ctnew]<-airpresforr$mm[ctold]
  newairpres$DIFF[ctnew]<-airpresforr$DIFF[ctold]
  newairpres$PRES[ctnew]<-airpresforr$PRES[ctold]
  ctnew = ctnew + 1
  #print(c(ctnew[1], ctold[1]))
  
  if (airpresforr$DIFF[ctold]!=6 & airpresforr$DIFF[ctold]!=-54) {
    #print(c(ctold[1], airpresforr$DIFF[ctold]))
    nnew = (airpresforr$DIFF[ctold]/6) -1 # adds rows only when DIFF is positive!!
    if (airpresforr$DIFF < 1) { # accounts for negative DIFF's and 0's
      nnew = ((airpresforr$DIFF[ctold]+60)/6)-1
    }
    #print(nnew)
    if(nnew>0){
      presdelta = (airpresforr$PRES[ctold+1] - airpresforr$PRES[ctold])/(nnew+1)
      for (i in 1:nnew){
        #print(c(ctold[1], nnew[1]) )
        newmm = newairpres$mm[ctnew-1] +6 
        newhh = newairpres$hh[ctnew-1]
        newDD = newairpres$DD[ctnew-1]
        newMM = newairpres$MM[ctnew-1]
        newYY = newairpres$`#YY`[ctnew-1]
        if (newmm==60){
          newmm = 0
          newhh = newhh+1
        }
        if (newhh==24){
          newhh = 0
          newDD = newDD+1
        }
        lastdaymonth = F 
        
        if (lastdaymonth == T) {
          newDD == 1
          newMM = newMM+1
        }
        
        if (newMM == 2) {
          if (newDD > 29) {
            lastdaymonth == T
          }
          else {
            if (newDD > 28) {
              lastdaymonth == T
            }
          }
        } 
        else {
          if (newMM == 4 | newMM == 6 | newMM == 9 | newMM == 11) {
            if (newDD > 30) {
              lastdaymonth == T
            }
          } else {
            if (newDD > 31) {
              lastdaymonth == T
            }
          }
        }
        
        if (newMM == 13){ #check if newMM is now 13, if so set to 1 and increment newYY
          newMM = 1
          newYY = newYY+1
        }
        newairpres$`#YY`[ctnew]<-newYY
        newairpres$MM[ctnew]=newMM
        newairpres$DD[ctnew]=newDD
        newairpres$hh[ctnew]=newhh
        newairpres$mm[ctnew]=newmm
        newairpres$PRES[ctnew]=newairpres$PRES[ctnew-1]+presdelta
        ctnew = ctnew+1
      }
    }
  }
}

newairpres$X8 = NULL
                     

write.csv(newairpres, file = "newairpres_maybe.csv") #creates new csv

