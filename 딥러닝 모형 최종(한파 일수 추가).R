install.packages("nnet")
library(nnet)
install.packages("readxl")
library(readxl)
install.packages("neuralnet")
library(neuralnet)
install.packages("devtools")
install.packages("cli")
devtools::install_github("bips-hb/neuralnet")
ypedp <- read.csv("D:/폴더/대외할동/21-1/한우수급모형경진대회/밀화부리/데이터/40개월.csv")

a<-ypedp$Death*100
ypedp$Death<-a


f<-Death ~ temp + moist + rain + wind + hot + Dust + cold + ind

fit<-neuralnet(f, 
               data = ypedp[1:63, 2:10],
               hidden = c(5,10,20),
               algorithm = "rprop+",
               stepmax = 1e+06,
               rep = 500,
               err.fct ="sse",
               act.fct = "logistic",
               threshold = 0.01,
               linear.output = TRUE)
plot(fit)

yield.out_40 = compute(fit, ypedp[64:75, 3:10])

yield.out_40$net.result




#RMSE 시작
yield_40<- yield.out_40$net.result*sd(ypedp$Death)+mean(ypedp$Death)
A <- matrix(c(yield_40[1:12], ypedp$Death[64:75]), nrow=12, ncol=2, byrow=F)
print(A)
#RMSE#
RMSE_40 <- sqrt(sum((A[,1] - A[,2])^2)/(nrow(A)))     
print(RMSE_40)

document<-cbind(RMSE_5	,RMSE_6	,RMSE_7	,RMSE_8	,RMSE_9	,RMSE_10	,RMSE_11	,RMSE_12	,RMSE_13	,RMSE_14	,RMSE_15	,RMSE_16	,RMSE_17	,RMSE_18	,RMSE_19	,RMSE_20	,RMSE_21	,RMSE_22	,RMSE_23	,RMSE_24	,RMSE_25	,RMSE_26	,RMSE_27	,RMSE_28	,RMSE_29	,RMSE_30	,RMSE_31	,RMSE_32	,RMSE_33	,RMSE_34	,RMSE_35	,RMSE_36	,RMSE_37	,RMSE_38	,RMSE_39	,RMSE_40)
babo<-cbind(yield.out_5$net.result	,yield.out_6$net.result	,yield.out_7$net.result	,yield.out_8$net.result	,yield.out_9$net.result	,yield.out_10$net.result	,yield.out_11$net.result	,yield.out_12$net.result	,yield.out_13$net.result	,yield.out_14$net.result	,yield.out_15$net.result	,yield.out_16$net.result	,yield.out_17$net.result	,yield.out_18$net.result	,yield.out_19$net.result	,yield.out_20$net.result	,yield.out_21$net.result	,yield.out_22$net.result	,yield.out_23$net.result	,yield.out_24$net.result	,yield.out_25$net.result	,yield.out_26$net.result	,yield.out_27$net.result	,yield.out_28$net.result	,yield.out_29$net.result	,yield.out_30$net.result	,yield.out_31$net.result	,yield.out_32$net.result	,yield.out_33$net.result	,yield.out_34$net.result	,yield.out_35$net.result	,yield.out_36$net.result	,yield.out_37$net.result	,yield.out_38$net.result	,yield.out_39$net.result	,yield.out_40$net.result)
dd<-rbind(babo,document)
dd
write.csv(dd,"D:/폴더/대외할동/21-1/한우수급모형경진대회/밀화부리/데이터/result4.csv")

