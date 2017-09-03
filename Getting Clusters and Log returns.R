    #install.packages("EMCluster")
    getwd()
    setwd("F:/R_Projects/")
    congo_data=read.csv(file= "my_service_data.csv",head=TRUE,sep=",")
    ticker1.returns = diff(log(1/(congo_data$Sirius.XM.Holdings.Inc...SIRI.)))
    ticker2.returns = diff(log(1/(congo_data$Comcast.Corporation..CMCSA.)))
    ticker3.returns = diff(log(1/(congo_data$McDonald.s.Corporation..MCD.)))
    ticker4.returns = diff(log(1/(congo_data$FedEx.Corporation..FDX.)))
    ticker5.returns = diff(log(1/(congo_data$Wal.Mart.Stores..Inc...WMT.)))
    congo_data=congo_data[-c(2299),]
    congo_data["log_Siri"]=ticker1.returns
    congo_data["log_CMCSA"]=ticker2.returns
    congo_data["log_MCD"]=ticker3.returns
    congo_data["log_FDX"]=ticker4.returns
    congo_data["log_WMT"]=ticker5.returns
    ticker6.returns = diff(log(1/(congo_data$S.P.500...GSPC.)))
    ticker7.returns = diff(log(1/(congo_data$CBOE.Interest.Rate.10.Year.T.No...TNX.)))
    ticker8.returns = diff(log(1/(congo_data$Nikkei.225...N225.)))
    ticker9.returns = diff(log(1/(congo_data$Dow.Jones.Industrial.Average...DJI.)))
    ticker10.returns = diff(log(1/(congo_data$ESTX50.EUR.P...STOXX50E.)))
    ticker11.returns=diff(log(1/(congo_data$PHLX.Gold.Silver.Sector...XAU.)))
    ticker12.returns=diff(log(1/(congo_data$X1.month.LIBOR)))
    ticker13.returns=diff(log(1/(congo_data$X10yr.treasury)))
    ticker14.returns=diff(log(1/(congo_data$UNRATE)))
    ticker15.returns=diff(log(1/(congo_data$UMCSENT)))
    ticker16.returns=diff(log(1/(congo_data$PPIACO)))
    ticker17.returns=diff(log(1/(congo_data$DCOILWTICO)))
    ticker18.returns=diff(log(1/(congo_data$gold.price)))
    congo_data=congo_data[-c(2299),]
    congo_data["log_S&P"]=ticker6.returns
    congo_data["log_Int"]=ticker7.returns
    congo_data["log_Nikkei"]=ticker8.returns
    congo_data["log_Dow"]=ticker9.returns
    congo_data["log_GOld"]=ticker11.returns
    congo_data["log_LIBOR"]=ticker12.returns
    congo_data["log_treasury"]=ticker13.returns
    congo_data["log_UNRATE"]=ticker14.returns
    congo_data["log_UMSCENT"]=ticker15.returns
    congo_data["log_PPIACO"]=ticker16.returns
    congo_data["log_DCOILWTICO"]=ticker17.returns
    congo_data["log_GOld"]=ticker18.returns
    congo_data=congo_data[,-c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19)]
    #write.csv(file = "F:/Python_proj/net_congo_data.csv", x=fin_data)
    #K_means:
    
    sapply(congo_data, class)
    mydata1 <- scale(congo_data[,c(3,4,5,6,7)])
    
    apply(mydata1,2,sd) # sd=1 after scaling
    apply(mydata1,2,mean) # mean=0 after scaling
    
    # Determine number of clusters
    wss <- (nrow(mydata1)-1)*sum(apply(mydata1,2,var))
    for (i in 2:50) wss[i] <- sum(kmeans(mydata1, 
                                         centers=i)$withinss)
    plot(1:50, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")
    # K-Means Cluster Analysis
    fit <- kmeans(mydata1, 4) # 3 cluster solution
    # get cluster means
    aggregate(mydata1,by=list(fit$cluster),FUN=mean)
    # append cluster assignment
    sample_stocks <- data.frame(mydata1, fit$cluster)
    #sample_stocks
    sample_stocks$fit.cluster <- as.factor(sample_stocks$fit.cluster)
    #install.packages("ggplot2")
    library(ggplot2)
    ggplot(sample_stocks, aes(x=congo_data$log_Siri, y=congo_data$log_CMCSA, color = sample_stocks$fit.cluster))+ geom_point()
    
    #major_f_data=read.csv("major_factors.csv",header = TRUE,sep = ',')
    
   
    major_f_data=congo_data[,-c(3,4,5,6,7)]
    
    
    major_f_data=na.omit(major_f_data)
    
    net_data=cbind(major_f_data,sample_stocks$fit.cluster)
    colnames(net_data)[14]="Cluster_number"
    
    write.csv(file = "F:/Python_proj/net_service_data.csv", x=net_data)
    #
    ##install.packages("caret")
    ##install.packages("e1071")
    #library(caret)
    #library(e1071)
    #train=createDataPartition(net_data$Cluster_number, p=0.6, list=FALSE)
    #training <- net_data[ train, ]
    #testing <- net_data[ -train, ]
    #
    #mod_fit <- train(net_data$Cluster_number ~ net_data$S.P.500...GSPC. + net_data$CBOE.Interest.Rate.10.Year.T.No...TNX.+ net_data$Nikkei.225...N225.+net_data$Dow.Jones.Industrial.Average...DJI.+net_data$ESTX50.EUR.P...STOXX50E.+net_data$PHLX.Gold.Silver.Sector...XAU.+net_data$Advanced.Micro.Devices..Inc...AMD.,  data=training, method="glm", family="binomial")
