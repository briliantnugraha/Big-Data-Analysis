library(dplyr)
library(data.table)
setwd("H:/Big data Analysis/Ticket")

#read the csv data
concert <- fread("concert.csv")
########################################################################

########################## preprocess ##################################
#filter based on ticketName
#need a chinese language support, and a few mistic setting :D
# i couldn't execute this one yet
# above <- concert[,like(TicketName, "put the comparison here")]

#group the concert by ticketname, solddate, and soldprice
#then sum the data by that grouping, 
#and pick only SoldDate, SoldPrice, TicketName, and total_per_date
filter_concert <- group_by(concert, TicketName, SoldDate, SoldPrice) %>%
                  summarise(total_per_date=n()) %>%
                  select( SoldDate, SoldPrice, TicketName, total_per_date)
########################################################################

##################### dimensionality reduction ######################### 
#in this part, we want to pick the important features
#that might be useful to predict the salespeed of tickets
#we try to use 2 methods:

#a. cluster the data w.r.t the classes (this one is using PLSregression)
#b. cluster the data without knowing the classes (unsupervised learning)

#################### a.PLSregression code ##############################
#to simplify things, just put the classes on the right, and pick the classes
#first, find the classes column location in the concert data, 
#and reorganize the structure

#---note, this gives error if there is blank/missing values---
#so, what should we do about this guys? do we need to ignore it/anything else?
library(plsdepot)
#find the column location of SoldDate
name_loc <- which(colnames(concert) == "SoldDate")

#find the column location of SoldPrice
name_loc2 <- which(colnames(concert) == "SoldPrice")
#put all the class on the right
concert_class_on_right <- concert[,c(1:2, 4:8, 10:20, 3, 9)]
pls2<-plsreg2(concert_class_on_right[,1:18],
        concert_class_on_right[,19:20, drop=FALSE], comps = 5)
#######################################################################

################## b.unsupervised learning code #######################
library(MASS)
library(PearsonICA)
factor_concert <- as.factor( unlist(concert) )
mca_result <- mca(factor_concert, nf = 5)

# ica_result <- PearsonICA(X = concert, n.comp = 5, na.rm = TRUE)
#######################################################################
