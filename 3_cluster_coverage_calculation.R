#3: calculate coverage at cluster level

temp_length <- aggregate(cbind(subset_lessthan59$nigdata.H9), by=list(cluster=subset_lessthan59$nigdata.V001), FUN=length)

temp_sums <- aggregate(list(coverage=subset_lessthan59$nigdata.H9
                        #    ,edu=subset_lessthan59$nigdata.V106,
                         #   urbrur=subset_lessthan59$nigdata.V025,
                          #  wealth= subset_lessthan59$nigdata.V190
                        ), 
                       by=list(cluster=subset_lessthan59$nigdata.V001), FUN =sum)
 
temp_sums$coverage <- (temp_sums$coverage/temp_length$V1)*100
#temp_sums$edu <- (temp_sums$edu/temp_length$V1)*100
#temp_sums$urbrur <- (temp_sums$urbrur/temp_length$V1)*100
#temp_sums$wealth <- (temp_sums$wealth/temp_length$V1)*100
temp_sums$numbers <- (temp_length$V1)

subset_cluster$coverage=temp_sums$coverage
