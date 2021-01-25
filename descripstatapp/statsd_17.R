library(dplyr)
library(devtools)

library(officer)
library(flextable)

#### fix pairwise, also need to change set fisher, 
### detect averages

### Helper fxn 1: is_normal 
### is_normal takes a continuous variable from a dataframe 
### and determines if the values are normally distributed 
### outputs TRUE if they are, FALSE if not 
is_normal <- function(dataframe_column)
{
  ifelse((length(na.omit(dataframe_column))>5)
         &&(is_identical(dataframe_column)==FALSE),
         {
           myp <- shapiro.test(dataframe_column)
           myp <- myp$p.value
           ifelse(myp<0.05,myp<-FALSE,myp<-TRUE)
           return(myp)       
         },
         {
           return(FALSE)
         })
}

#### helper 2: is_identical 
### determines if all values in column are identical 
is_identical <- function(dataframe_column)
{
  dataframe_column <- na.omit(dataframe_column)
  for(i in dataframe_column)
  {
    ifelse((dataframe_column[[1]]==i),
           {
             identical = TRUE
             next
           },
           {
             identical = FALSE
           })
  }
  return(identical)
}

##### determines if column contains dates. These will be removed 
datedetect <- function(dataframe_column)
{
  ifelse((is.na(as.Date(as.character(dataframe_column),format="%m/%d/%Y"))==FALSE)|(is.na(as.POSIXct(as.character(dataframe_column),format="%m/%d/%Y %H:%M"))==FALSE)
         |(is.na(as.Date(as.character(dataframe_column),format="%B %d, %Y"))==FALSE),
         {
           return(TRUE)
         },
         {
           return(FALSE)
         })
}

# function allows user to input names for the column names 
# must ensure that there is no text including commented text beneath the final function 
# call or will not work 
# input is the number of groups analyzing 
list_from_user <- function(number_of_groups)
{
  t=0
  temp_list <- list()
  name = 1
  while(t<number_of_groups)
  {
    temp_list <- append(temp_list,name)
    name = name + 1 
    t=t+1
  }
  return(temp_list)
}

#### inputs a pvalue from statistical test, 
### rounds it to two decimal places if > 0.1, 
### three decmial places if < 0.1
### prints < 0.001 if p is < 0.001 
### returns rounded p value

p_value_round <- function(pval)
{
  ifelse((pval<0.1),
         {
           ifelse((pval<0.001),
                  {
                    pval <- "< 0.001"
                  },
                  {
                    pval <- round(pval, digits = 3)
                  })
         },
         {
           pval <- round(pval, digits = 2)
         })
  return(pval)
}

##### Determines if a variable is present in a particular list 
is_list_element <- function(list, var) 
{
  i = 1
  test=FALSE
  for (j in list)
  {
    ifelse((var==j),
           {
             test = TRUE 
             break
           },
           {
             next
           })
  }
  ifelse((test==TRUE),
         {
           return(TRUE)     
         },
         {
           return(FALSE)
         })
}

##### main fxn call 
##### all categorical variables must be coded as 0 and 1 
#### group_name is the grouping variables must be in the format 
#### group 1 = 0, group 2 = 1, group 3 = 2...etc 
### Set fisher allows user to specify the threshold
### number of observations beneath which fisher's exact test 
### will be used to compare categorical variables. Above this 
### threshold chi squared will be used 
### fxn will determine if continuous variables are normally distributed 
### or not and use t.test or man whitney for 2 groups, anova or 
### kriskal wallis for 3 or more groups 
statsd1 <- function(dataframe, set_fisher=10, group_name, number_of_groups){
  # input data from dataframe, opened from csv above fxn call  
  # defining output table with number of columns equal to the number of groups plus 
  # one for the variable name and one for the p-value 
  
  ifelse((number_of_groups>1),
         {
           categorical_table <- data.frame(matrix(nrow=1,ncol=(number_of_groups+2)))
           #pairtable = data.frame(matrix(nrow=1,ncol=(number_of_groups+3)))
         },
         {
           categorical_table <- data.frame(matrix(nrow=1,ncol=(number_of_groups+1)))
         })
  # Takes group_name and converts it to numeric starting from 0 if 
  # it is in character form 
  temp_dat = subset(dataframe, select=c(group_name))
  temp_dat = ConvertChar(temp_dat, group_name)
  
  dataframe[[group_name]] <- temp_dat[[1]][[group_name]]
  # #call ConvertChar on inputted dataframe to convert all character strings to numbers, 
  # #will order values 0,1,2,etc. 
  
  dataframe_list <- ConvertChar(dataframe, group_name)
  dataframe1 <- dataframe_list[[1]]
  
  #intialzes list to hold table column names 
  cattab_colnames=list()
  pairtab_colnames=list()
  
  #indexes for while loop that produces column names 
  n = 0 
  m = 1
  
  # intializes empty list that we will use to access each group by index 
  groups <- list()
  
  #produces column names of ultimate output table 
  #structured variable, group ____, group _____,..., p-val 
  #my_list_colnames <- list(1:number_of_groups)
  while (n<number_of_groups)
  {
    groups[[m]] <- dataframe1[dataframe1[[group_name]]==n,]
    total <- paste0("(","n= ",((length(na.omit(groups[[m]][[group_name]])))),")")
    cattab_colnames <- append(cattab_colnames, 
                              list(paste(m,total)))
    pairtab_colnames <- append(pairtab_colnames,
                               list(paste(m)))
    m = m+1
    n = n + 1
  }
  categorical_table <- data.frame(matrix(nrow=1,ncol=(number_of_groups+2)))
  ifelse((number_of_groups>1),
         {
           colnames(categorical_table)<-c("variable", cattab_colnames, "P-value")
         },
         {
           colnames(categorical_table)<-c("variable",cattab_colnames)
         })
  pairtable = data.frame(matrix(nrow=1,ncol=(number_of_groups+3)))
  colnames(pairtable)<- c("variable",pairtab_colnames,number_of_groups+1,number_of_groups+2)

  # outer most loop that sequentially steps through each variable in the imported csv 
  for (i in colnames(dataframe1)){
    if((length(na.omit(dataframe1[[i]]))<5))
    {
      next
    }
    if(i == group_name)
    {
      next 
    }
    a=1
    b = 1
    c=1
    d=1
    fish=0
    cat_list_percent = list()
    cat_list_num = list()
    num_percentlist=list()
    cont_anal_tab=list()
    my_list=list()
    norm_list=list()
    varskip = 0
    
    ifelse((((is.integer(dataframe1[[i]])==TRUE|is.numeric(dataframe1[[i]])==TRUE)&
               (max(dataframe1[[i]],na.rm=TRUE)==1))),
           {
             while(c<(number_of_groups+1))
             {
               if((sum(na.omit(groups[[c]][[i]])))<set_fisher)
               {
                 fish=1
               }
               guy <- (length(na.omit(groups[[c]][[i]])))
               sum <- (sum(groups[[c]][[i]], na.rm=TRUE))
               cat_list_percent[[c]] = paste0((round(((sum / guy)*100),digits=0)),"%")
               sum <- paste0("(",sum,")")
               cat_list_percent[[c]]=paste((cat_list_percent[[c]]),sum)
               num_percentlist<-append(num_percentlist,
                                       (cat_list_percent[[c]]))
               c = c + 1
             }
             
             ifelse((number_of_groups>1),
                    {
                  ifelse((number_of_groups<3),
                         {
                           ifelse((fish==0),
                                  {
                                    mytab <- with(dataframe1,table(dataframe1[[group_name]],
                                                                  dataframe1[[i]]))
                                    my_chi <- chisq.test(mytab)
                                    categorical_table <-rbind(categorical_table, 
                                                              c(i,num_percentlist, 
                                                                p_value_round(my_chi$p.value)))
                                  },
                                  {
                                    mytab <- with(dataframe1,table(dataframe1[[group_name]],
                                                                  dataframe1[[i]]))
                                    my_fisher <- try(fisher.test(mytab, workspace = 2*10^8))
                                    ifelse(((class(my_fisher))=="try-error"),
                                           {
                                             categorical_table <-rbind(categorical_table, 
                                                                       c(i,num_percentlist, 
                                                                         "error"))
                                           },
                                           {
                                             categorical_table <-rbind(categorical_table, 
                                                                       c(i,num_percentlist, 
                                                                         p_value_round(my_fisher$p.value)))
                                           })
                                  })
                         },
                         {
                           ifelse((fish==0),
                                  {
                                    mytab <- with(dataframe1,table(dataframe1[[group_name]],
                                                                  dataframe1[[i]]))
                                    my_test <- chisq.test(mytab)
                                    categorical_table <-rbind(categorical_table, 
                                                              c(i,num_percentlist, 
                                                                p_value_round(my_test$p.value)))
                                  },
                                  {
                                    mytab <- with(dataframe1,table(dataframe1[[group_name]],
                                                                  dataframe1[[i]]))
                                    my_test <- try(fisher.test(mytab, workspace = 2*10^8))
                                    ifelse(((class(my_test))=="try-error"),
                                           {
                                             categorical_table <-rbind(categorical_table, 
                                                                       c(i,num_percentlist, 
                                                                         "error"))
                                             next
                                           },
                                           {
                                             categorical_table <-rbind(categorical_table, 
                                                                       c(i,num_percentlist, 
                                                                         p_value_round(my_test$p.value)))
                                           })
                                  })
                           pairlist = list()
                           
                                    ifelse((my_test$p.value<0.05),
                                           {
                                             startvar = 0
                                             endvar=1
                                             cream = 1
                                             z=1
                                             ream=2
                                             while(z <= number_of_groups+5)
                                             {
                                               total_frame = data.frame()
                                               if(number_of_groups==4)
                                               {
                                                 guy<-as.data.frame(groups[cream])
                                                 me<-as.data.frame(groups[ream])
                                                 ifelse((is.null(cont_anal_tab[cream])==FALSE 
                                                         &is.null(cont_anal_tab[ream])==FALSE
                                                         &sum(na.omit(guy[[i]]))>0
                                                         &sum(na.omit(me[[i]]))>0),
                                                        {
                                                          total_frame <- rbind((as.data.frame(groups[cream])),(as.data.frame(groups[ream])))
                                                          ifelse(((sum(total_frame[[i]], na.rm=TRUE)>0)),
                                                                 {
                                                                   ifelse((fish==0),
                                                                          {
                                                                            newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                             total_frame[[i]]))
                                                                            my_test <- chisq.test(newtab)
                                                                            pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                          },
                                                                          {
                                                                            newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                             total_frame[[i]]))
                                                                            my_test <- try(fisher.test(newtab, workspace = 2*10^8),silent=TRUE)
                                                                            ifelse((class(my_test)=="try-error"),
                                                                                   {
                                                                                     pairlist <- append(pairlist, "no observed values in both groups")
                                                                                   },
                                                                                   {
                                                                                     pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                                   })
                                                                          
                                                                          })
                                                                 },
                                                                 {
                                                                   # endvar = endvar + 1 
                                                                   # ream = ream + 1
                                                                   # z = z + 1
                                                                   # next
                                                                 })
                                                          if(ream==number_of_groups)
                                                          {
                                                            cream = cream + 1
                                                            ifelse((ream==number_of_groups&cream>1),
                                                                   {
                                                                     # print(z)
                                                                     # print("one")
                                                                     # print(cream)
                                                                     # print(ream)
                                                                     # print('')
                                                                     cream = cream - 1
                                                                     ream = ream - 1
                                                                   },
                                                                   {
                                                                     # print(z)
                                                                     # print("two")
                                                                     # print(cream)
                                                                     # print(ream)
                                                                     # print('')
                                                                     ream = ream-2
                                                                   })
                                                          }
                                                          ream = ream + 1
                                                          z = z + 1
                                                        },
                                                        {
                                                          if(ream==number_of_groups)
                                                          {
                                                            cream = cream + 1
                                                            ifelse((z==number_of_groups+1),
                                                                   {
                                                                     cream = cream - 1
                                                                     ream = ream - 1
                                                                   },
                                                                   {
                                                                     ream = ream-2
                                                                   })
                                                          }
                                                          ream = ream + 1
                                                          pairlist <- append(pairlist, '')
                                                          z = z + 1
                                                        })
                                               }
                                               if(number_of_groups==3)
                                               {
                                                 if(z>3)
                                                 {
                                                   pairlist <- append(pairlist,'')
                                                   pairlist <- append(pairlist,'')
                                                   pairlist <- append(pairlist,'')
                                                   break
                                                 }
                                                 guy<-as.data.frame(groups[cream])
                                                 me<-as.data.frame(groups[ream])
                                                 ifelse((is.null(cont_anal_tab[cream])==FALSE 
                                                         &is.null(cont_anal_tab[ream])==FALSE
                                                         &sum(na.omit(guy[[i]]))>0
                                                         &sum(na.omit(me[[i]]))>0),
                                                        {
                                                          total_frame <- rbind((as.data.frame(groups[cream])),(as.data.frame(groups[ream])))
                                                          total_frame <- rbind((as.data.frame(groups[cream])),(as.data.frame(groups[ream])))
                                                          ifelse(((sum(total_frame[[i]], na.rm=TRUE)>0)),
                                                                 {
                                                                   ifelse((fish==0),
                                                                          {
                                                                            newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                             total_frame[[i]]))
                                                                            my_test <- chisq.test(newtab)
                                                                            pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                          },
                                                                          {
                                                                            newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                             total_frame[[i]]))
                                                                            my_test <- fisher.test(newtab)
                                                                            pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                            
                                                                            #### fix 
                                                                          })
                                                                   
                                                                 },
                                                                 {
                                                                   # endvar = endvar + 1 
                                                                   # ream = ream + 1
                                                                   # z = z + 1
                                                                   # next
                                                                 })
                                                          if(ream==number_of_groups)
                                                          {
                                                            cream = cream + 1
                                                            cream=cream-1
                                                            ream = ream - 1
                                                          }
                                                          ream = ream + 1
                                                          z = z + 1
                                                        },
                                                        {
                                                          if(ream==number_of_groups&cream<2)
                                                          {
                                                            cream = cream + 1
                                                            cream=cream-1
                                                            ream = ream-1
                                                          }
                                                          ream = ream + 1
                                                          pairlist <- append(pairlist, '')
                                                          z = z + 1
                                                        })
                                               }
                                               if(number_of_groups==5)
                                               {
                                                 guy<-as.data.frame(groups[cream])
                                                 me<-as.data.frame(groups[ream])
                                                 ifelse((is.null(cont_anal_tab[cream])==FALSE 
                                                         &is.null(cont_anal_tab[ream])==FALSE
                                                         &sum(na.omit(guy[[i]]))>0
                                                         &sum(na.omit(me[[i]]))>0),
                                                        {
                                                          total_frame <- rbind((as.data.frame(groups[cream])),(as.data.frame(groups[ream])))
                                                          ifelse(((sum(total_frame[[i]], na.rm=TRUE)>0)),
                                                                 {
                                                                   ifelse((fish==0),
                                                                          {
                                                                            newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                             total_frame[[i]]))
                                                                            my_test <- chisq.test(newtab)
                                                                            pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                          },
                                                                          {
                                                                            newtab <- with(total_frame,table(total_frame[[group_name]],
                                                                                                             total_frame[[i]]))
                                                                            my_test <- fisher.test(newtab, workspace = 2*10^8)
                                                                            #### fix
                                                                            pairlist <- append(pairlist, p_value_round(my_test$p.value))
                                                                          })
                                                                 },
                                                                 {
                                                                   # endvar = endvar + 1 
                                                                   # ream = ream + 1
                                                                   # z = z + 1
                                                                   # next
                                                                 })
                                                          if(ream==number_of_groups)
                                                          {
                                                            cream = cream + 1
                                                            ifelse((ream==number_of_groups&cream>1),
                                                                   {
                                                                     # print(z)
                                                                     # print("one")
                                                                     # print(cream)
                                                                     # print(ream)
                                                                     # print('')
                                                                     cream = cream - 1
                                                                     ream = ream - 1
                                                                   },
                                                                   {
                                                                     # print(z)
                                                                     # print("two")
                                                                     # print(cream)
                                                                     # print(ream)
                                                                     # print('')
                                                                     ream = ream-3
                                                                   })
                                                          }
                                                          ream = ream + 1
                                                          z = z + 1
                                                        },
                                                        {
                                                          if(ream==number_of_groups)
                                                          {
                                                            cream = cream + 1
                                                            ifelse((z==number_of_groups+1),
                                                                   {
                                                                     cream = cream - 1
                                                                     ream = ream - 1
                                                                   },
                                                                   {
                                                                     ream = ream-2
                                                                   })
                                                          }
                                                          ream = ream + 1
                                                          pairlist <- append(pairlist, '')
                                                          z = z + 1
                                                        })

                                               }
                                             }
                                             pairtable <- rbind(pairtable,
                                                                c(i,unlist(pairlist)))
                                             
                                             #next
                                           },
                                           {
                                             next
                                           })
                        })
                    },
                    {
                      categorical_table <-rbind(categorical_table, 
                                                c(i,num_percentlist, 
                                                  ''))
                    })
             
           },
           {
             ifelse(((is.numeric(dataframe1[[i]]))==TRUE),
                    {
                      normal = 0
                      while(c<(number_of_groups+1))
                      {
                        if(is_normal(groups[[c]][[i]])==TRUE)
                        {
                          normal = normal + 1 
                        }
                        
                        ifelse(((length(na.omit(groups[[c]][[i]])))>3),
                               {
                                 guy <- median(groups[[c]][[i]],na.rm=TRUE)
                                 quart1 <- quantile(groups[[c]][[i]],c(0.25),na.rm=TRUE)
                                 quart3 <- quantile(groups[[c]][[i]],c(0.75),na.rm=TRUE)
                                 IQR <- paste0("(",round(quart1,digits=2),"-",
                                               round(quart3,digits=2),")")
                                 med_quart <- paste(round(guy,digits=2),IQR)
                                 my_list<-append(my_list, med_quart)
                                 cont_anal_tab<-append(cont_anal_tab,
                                                       list(jitter(groups[[c]][[i]])))
                                 c=c+1
                               },
                               {
                                 c=c+1
                                 
                               })
                      }
                      
                      ifelse((normal==number_of_groups),
                             {
                               while(a<(number_of_groups+1))
                               {
                                 guy <- mean(groups[[a]][[i]],na.rm=TRUE)
                                 std <- sd(groups[[a]][[i]],na.rm=TRUE)
                                 stdpaste <- paste("±", round(std,digits=2))
                                 men_std <- paste(round(guy,digits=2),stdpaste)
                                 norm_list<-append(norm_list, men_std)
                                 list(jitter(groups[[a]][[i]]))
                                 a=a+1
                                 
                               }
                               ifelse((number_of_groups>1),
                                      {
                                        ifelse((number_of_groups<3),
                                               {
                                                 my_cox <- t.test((unlist(cont_anal_tab[1])),
                                                                  (unlist(cont_anal_tab[2])),
                                                                  alternative = "two.sided")
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,unlist(norm_list),
                                                                             (p_value_round(my_cox$p.value))))
                                               },
                                               {
                                                 my_anova <- aov(dataframe1[[i]]~dataframe1[[group_name]], data=dataframe1)
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,unlist(norm_list), 
                                                                             (p_value_round(summary(my_anova)[[1]][["Pr(>F)"]]))))
                                                 
                                                 ifelse((summary(my_anova)[[1]][["Pr(>F)"]]<0.05),
                                                        {
                                                          cream = 1
                                                          z=1
                                                          ream=2
                                                          pairlist = list()
                                                          while(z <= number_of_groups+2)
                                                          {
                                                            if(number_of_groups==4)
                                                                   {
                                                                     ifelse((is.null(cont_anal_tab[cream])==FALSE 
                                                                             &is.null(cont_anal_tab[ream])==FALSE),
                                                                            # &is.numeric(cont_anal_tab[cream])==TRUE
                                                                            #&is.numeric(cont_anal_tab[ream])==TRUE),
                                                                            {
                                                                              my_cox <- t.test((unlist(cont_anal_tab[cream])),
                                                                                               (unlist(cont_anal_tab[ream])),
                                                                                               alternative = "two.sided")
                                                                              pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                              if(ream==number_of_groups&cream<2)
                                                                              {
                                                                                cream = cream + 1
                                                                                ream = ream-2
                                                                              }
                                                                              ream = ream + 1
                                                                              z = z + 1
                                                                            },
                                                                            {
                                                                              if(ream==number_of_groups&cream<2)
                                                                              {
                                                                                cream = cream + 1
                                                                                ream = ream-2
                                                                              }
                                                                              ream=ream+1
                                                                              pairlist <- append(pairlist, '')
                                                                              z = z + 1
                                                                            })
                                                                   }
                                                            if(number_of_groups==3)
                                                            {
                                                              if(z>3)
                                                              {
                                                                pairlist <- append(pairlist,'')
                                                                pairlist <- append(pairlist,'')
                                                                pairlist <- append(pairlist,'')
                                                                break
                                                              }
                                                              ifelse((is.null(cont_anal_tab[cream])==FALSE 
                                                                      &is.null(cont_anal_tab[ream])==FALSE),
                                                                     # &is.numeric(cont_anal_tab[cream])==TRUE
                                                                     #&is.numeric(cont_anal_tab[ream])==TRUE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[cream])),
                                                                                        (unlist(cont_anal_tab[ream])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(ream==number_of_groups&cream<2)
                                                                       {
                                                                         cream = cream + 1
                                                                         ream = ream - 1
                                                                       }
                                                                       ream = ream + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(ream==number_of_groups&cream<2)
                                                                       {
                                                                         cream = cream + 1
                                                                         ream = ream - 1
                                                                       }
                                                                       ream=ream+1
                                                                       pairlist <- append(pairlist, '')
                                                                       z = z + 1
                                                                     })
                                                            }
                                                            # if(number_of_groups==5)
                                                            # {
                                                            #   
                                                            # }
                                                          }
                                                          pairtable <- rbind(pairtable,
                                                                             c(i,unlist(pairlist)))
                                                          next
                                                        },
                                                        {
                                                          next
                                                        })
                                               })
                                      },
                                      {
                                        categorical_table <-rbind(categorical_table,
                                                                  c(i,unlist(norm_list),
                                                                    ""))
                                      })
                             },
                             {
                               ifelse((number_of_groups>1),
                                      {
                                        ifelse((number_of_groups<3),
                                               {
                                                 my_cox <- wilcox.test((unlist(cont_anal_tab[1])),
                                                                       (unlist(cont_anal_tab[2])),
                                                                       alternative = "two.sided")
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,(unlist(my_list)),
                                                                             (p_value_round(my_cox$p.value))))
                                               },
                                               {
                                                 
                                                 my_kw <- kruskal.test(dataframe1[[i]]~dataframe1[[group_name]], data=dataframe1)
                                                 categorical_table <-rbind(categorical_table,
                                                                           c(i,unlist(my_list), 
                                                                             (p_value_round(my_kw$p.value))))
                                                 ifelse((my_kw$p.value<0.05),
                                                        {
                                                          cream = 1
                                                          z=1
                                                          ream=2
                                                          pairlist = list()
                                                          while(z <= number_of_groups+2)
                                                          {
                                                            if(number_of_groups==4)
                                                            {
                                                              ifelse((is.null(cont_anal_tab[cream])==FALSE 
                                                                      &is.null(cont_anal_tab[ream])==FALSE),
                                                                     # &is.numeric(cont_anal_tab[cream])==TRUE
                                                                     #&is.numeric(cont_anal_tab[ream])==TRUE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[cream])),
                                                                                        (unlist(cont_anal_tab[ream])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(ream==number_of_groups&cream<2)
                                                                       {
                                                                         cream = cream + 1
                                                                         ream = ream-2
                                                                       }
                                                                       ream = ream + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(ream==number_of_groups&cream<2)
                                                                       {
                                                                         cream = cream + 1
                                                                         ream = ream-2
                                                                       }
                                                                       ream=ream+1
                                                                       pairlist <- append(pairlist, '')
                                                                       z = z + 1
                                                                     })
                                                            }
                                                            if(number_of_groups==3)
                                                            {
                                                          
                                                              if(z>3)
                                                              {
                                                                pairlist <- append(pairlist,'')
                                                                pairlist <- append(pairlist,'')
                                                                pairlist <- append(pairlist,'')
                                                                break
                                                              }
                                                              ifelse((is.null(cont_anal_tab[cream])==FALSE 
                                                                      &is.null(cont_anal_tab[ream])==FALSE),
                                                                     # &is.numeric(cont_anal_tab[cream])==TRUE
                                                                     #&is.numeric(cont_anal_tab[ream])==TRUE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[cream])),
                                                                                        (unlist(cont_anal_tab[ream])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(ream==number_of_groups&cream<2)
                                                                       {
                                                                         cream = cream + 1
                                                                         ream = ream - 1
                                                                       }
                                                                       ream = ream + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(ream==number_of_groups&cream<2)
                                                                       {
                                                                         cream = cream + 1
                                                                         ream = ream - 1
                                                                       }
                                                                       ream=ream+1
                                                                       pairlist <- append(pairlist, '')
                                                                       z = z + 1
                                                                     })
                                                            }
                                                            # if(number_of_groups==5)
                                                            # {
                                                            #   
                                                            # }
                                                          }
                                                          pairtable <- rbind(pairtable,
                                                                             c(i,unlist(pairlist)))
                                                          next
                                                        },
                                                        {
                                                          next
                                                        })
                                               })
                                      },
                                      {
                                        categorical_table <-rbind(categorical_table,
                                                                  c(i,unlist(my_list), 
                                                                    ''))
                                      })
                             })
                      
                    }, next)
           })
  }
  ifelse((number_of_groups>2), 
  {
    pairtable <- pairtable[-1,]
  },
  {
    pairtable <- data.frame()
  })
  
  x = 1 
  column_list <- list()
  while(x < (length(colnames(categorical_table))-1))
  {
    column_list <- append(column_list, '')
    x=x+1
  }
  collist<-list()
  
  q = 0
  for (b in colnames(dataframe_list[[2]]))
  {
    q = q + 1
  }
  
  ifelse((q==1),
         {
           ord_frame<-data.frame(dataframe1[[group_name]])
           collist<-colnames(ord_frame)
           collist[[1]]=group_name
           colnames(ord_frame)<-collist
         },
         {
           ord_frame<-data.frame(dataframe1[[group_name]], dataframe_list[[2]])
           collist<-colnames(ord_frame)
           collist[[1]]=group_name
           colnames(ord_frame)<-collist
         })

  n = 0
  m = 1
  group<-list()
  while (n<number_of_groups)
  {
    group[[m]] <- ord_frame[ord_frame[[1]]==n,]
    m = m+1
    n = n + 1
  }
 
  for (j in colnames(ord_frame))
  {
    if(j == group_name)
    {
      next
    }
    
    h = 1
    while(h <= length(colnames(ord_frame)))
    {
      ifelse(((paste(ord_frame[[j]][[1]]))=='NA'),
             {
               h = h + 1
             },
             {
               var1 <- 1 
               while(var1 < number_of_groups)
               {
                 ifelse((sum(na.omit(group[[var1]][[colnames(ord_frame)[h]]]))<set_fisher),
                        {
                          fish = 1
                        },
                        {
                          fish = 0
                        })
                 var1 = var1 + 1 
               }
               h = h + 1
             })
    }
    ifelse(((paste(ord_frame[[j]][[1]]))=='NA'),
           {
             
             ifelse((fish==0),
                    {
                      dataframe[[j]] = factor(dataframe[[j]])
                      mytab <- with(dataframe,table(dataframe[[group_name]],
                                                    dataframe[[j]]))
                      my_test <- chisq.test(mytab)
                      categorical_table <- rbind(categorical_table,
                                                 c(j,unlist(column_list),p_value_round(my_test$p.value)))
                      next
                    },
                    {
                      dataframe[[j]] = factor(dataframe[[j]])
                      mytab <- with(dataframe,table(dataframe[[group_name]],
                                                    dataframe[[j]]))
                      my_test <- try(fisher.test(mytab, workspace = 2*10^8),silent=TRUE)
                      ifelse((class(my_test)=="try-error"),
                             {
                               categorical_table <- rbind(categorical_table,
                                                          c(j,unlist(column_list),"no observed values in both groups"))
                             },
                             {
                               categorical_table <- rbind(categorical_table,
                                                          c(j,unlist(column_list),p_value_round(my_test$p.value)))
                               next
                             })
                    })
           },
           {
             z=1
             cat_list_percent = list()
             num_percentlist = list()
             while(z <= number_of_groups)
             {
               if((length(na.omit(groups[[z]][[j]])))<set_fisher)
               {
                 
                 fish=1
                 
               }
               guy <- (length(na.omit(group[[z]][[j]])))
               sum <- (sum(group[[z]][[j]], na.rm=TRUE))
               cat_list_percent[[z]] = paste0((round(((sum / guy)*100),digits=0)),"%")
               sum <- paste0("(",sum,")")
               cat_list_percent[[z]]=paste((cat_list_percent[[z]]),sum)
               num_percentlist<-append(num_percentlist,
                                       (cat_list_percent[[z]]))
               z = z + 1
             }
             categorical_table <-rbind(categorical_table,
                                      c(j,num_percentlist,''))
 
             next
           })
  }
  categorical_table <- categorical_table[-1,]
  my_table_list = list() 
  my_table_list = list(categorical_table, pairtable)
}

##### fix fisher for ordinal analysis 
#### fix fisher for pairwise. Pairwise doesn't work


#### flex-table to output to .docx 
#### takes table (cattable) and ouputs it is a formatted table 
### in word to specified output file "outfile." Output file must be 
### format "filename.docx"
flex_tab_addnew <- function(cattable, outfile)
{
  catflex <- qflextable(cattable)
  catflex<-autofit(catflex)
  doc <- read_docx()
  doc <- body_add_flextable(doc, value = catflex)
  print(doc, target = outfile)
}