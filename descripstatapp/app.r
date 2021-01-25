# Main fxn 1: ConvertChar: converts character to numeric string, can handle ordinal varialbes 
# will convert them to independent binary response variables, which is 
# appropriate for descriptive analysis, but for other analyses variables 
# should be kept in their ordinal form. Also determines if any columns contain 
# dates and removes them. Otherwise would turn each individual date into a separate 
# categorical variables 

## charlist_creat: removes specified column from dataframe or list, empty if 
## neither dataframe or list 

charlist_create <- function(dataframe_column)
{
  charlist = list()
  for(j in dataframe_column)
  {
    if((is.null(dataframe_column)==TRUE)| j=='')
    {
      next
    }
    ifelse((is_list_element(charlist,j)==FALSE),
           {
             charlist <- append(charlist,j)
           },
           {
             next
           })
  }
  return(charlist)
}

## remove_var: removes specified variable from list 
remove_var <- function(dataframe, var)
{
  ifelse((class(dataframe)=="data.frame"),
         {
           new.frame=data.frame(matrix(nrow=1,ncol=1))
           newcolnames<- list()
           for (i in colnames(dataframe))
           {
             ifelse((var!=i),
                    {
                      new.frame <- data.frame(new.frame, dataframe[[i]])
                      newcolnames <- append(newcolnames, i) 
                      next
                    },
                    {
                      next
                    })
           }
           new.frame <- new.frame[-1]
           colnames(new.frame) <- newcolnames
         },
         {
           ifelse((class(dataframe)=="list"),
                  {
                    new.frame=list()
                    for (i in dataframe)
                    {
                      ifelse((var!=i),
                             {
                               new.frame = append(i, new.frame)
                               next
                             },
                             {
                               next
                             })
                    }
                  },
                  {
                    new.frame = list()
                  })
         })
  return(new.frame)
}

#
ConvertChar <- function(dataframe, group_name)
{
  new_frame <- data.frame(matrix(nrow=1,ncol=1))
  colname_new <- list()
  frame_list <- list()
  for (i in colnames(dataframe))
  {
    if(datedetect(dataframe[[i]])==TRUE)
    {
      next
    }
    e = 1
    ifelse((class(dataframe[[i]])=="character"),
           {
             #colnamelist = colnames(dataframe)
             newvarlist = list()
             charlist = list()
             numlist = list()
             start=1
             count=0
             for(j in dataframe[[i]])
             {
               if((is.null(j)==TRUE)| j=='')
               {
                 next
               }
               ifelse((is_list_element(charlist,j)==FALSE),
                      {
                        charlist <- append(charlist,j)
                      },
                      {
                        next
                      })
             }
             for(x in charlist)
             {
               numlist <- append(numlist,count)
               count=count+1
             }
             ifelse(((length(charlist)>2)&(is_list_element(charlist, group_name)==FALSE)),
                    {
                      #colnamelist <- colnames(dataframe)
                      d = 1
                      m = 1
                      while(m <= length(charlist))
                      { 
                        newvarlist = list()
                        blanklist = list()
                        for (z in dataframe[[i]])
                        {
                          ifelse((z==charlist[[m]]),
                                 {
                                   newvarlist = append(newvarlist, 1)
                                 },
                                 {
                                   newvarlist = append(newvarlist, 0)
                                 })
                          if(e==length(charlist))
                          {
                            blanklist = append(blanklist,'')
                          }
                        }
                        #colnamelist <- append(paste(charlist[[m]]),colnamelist)
                        colname_new <-append(paste(charlist[[m]]),colname_new)
                        #dataframe <- data.frame(unlist(newvarlist),dataframe)
                        new_frame <- data.frame(unlist(newvarlist),new_frame)
                        colnames(new_frame) <- colname_new
                        
                        m = m + 1
                        e = e + 1
                      }
                      dataframe <- remove_var(dataframe, i)
                      #colnamelist <- colnames(dataframe)
                      # dataframe <- data.frame(unlist(blanklist),dataframe)
                      new_frame <-data.frame(unlist(blanklist),new_frame)
                      #colnamelist <- append(paste(i), colnamelist)
                      colname_new <- append(paste(i),colname_new)
                      colnames(new_frame)<-colname_new
                    },
                    {
                      while(start <= length(dataframe[[i]]))
                      {
                        charlistnum=1
                        while(charlistnum<=length(charlist))
                        {
                          ifelse((dataframe[[i]][[start]]==charlist[[charlistnum]]),
                                 {
                                   dataframe[[i]][[start]] <- numlist[[charlistnum]]
                                   start = start+1
                                   break
                                 },
                                 {
                                   charlistnum = charlistnum + 1
                                 })
                        }
                        next
                      }
                      next
                    })
             next
           },
           {
             next
           })
  }
  dataframe <- as.data.frame(apply(dataframe, 2, as.numeric))
  new_frame <- remove_var(new_frame, "NA")
  new_frame <- as.data.frame(apply(new_frame, 2, as.numeric))
  frame_list <- list(dataframe,new_frame)
  return(frame_list)
}
##################################################################################################################
##################################################################################################################

# Main function 2: statsd1: Computes table of descriptive statistics and table of p-values from pairwise comparisons
# group_name is the grouping variable 
# Set fisher allows user to specify the threshold number of observations beneath which fisher's exact test 
# will be used to compare categorical variables. Above this threshold chi squared will be used. 
# fxn will determine if continuous variables are normally distributed or not and use t.test or 
# man-whitney for 2 groups, anova or kriskal wallis for 3 or more groups   
library(dplyr)
library(officer)

# is_normal 
# is_normal takes a continuous variable from a dataframe 
# and determines if the values are normally distributed 
# outputs TRUE if they are, FALSE if not 
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

## is_identical 
## determines if all values in column are identical 
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

## datedetect
## determines if column contains dates. These will be removed 
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

## list_from_user
## function allows user to input names for the column names 
## must ensure that there is no text including commented text beneath the final function 
## call or will not work 
## input is the number of groups being analyzed 
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

## p_value_round
## inputs a pvalue from a statistical test, 
## rounds it to two decimal places if > 0.1, 
## three decmial places if < 0.1
## prints < 0.001 if p is < 0.001 
## returns rounded p value

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

## is_list_element
## Determines if a variable is present in a particular list 
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

statsd1 <- function(dataframe, set_fisher=10, group_name, number_of_groups){
  ifelse((number_of_groups>1),
         {
           categorical_table <- data.frame(matrix(nrow=1,ncol=(number_of_groups+2)))
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
                                        startvar = 1
                                        endvar=2
                                        z=1
                                        while(z <= number_of_groups+5)
                                        {
                                          total_frame = data.frame()
                                          if(number_of_groups==4)
                                          {
                                            guy<-as.data.frame(groups[startvar])
                                            me<-as.data.frame(groups[endvar])
                                            ifelse((is.null(cont_anal_tab[startvar])==FALSE 
                                                    &is.null(cont_anal_tab[endvar])==FALSE
                                                    &sum(na.omit(guy[[i]]))>0
                                                    &sum(na.omit(me[[i]]))>0),
                                                   {
                                                     total_frame <- rbind((as.data.frame(groups[startvar])),(as.data.frame(groups[endvar])))
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
                                                              # z = z + 1
                                                              # next
                                                            })
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       ifelse((endvar==number_of_groups&startvar>1),
                                                              {
                                                                startvar = startvar - 1
                                                                endvar = endvar - 1
                                                              },
                                                              {
                                                                endvar = endvar-2
                                                              })
                                                     }
                                                     endvar = endvar + 1
                                                     z = z + 1
                                                   },
                                                   {
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       ifelse((z==number_of_groups+1),
                                                              {
                                                                startvar = startvar - 1
                                                                endvar = endvar - 1
                                                              },
                                                              {
                                                                endvar = endvar-2
                                                              })
                                                     }
                                                     endvar = endvar + 1
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
                                            guy<-as.data.frame(groups[startvar])
                                            me<-as.data.frame(groups[endvar])
                                            ifelse((is.null(cont_anal_tab[startvar])==FALSE 
                                                    &is.null(cont_anal_tab[endvar])==FALSE
                                                    &sum(na.omit(guy[[i]]))>0
                                                    &sum(na.omit(me[[i]]))>0),
                                                   {
                                                     total_frame <- rbind((as.data.frame(groups[startvar])),(as.data.frame(groups[endvar])))
                                                     total_frame <- rbind((as.data.frame(groups[startvar])),(as.data.frame(groups[endvar])))
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
                                                              # z = z + 1
                                                              # next
                                                            })
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       startvar=startvar-1
                                                       endvar = endvar - 1
                                                     }
                                                     endvar = endvar + 1
                                                     z = z + 1
                                                   },
                                                   {
                                                     if(endvar==number_of_groups&startvar<2)
                                                     {
                                                       startvar = startvar + 1
                                                       startvar=startvar-1
                                                       endvar = endvar-1
                                                     }
                                                     endvar = endvar + 1
                                                     pairlist <- append(pairlist, '')
                                                     z = z + 1
                                                   })
                                          }
                                          if(number_of_groups==5)
                                          {
                                            guy<-as.data.frame(groups[startvar])
                                            me<-as.data.frame(groups[endvar])
                                            ifelse((is.null(cont_anal_tab[startvar])==FALSE 
                                                    &is.null(cont_anal_tab[endvar])==FALSE
                                                    &sum(na.omit(guy[[i]]))>0
                                                    &sum(na.omit(me[[i]]))>0),
                                                   {
                                                     total_frame <- rbind((as.data.frame(groups[startvar])),(as.data.frame(groups[endvar])))
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
                                                              # z = z + 1
                                                              # next
                                                            })
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       ifelse((endvar==number_of_groups&startvar>1),
                                                              {
                                                                startvar = startvar - 1
                                                                endvar = endvar - 1
                                                              },
                                                              {
                                                                endvar = endvar-3
                                                              })
                                                     }
                                                     endvar = endvar + 1
                                                     z = z + 1
                                                   },
                                                   {
                                                     if(endvar==number_of_groups)
                                                     {
                                                       startvar = startvar + 1
                                                       ifelse((z==number_of_groups+1),
                                                              {
                                                                startvar = startvar - 1
                                                                endvar = endvar - 1
                                                              },
                                                              {
                                                                endvar = endvar-2
                                                              })
                                                     }
                                                     endvar = endvar + 1
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
                                                          startvar = 1
                                                          z=1
                                                          endvar=2
                                                          pairlist = list()
                                                          while(z <= number_of_groups+2)
                                                          {
                                                            if(number_of_groups==4)
                                                            {
                                                              ifelse((is.null(cont_anal_tab[startvar])==FALSE 
                                                                      &is.null(cont_anal_tab[endvar])==FALSE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[startvar])),
                                                                                        (unlist(cont_anal_tab[endvar])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar-2
                                                                       }
                                                                       endvar = endvar + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar-2
                                                                       }
                                                                       endvar=endvar+1
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
                                                              ifelse((is.null(cont_anal_tab[startvar])==FALSE 
                                                                      &is.null(cont_anal_tab[endvar])==FALSE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[startvar])),
                                                                                        (unlist(cont_anal_tab[endvar])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar - 1
                                                                       }
                                                                       endvar = endvar + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar - 1
                                                                       }
                                                                       endvar=endvar+1
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
                                                          startvar = 1
                                                          z=1
                                                          endvar=2
                                                          pairlist = list()
                                                          while(z <= number_of_groups+2)
                                                          {
                                                            if(number_of_groups==4)
                                                            {
                                                              ifelse((is.null(cont_anal_tab[startvar])==FALSE 
                                                                      &is.null(cont_anal_tab[endvar])==FALSE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[startvar])),
                                                                                        (unlist(cont_anal_tab[endvar])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar-2
                                                                       }
                                                                       endvar = endvar + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar-2
                                                                       }
                                                                       endvar=endvar+1
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
                                                              ifelse((is.null(cont_anal_tab[startvar])==FALSE 
                                                                      &is.null(cont_anal_tab[endvar])==FALSE),
                                                                     {
                                                                       my_cox <- t.test((unlist(cont_anal_tab[startvar])),
                                                                                        (unlist(cont_anal_tab[endvar])),
                                                                                        alternative = "two.sided")
                                                                       pairlist <- append(pairlist, p_value_round(my_cox$p.value))
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar - 1
                                                                       }
                                                                       endvar = endvar + 1
                                                                       z = z + 1
                                                                     },
                                                                     {
                                                                       if(endvar==number_of_groups&startvar<2)
                                                                       {
                                                                         startvar = startvar + 1
                                                                         endvar = endvar - 1
                                                                       }
                                                                       endvar=endvar+1
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

##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################


# Define UI ----
library(dplyr)
#library(devtools)

library(officer)
#library(flextable)
library(shiny)
#setwd("/Users/andrewmelehy/Applications/documents/biostat/descripstatapp")

library(shinythemes)
library(shinycssloaders)

ui <- fluidPage(
  
  fluidPage(theme = shinytheme("sandstone"),
  titlePanel("Descriptive Statistics"),
  
  sidebarPanel(
    helpText(""),
    fileInput("my_file", 
                label = "Choose .CSV file to Upload",
                multiple=FALSE,
                accept = ".csv"),
    checkboxInput("header", "Header", TRUE),
    
    textInput("my_text",
              label = "Enter grouping variable name"),
    
    sliderInput("group_number", 
                label = "Enter number of groups (up to 5)",
                min=0, max=5, value = c(0)),
    sliderInput("Fisher", 
                label = "Set cutoff value for Fisher's exact test",
                min=0, max=25, value = c(0)),
    
    actionButton("submit", "Submit"),
    br(),
    br(),
    br(),
    downloadButton("downloadData", "Table 1"),
    br(),
    br(),
    downloadButton("downloadData2", "Pairwise table"),
  ),
  
  mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Instructions",
                  br(),
                  p(strong("This app is designed to produce a table of descriptive statistics for clinical research studies. 
                     All uploaded data should have medical record numbers and other protected health information removed
                     as this app is housed on a public server. The data will be temporarily loaded to the server for 
                     analysis, but not saved there. Following closure, the data is removed from the server."),style = "background-color:yellow"),
                  p("Produces a table one with each variable summarized as % (number) for categorical variables 
                    and mean or median (SD or IQR) for continuous variables for each group (supports up to 5 groups). If there are more than 2 groups, will 
                    also produce a table of p-values produced in pairwise testing. Categorical variables can be coded as 0 or 1, or using 
                    character names like 'one' or 'two'. All categorical variables with more than two possible values should be coded as 
                    character names."),
		  p("Example output:"),
                  img(src = "pic6.png", height = 105, width = 520),
                  h3("Step1: Uploading a spreadsheet"),
                  p("Must use .csv file that any excel file can be converted to using 'Save As'
                  and should be in the format where variable/ column names are contained in the first row as 
                  shown below"),
                  img(src = "pic1.png", height = 105, width = 400),
                  br(),
                  h3("Step 2: Input your grouping variable"),
                  p("This app can produce descriptive statistics that compare up to five groups simultaneously. A single 
                    variable must be created in your spreadsheet that differentiates the groups. In the below example the first group is 
                    given a value of 0 in the 'Group' variable and the second group a value of 1. Either numbers or words can be 
                    supplied in the grouping variable like 'one' or 'zero.'"),
                  img(src = "pic2.png", height = 105, width = 400),
                  br(),
                  h3("Step 3: Setting a Fisher's exact test cutoff value"),
                  p("Categorical variables can be compared using the chi-squared test or Fisher's exact test. Chi-squared is 
                    not appropriate for small sample sizes thus requiring the use of Fisher's exact test. The exact test 
                    is technically always appropriate to perform, though commonly it is used when 20% of groups being compared 
                    have < 5 instances of the observed variable. This means that if we are comparing four groups, if one of 
                    those groups had < 5 observations, that is 25% of groups, necessitating the exact test. The chosen cutoff 
                    value in the app means that if any single group has less than that number of observations, the exact test 
                    will be used instead of chi-squared. For a thorough explanation of this topic see reference (1)"),
                  h3("Continuous variables"),
                  p("Continuous variables are compared using either a t-test or mann-whitney U test in comparing two groups and ANOVA 
                    or Kruskal-Wallis if comparing more than two groups. The parametric tests (t-test and ANOVA) are used to compare 
                    group means when variables are normally distributed. If variables are not normally distrubted, the mean is significantly
                    affected by outliers and is thus a poor representation of the 'middle' of the distrubtion, which is why we compare 
                    medians in this case as they more accurately signify the middle of the distribution. Mann-whitney U and Kruskal-wallis are 
                    the non-parametric tests used to compare group medians."),
                  br(),
                  p("Variables are assessed for normality using the Shapiro-wilk test. However, the Shapiro-wilk test is not accurate 
                    when using small sample sizes so if a group has fewer than 5 observations, it is automatically considered 
                    non-normal. "),
                  img(src = "pic3.png", height = 210, width = 600, style="display: block; margin-left: auto; margin-right: auto;"),
                  h3("Multi-level categorical variables"),
                  p("Categorical variables with more than two possible values should be used when these values are mutually exclusive, meaning 
                    that if a patient takes on a certain value of the variable it is not possible that they could take on a different value simultaneously. 
                    A good example of this is if patients were broken into age ranges as an individual cannot be different ages at the same time. In 
                    this case it is most appropriate to treat this variable as a true multi-level categorical variable that will produce a single 
                    p-value in a table 1 denoting differences between groups in how many patients occupy each value of the multi-level categorical variable.
                    Alternatively, for multi-level categorical variables that an individual could have different values for simultaneously are also possible. For 
                    example, a single patient can have multiple medical diagnoses simultaneously. In this case, it is appropriate to transform each 
                    value of the categorical variable into an independent variable (new column in a spreedsheet) where all patients will have either a '0'
                    or '1' for this variable, denoting its presence of absence. All non-mutally exclusive multi-level categorical variables should be transformed 
                    prior to uploading the spreedsheet into this program"),
                  h3("Pairwise testing"),
                  p("When the number of groups is greater than 2, significant results of tests for categorical and continuous variables described above, 
                    simply demonstrate that at least one group median, mean, or number of observations significantly differs from one or more of the other 
                    groups. However, these significant results tells us nothing about which groups differ and at times we require this specific information. 
                    If a comparative test produces a p-value less than 0.05, pairwise testing will automatically be conducted between every possible 
                    pair of groups. The results (p-values) of these tests will be put into a separate table that can be downloaded. The image below shows how 
                    the table column numbers correspond to the pairs of groups tested."),
                  img(src = "pic5.png", height = 350, width = 700, style="display: block; margin-left: auto; margin-right: auto;")
            
                ),
                tabPanel("Table 1", tableOutput("table1") %>% withSpinner(color="#0dc5c1")),
                tabPanel("Pairwise table", tableOutput("pairwise") %>% withSpinner()),
                tabPanel("References", 
                         br(),
                         p("1) Kim H. Y. (2017). Statistical notes for clinical researchers: Chi-squared test and Fisher's exact test. Restorative dentistry & endodontics, 42(2), 152–155. https://doi.org/10.5395/rde.2017.42.2.152")
                         )
    ),

    #tableOutput("table1"),
    #tableOutput("pairwise")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  observeEvent(input$submit, {
    filedata <- reactive({
      myfile <- input$my_file
      if (is.null(myfile)) {
        # User has not uploaded a file yet
        return(NULL)
      }
      read.csv(myfile$datapath, header=input$header, stringsAsFactors = FALSE)
    })
    me <- statsd1(filedata(),input$Fisher,input$my_text,input$group_number)
    output$table1 <- renderTable ({
      me[[1]]
    })
    if(input$group_number>2)
    {
      output$pairwise <- renderTable ({
        me[[2]]
      })
    }
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("mydata", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(me[[1]], file, row.names = FALSE)
      }
    )
    if(input$group_number>2)
    {
      output$downloadData2 <- downloadHandler(
        filename = function() {
          paste("pairwise", ".csv", sep = "")
        },
        content = function(file) {
          write.csv(me[[2]], file, row.names = FALSE)
        }
      )
    }
 
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
