##8.26
##LULU： https://github.com/tobiasgf/lulu

library(devtools)
install_github("tobiasgf/lulu")    #报错invalid multibyte string, element 1

#参考
#https://bbs.pinggu.org/thread-3640539-2-1.html
Sys.setlocale(category = "LC_ALL", locale = "chs")
#cht是繁体中文
#chs是简体中文
#us是美国英语 
Sys.setlocale()
#改成英语就行了
Sys.setlocale(category = "LC_ALL", locale = "us")


library(lulu)
?lulu
lulu



lulu(otutable, matchlist, minimum_ratio_type = "min", minimum_ratio = 1,
     minimum_match = 84, minimum_relative_cooccurence = 0.95)

#lulu(my_table, my_matchlist)


setwd("E:/桌面/test_data/")
# library(vroom)
# otu = vroom("Galaxy119-[UPARSE_otu_table.txt].tabular")
# matchlist = vroom("match_list.txt")

##uparse
otu = read.table(file="Galaxy119-[UPARSE_otu_table.txt].tabular",sep="\t",header=T,row.names = 1)
matchlist = read.table(file="uparse_match_list.txt",sep="\t")
richness = as.data.frame(colSums(otu>0))
rich_gg = as.data.frame(cbind(c(1:92),richness[,1]))
colnames(rich_gg) = c("sample","richness")
rich_gg

curated_otu = lulu(otu,matchlist)

curated_tab = curated_otu$curated_table
curated_otus = curated_otu$curated_otus
discarded_count = curated_otu$discarded_count

curated_richness = as.data.frame(colSums(curated_tab>0))
curated_rich_gg = as.data.frame(cbind(c(1:92),curated_richness[,1]))
colnames(curated_rich_gg) = c("sample","richness")

##unoise
otu = read.table(file="Galaxy115-[Unoise3_otu_table.txt].tabular",sep="\t",header=T,row.names = 1)
matchlist = read.table(file="unoise_match_list.txt",sep="\t")
richness = as.data.frame(colSums(otu>0))
uno_rich_gg = as.data.frame(cbind(c(1:92),richness[,1]))
colnames(uno_rich_gg) = c("sample","richness")
uno_rich_gg

curated_otu = lulu(otu,matchlist)

curated_tab = curated_otu$curated_table
curated_otus = curated_otu$curated_otus
discarded_count = curated_otu$discarded_count

curated_richness = as.data.frame(colSums(curated_tab>0))
uno_curated_rich_gg = as.data.frame(cbind(c(1:92),curated_richness[,1]))
colnames(uno_curated_rich_gg) = c("sample","richness")


library(ggplot2)
plot_theme = theme(panel.background=element_blank(),
                   panel.grid=element_blank(),
                   axis.line.x=element_line(size=.5, colour="black"),
                   axis.line.y=element_line(size=.5, colour="black"),
                   axis.ticks=element_line(color="black"),
                   axis.text=element_text(color="black", size=24),
                   legend.position="right",
                   legend.background=element_blank(),
                   legend.key=element_blank(),
                   legend.text= element_text(size=24),
                   text=element_text(family="sans", size=24)
) 
p = ggplot() + geom_point(data=rich_gg,aes(x=sample,y=richness),color="red",size=2)+
                geom_point(data=curated_rich_gg,aes(x=sample,y=richness),color="orange",size=2)+
              geom_point(data=uno_rich_gg,aes(x=sample,y=richness),color="blue",size=2)+
                geom_point(data=uno_curated_rich_gg,aes(x=sample,y=richness),color="black",size=2)+
               geom_hline(aes(yintercept=8))+
                plot_theme+scale_x_continuous(labels = NULL)+ theme(legend.position="None")

p


?install_github
#install_github其实是remotes包中的函数
#remotes::install_github("tobiasgf/lulu")

?iconv()
#读文件报nvalid multibyte string, element 1可用这个
#iconv file.pcl -f UTF-8 -t ISO-8859-1 -c 
#-c 跳过不能转义的字符




####lulu源代码
lulu <- function(otutable, matchlist, minimum_ratio_type = "min", minimum_ratio = 1, minimum_match = 84, minimum_relative_cooccurence = 0.95) {
  
  require(dplyr)
  
  start.time <- Sys.time()
  
  colnames(matchlist) <- c("OTUid", "hit", "match")
  
  # remove no hits (vsearch)
  
  matchlist = matchlist[which(matchlist$hit != "*"), ]
  
  # remove self-hits
  
  matchlist = matchlist[which(matchlist$hit != matchlist$OTUid), ]
  
  # Making a separate table with stats (total readcount and spread).
  
  # otutable = otu 自己加的一行
  statistics_table <- otutable[, 0]
  
  statistics_table$total <- rowSums(otutable)
  
  
  
  # calculating spread (number of presences (samples with 1+ read) pr OTU)
  
  statistics_table$spread <- rowSums(otutable > 0) #包含此OTU的样本的个数
  
  statistics_table <- statistics_table[with(statistics_table,
                                            
                                            order(spread,
                                                  
                                                  total,
                                                  
                                                  decreasing = TRUE)), ]
  
  otutable <- otutable[match(row.names(statistics_table),
                             
                             row.names(otutable)), ]
  
  
  
  statistics_table$parent_id <- "NA"
  
  log_con <- file(paste0("lulu.log_", format(start.time, "%Y%m%d_%H%M%S")),
                  
                  open = "a")
  
  for (line in seq(1:nrow(statistics_table))) {  #line=2
    
    # make a progressline
    
    print(paste0("progress: ",
                 
                 round(((line/nrow(statistics_table)) * 100), 0), "%"))
    
    potential_parent_id <- row.names(otutable)[line]
    
    cat(paste0("\n", "####processing: ", potential_parent_id, " #####"),
        
        file = log_con)
    
    daughter_samples <- otutable[line, ]
    
    #minimum_match = 84
    
    hits <- matchlist[which(matchlist$OTUid == potential_parent_id &
                              
                              matchlist$match > minimum_match), "hit"]
    
    cat(paste0("\n", "---hits: ", hits), file = log_con)
    
    last_relevant_entry <- sum(statistics_table$spread >=
                                 
                                 statistics_table$spread[line])
    
    potential_parents <- which(row.names(otutable)[1:last_relevant_entry]
                               
                               %in% hits)
    
    cat(paste0("\n", "---potential parent: ",
               
               row.names(statistics_table)[potential_parents]), file = log_con)
    
    success <- FALSE
    
    if (length(potential_parents) > 0) {
      
      for (line2 in potential_parents) {  #line2 = 1
        
        cat(paste0("\n", "------checking: ", row.names(statistics_table)[line2]),
            
            file = log_con)
        
        if (!success) {
          
          relative_cooccurence <-
            
            sum((daughter_samples[otutable[line2, ] > 0]) > 0)/
            
            sum(daughter_samples > 0)
          
          cat(paste0("\n", "------relative cooccurence: ",
                     
                     relative_cooccurence), file = log_con)
          
          if (relative_cooccurence >= minimum_relative_cooccurence) {
            
            cat(paste0(" which is sufficient!"), file = log_con)
            
            if (minimum_ratio_type == "avg") {
              
              relative_abundance <-
                
                mean(otutable[line2, ][daughter_samples > 0]/
                       
                       daughter_samples[daughter_samples > 0])
              
              cat(paste0("\n", "------mean avg abundance: ",
                         
                         relative_abundance), file = log_con)
              
            } else {
              
              relative_abundance <-
                
                min(otutable[line2, ][daughter_samples > 0]/
                      
                      daughter_samples[daughter_samples > 0])
              
              cat(paste0("\n", "------min avg abundance: ",
                         
                         relative_abundance), file = log_con)
              
            }
            
            if (relative_abundance > minimum_ratio) {
              
              cat(paste0(" which is OK!"), file = log_con)
              
              if (line2 < line) {
                
                statistics_table$parent_id[line] <-
                  
                  statistics_table[row.names(otutable)[line2],"parent_id"]
                
                cat(paste0("\n", "SETTING ",
                           
                           potential_parent_id, " to be an ERROR of ",
                           
                           (statistics_table[row.names(otutable)[line2],
                                             
                                             "parent_id"]), "\n"),
                    
                    file = log_con)
                
              } else {
                
                statistics_table$parent_id[line] <- row.names(otutable)[line2]
                
                cat(paste0("\n", "SETTING ", potential_parent_id,
                           
                           " to be an ERROR of ", (row.names(otutable)[line2]),
                           
                           "\n"), file = log_con)
                
              }
              
              success <- TRUE
              
            }
            
          }
          
        }
        
      }
      
    }
    
    if (!success) {
      
      statistics_table$parent_id[line] <- row.names(statistics_table)[line]
      
      cat(paste0("\n", "No parent found!", "\n"), file = log_con)
      
    }
    
  }
  
  
  
  close(log_con)
  
  total_abundances <- rowSums(otutable)
  
  curation_table <- cbind(nOTUid = statistics_table$parent_id, otutable)
  
  statistics_table$curated <- "merged"
  
  curate_index <- row.names(statistics_table) == statistics_table$parent_id
  
  statistics_table$curated[curate_index] <- "parent"
  
  statistics_table <- transform(statistics_table,
                                
                                rank = ave(total,FUN = function(x)
                                  
                                  rank(-x, ties.method = "first")))
  
  curation_table <- as.data.frame(curation_table %>%
                                    
                                    group_by(nOTUid) %>%
                                    
                                    summarise_all(funs(sum)))
  
  row.names(curation_table) <- as.character(curation_table$nOTUid)
  
  curation_table <- curation_table[, -1]
  
  curated_otus <- names(table(statistics_table$parent_id))
  
  curated_count <- length(curated_otus)
  
  discarded_otus <- setdiff(row.names(statistics_table), curated_otus)
  
  discarded_count <- length(discarded_otus)
  
  end.time <- Sys.time()
  
  time.taken <- end.time - start.time
  
  result <- list(curated_table = curation_table,
                 
                 curated_count = curated_count,
                 
                 curated_otus = curated_otus,
                 
                 discarded_count = discarded_count,
                 
                 discarded_otus = discarded_otus,
                 
                 runtime = time.taken,
                 
                 minimum_match = minimum_match,
                 
                 minimum_relative_cooccurence = minimum_relative_cooccurence,
                 
                 otu_map = statistics_table,
                 
                 original_table = otutable)
  
  
  
  return(result)
  
}
