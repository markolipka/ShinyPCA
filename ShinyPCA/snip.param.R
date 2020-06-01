
snip.param <- function(df, param){
    
    # store position in the data.frame of selected parameter
    param.pos <- which(names(df) == param)
    
    # generate a data.frame containing TRUE / FALSE for non-missing / missing respectively
    not.nas <- df %>% is.na() %>% `!` %>% as.data.frame()
    
    # count number of values lost ...
    loss   <- sum(not.nas[, param.pos]) # sum of TRUE values is number of non-missings
    # and gained due to trimming of the parameter
    profit <- sum(
        as.matrix(
            not.nas[!not.nas[ ,param.pos] & # where param column is FALSE
                        complete.cases(not.nas[,-param.pos]), ]))# and all others are TRUE !
    
  #  message(loss, " values lost due to trimming of '", param, "';\n",
   #         profit, " values rescued from oncoming 'na.omit()'.\n(diff = ",
    #        profit - loss ,")")
    
    trimmed.df <- df[ , -param.pos]
    return(list(trimmed.df = trimmed.df,
                balance = data.frame(loss = loss,
                                     profit = profit,
                                     net = profit - loss)))
}

### walk through names(testdf) in random order and record snip.param balance

# counter <- 0
# while (TRUE) {
#     counter <- counter + 1
#     df <- testdf
#     res <- data.frame(NULL)
#     for (i in sample(names(testdf))[-1]) {
#         lala <- snip.param(df, i)
#         res <- rbind(res, cbind(counter, i, lala$balance))
#         df <- lala$trimmed.df
#     }
#     write.table(res, file = "krassertest.csv", 
#                 sep = ",", dec = ".",
#                 append = TRUE,
#                 col.names = F)
# }
# 
# 
# 
# krass <- read.csv("krassertest.csv", header = FALSE, 
#                   col.names = c("pos", "run", "param",
#                                 "loss", "profit", "net"))
# 
# krass %>%
#     ggplot(aes(x = param, y = profit, group)) +
#     geom_boxplot() +
#     coord_flip()
