### generalized PCA function

## only proper data.frame and two vectors with column names of meta data and measure variables needed
## optional argument drop.na takes care for removal of NA values before the PCA analysis
## two parameters can be defined as fill (hull/points) and shapes (points) aesthetics
## output plots can be extended with further ggplot layers

library(tidyverse)
library(ggrepel)
source("stat_chull.R")

data.structure <- function(.data){
    is.param.numeric <-
        .data %>%
        map_lgl(is.numeric)
    
    constant <- 
        .data %>%
        apply(MARGIN = 2, FUN = function(x) all(duplicated(x)[-1L]))
    
    return(list(is.param.numeric = is.param.numeric,
                constants = constant))
}

data.quality <- function(.data,
                         #.id.vars,
                         .measure.vars){

    data.structure <- data.structure(.data)
    data.types <-  data.structure$is.param.numeric
    constants  <- data.structure$constants
    
    #### remove constant values (which make no sence in PCA ?!)
    constant.parameters <- names(constants[constants])
    nonconstant.parameters <- names(constants[!constants])
    
    data <- subset(.data, select = nonconstant.parameters)
    
    if (length(constant.parameters > 0)) {
    message("Parameter(s) [", paste(constant.parameters, collapse = ", "), "] ",
            " constant and therefore removed.\n")
    } 
    
    measure.vars <- if (missing(.measure.vars)) {
        intersect(names(data.types[data.types]), nonconstant.parameters)
    }else .measure.vars

    # id.vars <- if (missing(.id.vars)) {
    #     intersect(names(data.types[!data.types]), nonconstant.parameters)
    # }else .id.vars
                  
    missings.per.param <-
        data %>%
        is.na() %>%
        colSums()
    
    ### split data.frame in metadata and ...
    # meta <- data %>%
    #     as.data.frame() %>%
    #     select(id.vars)
    
    #### ... numerics for analysis
    values <- data %>%
        as.data.frame() %>%
        select(measure.vars)
    
    
    ### check data integrity:
    #### missing values:
    # missings.table <- values %>%
    #     is.na() %>% 
    #     rowSums() %>%
    #     data.frame(n.na = ., values) %>%
    #     merge(data, by = 0) %>%
    #     filter(n.na > 0)
    
    return(list(data = data,
                measure.vars = measure.vars,
                #id.vars = id.vars,
                missings.per.parameter = missings.per.param,
                constant.parameters = constant.parameters,
               # missings.table = missings.table,
               # meta = meta,
                values = values))
}


pca <- function(.data, 
               # .id.vars,
                .measure.vars,
                scale = TRUE, center = TRUE){
    
    quality.data <- data.quality(.data, .measure.vars)
    data <- quality.data$data
    #meta <- quality.data$meta
    values <- quality.data$values
    
    ## missing data
   # missings.table <- quality.data$missings.table
    missings.num.per.parameter <- quality.data$missings.per.parameter
    if (sum(missings.num.per.parameter) > 0) {
        message(sum(!complete.cases(values)),
                " rows were discarded because of missing values.\n")
        discarded <- data[!complete.cases(values), ]
        values <- na.omit(values)
    }
    
    #### sample size vs number of parameters
    n <- nrow(values)
    p <- ncol(values)
    
    n.soll <- (p^2 + p) / 2 # TODO: find reference!!
    if (n > n.soll) {
        message(paste("=)", n, ">", n.soll, "\n"))}else{
            warning(paste("=( WARNING: number of observations might be too low. ",
                          n, "$\\leq$", n.soll, "Try again with less parameters.\n"))}
    
    ### PCA analysis:
    pca.results <- prcomp(values, scale. = scale, center = center)
    
    loadings <- pca.results$rotation %>%
        data.frame(labels = row.names(.))
    
    scores <- pca.results$x %>%
        scale() %>% # what is this good for?
        merge(data, by = 0)
    
    eval.summary <- summary(pca.results)
    proportions <- as.data.frame(t(unclass(eval.summary$importance))) %>%
        mutate(PC = factor(rownames(.), levels = paste0("PC", 1:100), ordered = TRUE))
    
    return(list(pca.results = pca.results,
                #discarded.rows = discarded,
                summary = eval.summary,
                proportions = proportions,
                loadings = loadings,
                scores = scores))
}


pca.plot <- function(scores,
                     loadings,
                     arrow.scale = 4, # TODO: make this adjust automatically and remove this option
                     fill.group = NULL,
                     #shape.group = NULL,
                     pointsize = 2){
    fill.group  <- ifelse(is.null(fill.group), "Row.names", fill.group)
    #shape.group <- ifelse(is.null(shape.group), quality.data$id.vars[1], shape.group)
    
    scores %>%
        ggplot(aes(x = PC1, y = PC2)) + 
        geom_hline(yintercept = 0, colour = "grey") +
        geom_vline(xintercept = 0, colour = "grey") +
        stat_chull(aes_string(fill = fill.group), alpha = .5,
                   colour = "darkgrey", lwd = .2) +
        geom_point(aes_string(#shape = shape.group,
            fill = fill.group),
            size = pointsize, stroke = .8, color = "black", shape = 21) +
        #scale_shape_manual(values = c(21:25, 0:14, 16:20)) +
        theme_bw() +
        geom_segment(data = loadings, aes(x = 0, y = 0,
                                          xend = PC1 * arrow.scale,
                                          yend = PC2 * arrow.scale),
                     colour = "black", size = .3,
                     arrow = arrow(type = "open", angle = 23, length = unit(.5, "line"))) +
        geom_label_repel(data = loadings,
                         mapping = aes(x = PC1 * arrow.scale,
                                       y = PC2 * arrow.scale,
                                       label = labels),
                         colour = "black", alpha = .5, size = 2.5, nudge_x = .2) +
        coord_fixed(ratio = 1) +
        theme(axis.text = element_blank(), axis.ticks = element_blank())
}



loadings.plot <- function(loadings){
    
    loadings %>%
        select(labels, PC1, PC2) %>%
        mutate(sumPC = abs(PC1) + abs(PC2)) %>%
        arrange(sumPC) %>%
        mutate(labels = factor(labels, levels = labels, ordered = TRUE)) %>%
        gather(variable, value, PC1:PC2) %>%
        ggplot() +
        aes(x = labels, y = value, fill = variable) +
        geom_hline(yintercept = 0) +
        geom_col(position = "identity", alpha = .5) +
        #facet_wrap(~variable) +
        theme_minimal() +
        xlab("") +
        ylab("Parameter loadings") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
}



proportions.plot <- function(proportions){
    
    proportions %>%
        slice(1:10) %>%
        ggplot(aes(x = PC, y = `Proportion of Variance`)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = round(`Proportion of Variance`, digits = 2)),
                  vjust = -0.2, size = 3) +
        geom_label(aes(y = 1,
                       label = paste(100 * round(`Cumulative Proportion`,
                                                 digits = 2), "%")),
                   vjust = 1, size = 2.5) +
        expand_limits(y = 1) +
        theme_bw() +
        xlab("Principal component")
    
}
