library(tercen)
library(dplyr)
library(peakPick)
library(NbClust)
 
source('optics11.R')

ctx = tercenCtx() 

if (!(ctx$hasXAxis)) stop('An x axis is required')
if (length(ctx$labels) != 1) stop('One label is required')

data = ctx$select(unlist(list('.ri', '.ci', '.y', '.x', ctx$labels)))
  
data %>%
  group_by(.ri,.ci) %>%
  do(do.optics11(., ctx$labels)) %>% 
  select_(.dots='-cluster')  %>%
  ctx$addNamespace() %>% ctx$save()


