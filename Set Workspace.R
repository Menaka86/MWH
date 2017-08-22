rm(list = ls())

if(!"data.table"%in%installed.packages()) install.packages("data.table")
if(!"readxl"%in%installed.packages()) install.packages("readxl")
if(!"ggplot2"%in%installed.packages()) install.packages("ggplot2")
if(!"scales"%in%installed.packages()) install.packages("scales")
if(!"extrafont"%in%installed.packages()) install.packages("extrafont")

library(data.table)
library(readxl)
library(ggplot2)
library(scales)
library(extrafont)
