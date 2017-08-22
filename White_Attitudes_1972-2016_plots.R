allData <- fread(input = "J:/Personal/MWH/White_Attitudes_1972-2016_clean.csv",
                 data.table = T, stringsAsFactors = F)

# allData[, `Question Label` := paste0(substr(`Question Label`, 10, 12), ". ",
#                                      `Question Readable`)]
# allData[Category%in%c("Education", "Housing")&grepl("^2", `Question Label`), questionLevel := 3]
# allData[Category%in%c("Education", "Housing")&grepl("^3", `Question Label`), questionLevel := 2]
# allData[Category%in%c("Education", "Housing")&grepl("^1", `Question Label`), questionLevel := 1]
# allData[Category=="Employment"&grepl("^4", `Question Label`), questionLevel := 5]
# allData[Category=="Employment"&grepl("^5", `Question Label`), questionLevel := 4]
# allData[Category=="Employment"&grepl("^2", `Question Label`), questionLevel := 3]
# allData[Category=="Employment"&grepl("^3", `Question Label`), questionLevel := 2]
# allData[Category=="Employment"&grepl("^1", `Question Label`), questionLevel := 1]
# allData[Category%in%c("Police Relations", "Welfare")&grepl("^3", `Question Label`), questionLevel := 4]
# allData[Category%in%c("Police Relations", "Welfare")&grepl("^4", `Question Label`), questionLevel := 3]
# allData[Category%in%c("Police Relations", "Welfare")&grepl("^1", `Question Label`), questionLevel := 2]
# allData[Category%in%c("Police Relations", "Welfare")&grepl("^2", `Question Label`), questionLevel := 1]
# setorder(allData, Category, -questionLevel)
#
# allData[, `Question Facet` := factor(x = `Question Label`,
#                                      levels = unique(`Question Label`),
#                                      ordered = T)]
# stop()

# font_import()
# loadfonts(device="win")       #Register fonts for Windows bitmap output
# fonts()
yearsByCategory <- allData[, .(Year = seq(min(Year), max(Year))), by = .(Category)]
questionsByCategory <- allData[, .(`Question Code` = unique(`Question Code`)), by = .(Category)]
supplement <- merge(yearsByCategory, questionsByCategory, by = "Category",
                    all = T, allow.cartesian = T)

setkey(supplement)
setkey(allData, Category, Year, `Question Code`)
allData <- allData[supplement]

questionColors <- data.table(Category = unique(allData$Category),
                             `Question Color` = sample(rainbow(5, .65, .75, alpha = .7)),
                             key = "Category")
setkey(allData, Category)
allData <- questionColors[allData]

allPlots <- sapply(unique(allData$Category), function(x){

        thisCategory <- allData[Category==x]

        questionPlots <- sapply(unique(thisCategory$`Question Code`), function(y){

                thisQuestion <- thisCategory[`Question Code`==y]

                plot <- ggplot(data = thisQuestion) +
                        aes(x = factor(thisQuestion$Year), y = thisQuestion$`Percent Response`) +
                        geom_bar(fill = thisQuestion$`Question Color`,
                                 stat = "identity") +

                        scale_y_continuous(name = "",
                                           breaks = seq(0, 1, .2),
                                           labels = percent(seq(0, 1, .2)),
                                           limits = c(0, 1)) +

                        geom_hline(aes(color = "grey10"),
                                   yintercept = thisQuestion$`Mean Response Per Question`) +

                        theme(axis.text.x = element_text(angle = 270,
                                                         hjust = 1,
                                                         vjust = .5,
                                                         margin = margin(-12,0,0,0)),
                              axis.ticks.x = element_line(linetype = 0),
                              axis.text.y = element_text(angle = 0, hjust = 1),
                              panel.background = element_rect(fill = NA),
                              panel.grid.major.y = element_line(color = "grey10", linetype = 3),
                              panel.margin.x = unit(0, "npc"),
                              legend.position = "none",
                              text = element_text(family = "Times New Roman")) +

                        geom_text(aes(x = 4,
                                      y = thisQuestion$`Mean Response Per Question`,
                                      label = paste0("mean = ",
                                                     percent(thisQuestion$`Mean Response Per Question`)),
                                      vjust = -.25,
                                      fontface = "italic"))

                return(plot)
        }, simplify = F, USE.NAMES = T)

}, simplify = F, USE.NAMES = T)

stop()

for(j in unique(allData$Category)){
        pdf(file = paste0("White_Attitudes_1972-2016_", j, "_plots.pdf"),
            onefile = T, family = "Times New Roman", title = "White Attitudes 1972-2016", pointsize = 8)
        for(i in unique(allData$`Question Code`)){
                plot(allPlots[[i]])
        }
        dev.off()
}
