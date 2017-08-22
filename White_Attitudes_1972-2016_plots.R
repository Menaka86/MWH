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
yearsByCategory <- allData[, .(Year = seq(min(Year), max(Year))),
                           by = .(Category)]

questionsByCategory <- allData[, .(`Question Code` = unique(`Question Code`),
                                   `Mean Response Per Question` = unique(`Mean Response Per Question`)),
                               by = .(Category)]

supplement <- merge(yearsByCategory, questionsByCategory, by = "Category",
                    all = T, allow.cartesian = T)

setkey(supplement, Category, Year, `Question Code`)
setkey(allData, Category, Year, `Question Code`)
allData <- allData[, .SD, .SDcols = c("Category", "Year", "Question Code", "Percent Response")][supplement]

questionColors <- data.table(Category = unique(allData$Category),
                             `Question Color` = sample(rainbow(5, .65, .75, alpha = .7)),
                             key = "Category")
setkey(allData, Category)
allData <- questionColors[allData]
# stop()
allPlots <- sapply(unique(allData$Category), function(x){

        thisCategory <- allData[Category==x]

        questionPlots <- sapply(unique(thisCategory$`Question Code`), function(y){

                thisQuestion <- thisCategory[`Question Code`==y]
                thisQuestion <- thisQuestion[is.na(`Percent Response`), `Percent Response` := 0]

                plot <- ggplot() +

                        geom_bar(data = thisQuestion,
                                 mapping = aes(x = Year, y = `Percent Response`),
                                 fill = thisQuestion$`Question Color`,
                                 stat = "identity") +

#                         geom_smooth(data = thisQuestion,
#                                     mapping = aes(x = Year,
#                                                   y = `Percent Response`),
#                                     method = "lm") +
                        geom_hline(aes(color = "grey10"),
                                   yintercept = thisQuestion$`Mean Response Per Question`) +
                        # geom_abline(aes(color = "grey10"), slope = lbf[2],intercept = lbf[1]) +

                        scale_y_continuous(name = "",
                                           breaks = seq(0, 1, .2),
                                           labels = percent(seq(0, 1, .2)),
                                           limits = c(0, 1)) +
                        scale_x_continuous(name = "",
                                           breaks = seq(min(thisQuestion$Year),
                                                        max(thisQuestion$Year)),
                                           label = seq(min(thisQuestion$Year),
                                                       max(thisQuestion$Year)),
                                           limits = c(min(thisQuestion$Year),
                                                      max(thisQuestion$Year))) +

                        theme(axis.text.x = element_text(angle = 270,
                                                         hjust = 1,
                                                         vjust = .5,
                                                         margin = margin(-15,0,0,0)),
                              axis.ticks.x = element_line(linetype = 0),
                              axis.text.y = element_text(angle = 0, hjust = 1),
                              panel.background = element_rect(fill = NA),
                              panel.grid.major.y = element_line(color = "grey10", linetype = 3),
                              panel.margin.x = unit(0, "npc"),
                              legend.position = "none",
                              text = element_text(family = "Times New Roman")) +

                        geom_text(data = thisQuestion,
                                  aes(x = min(thisQuestion$Year)+4,
                                      y = thisQuestion$`Mean Response Per Question`,
                                      label = paste0("mean = ",
                                                     percent(thisQuestion$`Mean Response Per Question`)),
                                      vjust = -.25,
                                      fontface = "italic"))

                return(plot)
        }, simplify = F, USE.NAMES = T)

}, simplify = F, USE.NAMES = T)


stop()

sapply(names(allPlots), function(x){
        sapply(unique(allData[Category==x]$`Question Code`), function(y){
                pdf(file = paste0("White_Attitudes_1972-2016_", x, "-", y, "_plot.pdf"),
                    family = "Times New Roman",
                    title = "White Attitudes 1972-2016",
                    pointsize = 8)
                plot(allPlots[[x]][[y]])
                dev.off()
        })
})


