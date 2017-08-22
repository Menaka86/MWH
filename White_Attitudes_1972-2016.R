
##########################
##      LOAD DATA       ##
##                      ##
##########################

allData <- data.table(read_excel(path = "J:/Personal/MWH/WhiteAttitudes1972_2016.xlsx",
                                 sheet = 4,
                                 col_names = T))
questionsReadable <- fread(input = "J:/Personal/MWH/White_Attitudes_1972-2016_GSSCodes.csv",
                           stringsAsFactors = F,
                           data.table = T)

##########################
##      CLEAN DATA      ##
##                      ##
##########################

allData[`Question Code`=="Liveblk", `Question Code` := "Liveblks"]

# allData[, .N, by = .(Category, Question, Opinion)]
# allData[Category=="Welfare", .N, by = .(`Question Code`, Question, Opinion)]

#########################################################
##      COLLAPSE RESPONSES TO NORMALIZE OPINIONS       ##
##                                                     ##
#########################################################

## Create a Show binary for later use in dropping records
allData[, Show := 0]

## Keep only cases in which respondents expressed negative racial views

## EDUCATION
allData[Category=="Education" & Opinion=="YES", Show := 1]

## EMPLOYMENT
empCollapseAff <- c("STRONGLY OPPOSE PREF", "OPPOSE PREF")
allData[Category=="Employment" & `Question Code`=="Affrmact" & Opinion%in%empCollapseAff,
        Show := 1]
##
allData[Category=="Employment" & `Question Code`=="Racdif1" & Opinion=="NO",
        Show := 1]
allData[Category=="Employment" & `Question Code`=="Racdif2" & Opinion=="YES",
        Show := 1]
allData[Category=="Employment" & `Question Code`=="Racdif3" & Opinion=="NO",
        Show := 1]
allData[Category=="Employment" & `Question Code`=="Racdif4" & Opinion=="YES",
        Show := 1]

## HOUSING
allData[Category=="Housing" & `Question Code`=="Racopen" & Opinion=="OWNER DECIDES",
        Show := 1]
##
housingCollapseSeg <- c("AGREE STRONGLY", "AGREE SLIGHTLY")
allData[Category=="Housing" & `Question Code`=="Racseg" & Opinion%in%housingCollapseSeg,
        Show := 1]
##
housingCollapseLiveBlks <- c("STRONGLY OPPOSE", "OPPOSE")
allData[Category=="Housing" & `Question Code`=="Liveblks" & Opinion%in%housingCollapseLiveBlks,
        Show := 1]
##
allData <- allData[-allData[, .I[Category=="Housing" & `Question Code`=="Livewhts"]]]

## POLICE RELATIONS
allData[Category=="Police Relations" & `Question Code`=="Polhitok" & Opinion=="YES",
        Show := 1]
allData[Category=="Police Relations" & `Question Code`=="Polabuse" & Opinion=="YES",
        Show := 1]
allData[Category=="Police Relations" & `Question Code`=="Polmurdr" & Opinion=="YES",
        Show := 1]
##
policeCollapse <- c("SPEND MUCH MORE", "SPEND MORE")
allData[Category=="Police Relations" & `Question Code`=="Sppolice" & Opinion%in%policeCollapse,
        Show := 1]


## WELFARE
allData[Category=="Welfare" & `Question Code`=="Natfare" & Opinion=="TOO MUCH",
        Show := 1]
allData[Category=="Welfare" & `Question Code`=="Natrace" & Opinion=="TOO MUCH",
        Show := 1]
##
welfareCollapseHelpBlk <- c("4", "NO SPECIAL TREATMENT")
allData[Category=="Welfare" & `Question Code`=="Helpblk" & Opinion%in%welfareCollapseHelpBlk,
        Show := 1]
##
welfareCollapseWrkWayUp <- c("AGREE STRONGLY", "AGREE SOMEWHAT")
allData[Category=="Welfare" & `Question Code`=="Wrkwayup" & Opinion%in%welfareCollapseWrkWayUp,
        Show := 1]

allData <- allData[j=list(Count = sum(Count)),
                   by = .(Year, Category, `Question Code`, Question, Show)]

allData[, `Mean Response Per Question` := sum(Count*Show)/sum(Count),
        by = .(`Question Code`)]

allData <- allData[, .(`Percent Response` = sum(Count*Show)/sum(Count)),
                   by = .(Year, Category, `Mean Response Per Question`, `Question Code`, Question)]

questionLookup <- allData[, .(`Question Code` = unique(`Question Code`),
                              `Question Label` = paste0("Question ", seq(unique(`Question Code`)))),
                          by = Category][, .(`Question Code`, `Question Label`)]
setkey(questionLookup, `Question Code`)
setkey(questionsReadable, `Question Code`)
questionLookup <- questionsReadable[questionLookup]

setkey(allData, `Question Code`)
allData <- questionLookup[allData]

write.csv(x = allData[, .(`Question Code`, Category, `Question Label`,
                          `Question Readable`, Year, `Percent Response`,
                          `Mean Response Per Question`)],
          file = "J:/Personal/MWH/White_Attitudes_1972-2016_clean.csv",
          row.names = F)


