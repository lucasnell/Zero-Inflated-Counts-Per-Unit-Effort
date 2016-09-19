# Raw R code for 'bootstrapping.Rmd'


## @knitr packages
library('dplyr')
library('readxl')
library('ggplot2')
library('boot')
library('broom')
library('RColorBrewer')



## @knitr inputData
CPUEdf <- read_excel("./from_Bill/brett_example.xlsx",1) %>% 
    rename(Area = `Effort m^2`, Count = SumOfSPEC) %>%
    filter(Year %in% 1993:2013, !is.na(Year), Month == 10, !is.na(Month)) %>%
    select(Year, Month, Station, Count, Area) %>%
    mutate(CPUE = (Count/Area)*100) %>% 
    arrange(Year)




## @knitr bootstrapping
set.seed(9721)
CPUEboot <- CPUEdf %>%
    group_by(Year) %>% 
    bootstrap(100, by_group = TRUE) %>% 
    do(summarize(group_by(., Year), CPUE = mean(CPUE))) %>% 
    ungroup %>% 
    select(-replicate)

# Summary table
CPUEbootSumm <- CPUEboot %>%
    group_by(Year) %>%
    summarize(low = quantile(CPUE, probs = 0.025),
              mid = quantile(CPUE, probs = 0.5),
              high = quantile(CPUE, probs = 0.975))

CPUEbootSumm



## @knitr plotPreamble
# Minimal ggplot2 theme
plotTheme <- function(base_size = 10, base_family = 'Helvetica') {
    theme_minimal(base_size = base_size, base_family = base_family) %+replace%
        theme(
            strip.text = element_text(face = 'bold'),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.major.y = element_line(color = 'gray50', size = 0.125, 
                                              linetype = 3),
            panel.border = element_rect(fill = NA, color = "gray50"),
            axis.ticks = element_line(color = "gray50"),
            axis.ticks.length = unit(2, 'points'),
            legend.position = 'none'
        )
}

# Color palette for unique values in an input vector
myPalette <- function(inputVector){
    outPalette <- inputVector %>% 
        unique %>% 
        length %>% 
        colorRampPalette(brewer.pal(8,  'Dark2'))(.)
    return(outPalette)
}




## @knitr basePlot
bootPlot <- CPUEdf %>%
    ggplot(aes(x = factor(Year), y = CPUE, color = factor(Year))) +
    plotTheme() +
    scale_x_discrete('Year', breaks = seq(min(CPUEdf$Year), max(CPUEdf$Year))) +
    scale_color_manual(values = myPalette(CPUEdf$Year))




## @knitr ggBootCI_fun
ggBootCI <- function(inputData, numSims = 1e3, ciMethod = 'bca'){
    # List names in the `boot.ci` output for these methods differ from their input names
    if (ciMethod %in% c('norm', 'stud', 'perc')){
        outListMethod <- gsub('norm', 'normal', ciMethod) %>%
            gsub('stud', 'student', .) %>%
            gsub('perc', 'percent', .)
    } else {
        outListMethod <- ciMethod
    }
    # Inner function to compute mean and variance of the sampling distribution of the mean
    # for each bootstrap replication
    bootMean <- function(inputData, ind){
        m <- mean(inputData[ind])
        n <- length(ind)
        v <- var(inputData[ind]) / n
        return(c(m, v))
    }
    
    bootstraps <- boot(inputData, statistic = bootMean, R = numSims)
    
    # If all resamples return the same value, output df will simply be that value x3:
    if (diff(range(bootstraps$t)) == 0){
        result <- range(bootstraps$t)[1] %>%
            data.frame(ymin = ., y = ., ymax = .)
    } else { # If not, then run `boot.ci`:
        boot.ciOutput <- boot.ci(bootstraps, type = ciMethod)
        bootCI <- rev(boot.ciOutput[[outListMethod]])[c(2,1)]
        result <- data.frame(ymin = bootCI[1], 
                             y = median(bootstraps$t), 
                             ymax = bootCI[2])
    }
    
    return(result)
}





## @knitr bootPackagePlots
bootPlot +
    scale_y_continuous(expression(Mean ~ Catch ~ 100 ~ m^{-2})) +
    geom_point(position = position_jitter(width = 0.3, height = 0),
               alpha = 0.25, shape = 16) +
    stat_summary(fun.data = ggBootCI,
                 fun.args = list(numSims = 1e3, ciMethod = 'bca'),
                 geom = "errorbar", width = 0.25, size = 0.75) +
    stat_summary(fun.y = "mean", geom = "point", size = 3, shape = 23)





## @knitr WhitePlots
bootPlot +
    scale_y_continuous(expression(Mean ~ Catch ~ 100 ~ m^{-2}), trans = 'log10',
                       breaks = c(0.001, 0.01, 0.1, 1, 10, 100, 1000), 
                       limits = c(0.001, 1000),
                       labels = function(n){format(n, scientific = FALSE, 
                                                   drop0trailing = TRUE)}) +
    geom_errorbar(data = CPUEbootSumm, aes(ymin = low, y = mid, ymax = high, 
                                           x = factor(Year), color = factor(Year)),
                  width = 0.25, size = 0.75) +
    geom_point(data = CPUEbootSumm, aes(y = mid), 
               shape = 23, size = 3)


