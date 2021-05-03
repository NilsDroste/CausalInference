# refs: http://personality-project.org/r/basics.t.html
# https://www.r-bloggers.com/add-p-values-and-significance-levels-to-ggplots/

# load packages
require("ggpubr")

# read in data
caffeine <- readr::read_csv("./data/caffeine.csv")

# plot the data
p <- ggboxplot(caffeine, x = "Treatment", y = "SpellingPerformance",
               color = "Treatment", palette = "lancet",
               add = "jitter", title = "Effect of Caffeine on performance in a spelling test")
#  Add p-value
p + stat_compare_means(method = "t.test") + rremove("legend")

# computing the t test directly
t.test(SpellingPerformance~Treatment, data = caffeine)
# see https://statistics.berkeley.edu/computing/r-t-tests

# another way of plotting: a density plot (plotting a probability density function)
ggdensity(caffeine, x = "SpellingPerformance",
            add = "mean", rug = TRUE,
            color = "Treatment", fill = "Treatment",
            palette = "lancet")
