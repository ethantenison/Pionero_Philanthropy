

plot <- readRDS("./data/npo_data.rds")
library(ggplot2)

# Precalculate the breaks we'll need for the two histograms

his <- plot[!is.na(plot$budget), ]  
BudgetBreaks <- hist(plot = FALSE, his$budget, breaks = 5)$breaks


        
theme_set(theme_bw())
ggplot(his, aes(x=budget)) +
        geom_histogram(color = "#F2F2F2", fill = "#486F73", bins = 5)+
        theme( panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"),
               text=element_text(family="Times", face = "bold", size=12),
               plot.title = element_text(hjust = 0.5)) +
        xlab("Annual Budget") +
        ylab("Number of Nonprofits") +
        labs(title="Partner Budget's") +
        scale_x_continuous(labels = scales::dollar) 


ggsave("partner_budget_hist.png",width = 14, height =7, dpi = 500, units=c("in"))