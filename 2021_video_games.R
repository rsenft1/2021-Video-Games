library(here)
library(ggplot2)
library(dplyr)
library(pals)
library(grid)
library(gridExtra)
library(ggalt)
library(stringr)
library(cowplot)
library(ggthemes)

# read in data
df = read.delim(
  here("2021 Video Games.csv"),
  header = TRUE,
  stringsAsFactors = TRUE,
  quote = "",
  sep = ",",
  na = "NA"
)

df$Month.started <- factor(df$Month.started, levels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
noNA_meta_df <- df[!is.na(df$Metacritic.score),]
df <- df %>% mutate(Retail.Price..std.edition. = Retail.Price..std.edition. %>% str_remove_all("\\$") %>% as.numeric())

# Create a group-means data set
g_df <- df %>% 
  group_by(Genre) %>% 
  summarise(
    Metacritic.score = mean(Metacritic.score, na.rm=TRUE),
    Subjective.rating = mean(Subjective.rating, na.rm=TRUE)
  )
group_no_NA <- g_df[!is.na(g_df$Metacritic.score),]

# COLORS

bg="gray90"
myPalette <- c("#E15759", "#FF9D9A", "#F28E2B", "#FFBE7D", "#F1CE63", "#B6992D", "#8CD17D", "#59A14F", "#86BCB6", "#499894", "#A0CBE8", "#4E79A7", "#B07AA1","#D4A6C8", "#FABFD2", "#D37295", "#9D7660", "#D7B5A6", "#BAB0AC", "#79706E")
#Hard-code the mapping according to metacritic score
myColors <-setNames(myPalette, levels(reorder(df$Genre,df$Metacritic.score)))
sp=4 #spacer

# GRAPHS
A <- ggplot(data = df,aes(x = Month.started, y = Time.To.Beat..hrs., fill = Video.Game.Name)) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = myColors) + 
  theme_classic() + 
  theme(legend.position = "none",
        panel.background = element_rect(fill = bg, color =NA),
        plot.background = element_rect(fill = bg),
        legend.key = element_rect(fill = bg),
        legend.background=element_blank())+
  labs(y = "Estimated play time (in hrs, from HLTB)")

B <-ggplot(data = df, aes(x = Month.started, y = Time.To.Beat..hrs., fill = reorder(Genre, Metacritic.score))) +
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=myColors) +
  theme_linedraw() + 
  theme(panel.background = element_rect(fill = bg, color =NA),
        plot.background = element_rect(fill = bg, color=NA),
        legend.position = "none",
        plot.margin = unit(c(sp,sp,sp,sp), "points")) + 
  labs(title = "Time Played by Genre", x = "Month game was started", y = "Estimated play time (in hrs, from HLTB)")

C <- df %>% 
  group_by(Genre) %>% 
  summarize(count = n(), Metacritic.score = mean(Metacritic.score)) %>%
  ggplot(aes(y = reorder(Genre,(-count)), x = count, fill=reorder(Genre, Metacritic.score))) + 
  geom_bar(stat = 'identity') + 
  theme_linedraw() + 
  theme(panel.background = element_rect(fill = bg, color=NA),
        plot.background = element_rect(fill = bg, color=NA),
        legend.key = element_rect(fill = bg),
        legend.position = "none",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.margin = unit(c(sp,sp,sp,sp), "points")) + 
  scale_fill_manual(values=myPalette) + 
  scale_x_continuous(breaks = seq(0, 10, by = 2)) + 
  labs(
    x = "Number of games",
    y = "Genre",
    title = "Number of Games Played by Genre")


D <- ggplot(df, aes(y=reorder(Genre, -(Subjective.rating)), x=Subjective.rating, color=reorder(Genre, -(Metacritic.score)))) +
  geom_point(alpha=0.3, size=2) +
  geom_point(data = g_df, size=4, alpha=0.8) + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = bg, color=NA),
        plot.background = element_rect(fill = bg, color=NA),
        plot.margin = unit(c(sp,sp,sp,sp), "points")) + 
  scale_color_manual(values=myColors) +
  labs(
    x = "Subjective Rating",
    y = "Genre")

E <- ggplot(g_df, aes(y=reorder(Genre, -(Metacritic.score)), x=Metacritic.score, color=reorder(Genre, -(Metacritic.score)))) +
  geom_point(size=4, alpha=0.8) +
  geom_point(data = df, size=2, alpha=0.3) + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        plot.margin = unit(c(sp,sp,sp,sp), "points")) + 
  scale_fill_manual(values=myColors) +
  labs(x = "Metacritic Score",
    y = "Genre")

FF <- ggplot(group_no_NA, aes(y=reorder(Genre, -(Metacritic.score)), x=10*Subjective.rating, xend=Metacritic.score, color=reorder(Genre, (Metacritic.score)))) +
  geom_dumbbell(size=3, colour_xend ="#0e668b") +
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = bg, color=NA),
        plot.background = element_rect(fill = bg, color=NA),
        plot.margin = unit(c(sp,sp,sp,sp), "points")) + 
  scale_color_manual(values=myColors) +
  labs(x = expression("Subjective Rating" %->% "Metacritic Score (Dark Blue)"),
    y = "Genre")

G <- ggplot(noNA_meta_df, aes(y=reorder(Video.Game.Name, -(10*Subjective.rating-Metacritic.score)), x=10*Subjective.rating-Metacritic.score, fill=reorder(Genre, Metacritic.score))) +
  geom_bar(stat="identity") +
  theme_linedraw() + 
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = bg, color=NA),
        plot.background = element_rect(fill = bg, color=NA),
        legend.key = element_rect(fill = bg),
        legend.background = element_blank(),
        legend.position = "left",
        plot.margin = unit(c(sp,sp,sp,sp), "points")) +
  scale_fill_manual(values=myColors) + 
  labs(x = "Subjective Rating - Metacritic Score",
    y = "Game",
    fill="Genre")

H <- ggplot(df, aes(x = Metacritic.score, y = Retail.Price..std.edition.)) + 
  geom_smooth(
    method = "lm",
    se = TRUE,
    fullrange = FALSE,
    level = 0.95, color = "#E15759")+
  geom_point(size=2) + 
  theme_classic() + 
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = bg, color=NA),
        plot.background = element_rect(fill = bg, color=NA),
        plot.margin = unit(c(sp,sp,sp,sp), "points")) + 
  labs(x = "Metacritic Score",
    y = "Retail Price (USD)")

I <- ggplot(df, aes(x = Subjective.rating, y = Retail.Price..std.edition.)) + 
  geom_smooth(
    method = "lm",
    se = TRUE,
    fullrange = FALSE,
    level = 0.95, color="#E15759")+
  geom_point(size=2) + 
  theme_classic() + 
  theme(axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = bg, color=NA),
        plot.background = element_rect(fill = bg, color=NA),
        plot.margin = unit(c(sp,sp,sp,sp), "points")) + 
  labs(x = "Subjective Rating",
    y = "Retail Price (USD)")

g_df2 <- df %>% 
  group_by(Publisher) %>% 
  summarise(
    Metacritic.score = mean(Metacritic.score, na.rm=TRUE),
    Subjective.rating = mean(Subjective.rating, na.rm=TRUE)
  )

J <- ggplot(df, aes(y=reorder(Publisher, -(Subjective.rating)), x=Subjective.rating, color=reorder(Publisher, -(Subjective.rating)))) +
  geom_point(size=2, alpha=0.3) +
  geom_point(data = g_df2, size=4, alpha=0.8) + 
  theme_classic() + 
  theme(legend.position = "none",
        axis.text.x = element_text(color = "black"),
        axis.text.y = element_text(color = "black"),
        panel.background = element_rect(fill = bg, color=NA),
        plot.background = element_rect(fill = bg, color=NA),
        plot.margin = unit(c(sp,sp,sp,sp), "points")) + 
  scale_color_manual(values=rep(myPalette,2)) + 
  labs(caption="Graph by Rebecca Senft",
    x = "Subjective Rating",
    y = "Publisher", title= "Subjective Publisher Ratings")
  
# Put it all in a grid

grid.rect(gp=gpar(fill=NA))

lay <- rbind(c(1,1,1,1,1,1,1,1),
             c(2,2,2,2,2,2,2,2),
             c(3,3,3,5,5,6,6,8),
             c(3,3,3,5,5,6,6,9),
             c(3,3,3,4,4,7,7,10),
             c(3,3,3,4,4,7,7,10))
title=textGrob("2021: A year in video games",gp=gpar(fontsize=24, fontfamily="HersheySerif"), just="left", x = unit(0.5, "cm"), y = unit(-0.5, "cm"), hjust = 0)
subtitle=textGrob("This year, we played 62 games published in 2021, totaling an estimated 657 hours of play time.",gp=gpar(fontsize=16, fontfamily="Palatino"), x= unit(0.5, "cm"), y = unit(1, "cm"), hjust = 0)
credits <- textGrob("Graphs by \nRebecca Senft\nMade in R\n",
                     gp = gpar(fontsize=16, just = c("center", "bottom"), fontfamily="Palatino"))

plotlist=list(G, FF, B, C,D,H,I,credits)
myPlot <- grid.arrange(grobs = c(list(title), list(subtitle), plotlist), layout_matrix = lay, heights=c(1,5,10,10,10,10,0,0))
png(bg=bg, width = 4000, height = 2000,units = "px", res=200)
  cowplot::ggdraw(myPlot) + 
    theme(plot.background = element_rect(fill=bg, color = NA))
dev.off()
