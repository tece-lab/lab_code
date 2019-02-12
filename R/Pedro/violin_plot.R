ontogeny <- factor(c(
  rep("island_ontogeny", length(removed_failures_ont$mu)),
  rep("no_ontogeny", length(removed_failures_no_ont$mu))), 
  levels = c("no_ontogeny", "island_ontogeny"), ordered = TRUE)

View(mus_df)
mus_df <- data.frame(
  mu = c(removed_failures_ont$mu, removed_failures_no_ont$mu),
  ontogeny = ontogeny
)


hlines_df <- data.frame(ontogeny = c("island_ontogeny", "no_ontogeny"), hline = c(log(sumstats$mean_ext), log(1.231847/2)))

ggplot(data = mus_df, aes(x = ontogeny, y = log(mu), fill = ontogeny)) + 
  geom_violin(trim = FALSE) +
  scale_fill_manual(values = c("darkgreen", "red4")) +
  ggtitle("DAISIE extintion rate estimates in ontogeny \nand null-ontogeny scenarios")  +
  ylab("Extinction rate estimates (log transformed)") +
  xlab(element_blank()) +
  theme(
    axis.ticks.x = element_blank(),
    legend.position = "none",
    axis.line = element_line("black"),
    panel.background = element_blank()
    ) +
  scale_x_discrete(labels=c("island_ontogeny" = "Island ontogeny", "no_ontogeny" = "Null-ontogeny")) +
  geom_errorbar( # Add horizontal bars in set postion (specify in hlines_df)
    data = hlines_df,
    aes(y = NULL, ymax = hline, ymin = hline),
    color = "orange2", size = 1.1
    )

# Export to pdf
export::graph2ppt(height = 4, width = 4, file = "violin_plot_new2.ppt")
