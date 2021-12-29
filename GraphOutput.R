all_fronts <- unique(OutputCollect$xDecompAgg$robynPareto)
pf = all_fronts[1]
plotMediaShare <- OutputCollect$xDecompAgg[robynPareto == pf & rn %in% InputCollect$paid_media_vars]
uniqueSol <- plotMediaShare[, unique(solID)]
uniqueSol
sid = uniqueSol[2]

## plot spend x effect share comparison
plotMediaShareLoop <- plotMediaShare[solID == sid]
plotMediaShareLoop
rsq_train_plot <- plotMediaShareLoop[, round(unique(rsq_train), 4)]
nrmse_plot <- plotMediaShareLoop[, round(unique(nrmse), 4)]
decomp_rssd_plot <- plotMediaShareLoop[, round(unique(decomp.rssd), 4)]
mape_lift_plot <- ifelse(!is.null(InputCollect$calibration_input), plotMediaShareLoop[, round(unique(mape), 4)], NA)
suppressWarnings(plotMediaShareLoop <- melt.data.table(plotMediaShareLoop, id.vars = c("rn", "nrmse", "decomp.rssd", "rsq_train"), measure.vars = c("spend_share", "effect_share", "roi_total", "cpa_total")))
plotMediaShareLoop[, rn := factor(rn, levels = sort(InputCollect$paid_media_vars))]
plotMediaShareLoop
plotMediaShareLoopBar <- plotMediaShareLoop[variable %in% c("spend_share", "effect_share")]
plotMediaShareLoopLine <- plotMediaShareLoop[variable == ifelse(InputCollect$dep_var_type == "conversion", "cpa_total", "roi_total")]
line_rm_inf <- !is.infinite(plotMediaShareLoopLine$value)
ySecScale <- max(plotMediaShareLoopLine$value[line_rm_inf]) / max(plotMediaShareLoopBar$value) * 1.1

p1 <- ggplot(plotMediaShareLoopBar, aes(x = rn, y = value, fill = variable)) +
  geom_bar(stat = "identity", width = 0.5, position = "dodge") +
  geom_text(aes(label = paste0(round(value * 100, 2), "%")), color = "darkblue", position = position_dodge(width = 0.5), fontface = "bold") +
  geom_line(data = plotMediaShareLoopLine, aes(x = rn, y = value / ySecScale, group = 1, color = variable), inherit.aes = FALSE) +
  geom_point(data = plotMediaShareLoopLine, aes(x = rn, y = value / ySecScale, group = 1, color = variable), inherit.aes = FALSE, size = 4) +
  geom_text(
    data = plotMediaShareLoopLine, aes(label = round(value, 2), x = rn, y = value / ySecScale, group = 1, color = variable),
    fontface = "bold", inherit.aes = FALSE, hjust = -1, size = 6
  ) +
  scale_y_continuous(sec.axis = sec_axis(~ . * ySecScale)) +
  coord_flip() +
  theme(legend.title = element_blank(), legend.position = c(0.9, 0.2), axis.text.x = element_blank()) +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title = paste0("Share of Spend VS Share of Effect with total ", ifelse(InputCollect$dep_var_type == "conversion", "CPA", "ROI")),
    subtitle = paste0(
      "rsq_train: ", rsq_train_plot,
      ", nrmse = ", nrmse_plot,
      ", decomp.rssd = ", decomp_rssd_plot,
      ", mape.lift = ", mape_lift_plot
    ),
    y = "", x = ""
  )
p1
