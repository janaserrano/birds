#####Author: Andrea Brown
### IUCN status POST TVA analysis scatter plot - Fig. 2B ============================================
# Calculate average tva.score for VU species ONLY
vu <- TVA %>%
  group_by(IUCN_Status.pre) %>%
  summarise_at(vars(tva.score), list(avg = mean), na.rm = T)
vu <- subset(vu, IUCN_Status.pre == "VU")
vu.mean <- vu %>%
  select(-IUCN_Status.pre)
rm(vu)
# Remove rows with NA for SDM predictions since their TVA score will not be accurate
TVA <- TVA[complete.cases(TVA$Net_lost),] # use Net_lost as SDM index; end up with 336 species
# If original TVA score is greater than average value for VU species, then it becomes VU
TVA$TVA_status.post <- ifelse(TVA$tva.score > vu.mean$avg, "VU", "LC")
# How many species go from LC to VU
sum(TVA$IUCN_Status.pre == "LC" & TVA$TVA_status.post == "VU") # 72 = overlooked species
rm(vu.mean)
# plot data wrangling - highlight 72 species that are listed as LC but should be VU
TVA$ShouldB_VU <- ifelse(TVA$IUCN_Status.pre == "LC" & TVA$TVA_status.post == "VU", "VU", "LC")
# Plot
ggplot(TVA,
       aes(y = Exposure, x = Sens,
           shape = IUCN_Status.pre,
           fill = Adapt,
           color = ShouldB_VU)) +
  # add points
  geom_point(size=3, stroke=1.5) +
  # manually set shape according to value
  scale_shape_manual(values=c(LC = 21, VU = 24)) +
  # set color (outline of points) to red or transparent
  scale_color_manual(values = c(VU = "firebrick2", LC = "transparent"),
                     guide = "none")+
  # set fill (inside of points) to color scale
  scale_fill_gradient(low = "deepskyblue1", high = "#E69F00") +
  theme_minimal()+
  labs(fill = "Adaptive Capacity",
       shape = "IUCN Status",
       title = "B. Average TVA Trait Index", x = "Sensitivity")+
  #subtitle = "Blue = High adaptive Capacity; Orange = Low adaptive capacity") +
  theme_classic()+
  xlim(0,1.7) +
  ylim(0,1.7)