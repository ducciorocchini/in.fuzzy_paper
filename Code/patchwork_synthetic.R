# Tis script can be run after the scripts synthetic_size.R and synthetic_bands.R have been run. 
# It will join the two resulting plots in a single image.

library(patchwork)

# Output from synthetic_size.R
p_runtime_size

# Output from synthetic_bands.R
p_runtime_bands

p_runtime_size + p_runtime_bands 
# + plot_annotation(tag_levels = "A)") 
# plot_annotation() can be used to put labels A) and B) to plots, but in this case it might be misleading due to the number of bands called B
 
