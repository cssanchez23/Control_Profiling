# Create Outputs directory
dir.create("Outputs", showWarnings = FALSE)

# Remove parts of string from sample names in OTU table
colnames(otu_table_controls) <- gsub("CBINTERNAL.", "", colnames(otu_table_controls))

# Calculate the relative abundance (not rarefied)
otu_table_controls2 <- otu_table_controls %>%
  column_to_rownames("X.OTU.ID")

otu_table_controls_rel <- as.data.frame(apply(otu_table_controls2, 2, function(x) x/sum(x)))

# Calculate sum relative abundance for all taxa in the OTU controls table
otu_table_controls_top20 <- otu_table_controls_rel %>%
  dplyr::mutate(Sum = rowSums(.)) %>%
  arrange(desc(Sum)) %>%
  rownames_to_column("Taxon") %>%
  slice(1:20) %>%
  rename("EXT.POS.MSA2002.PlatE.0018.2.B12"="EXT.MSA2002.PlatE.0018.2.B12") %>%
  column_to_rownames("Taxon")

# Use only the Genus_Species identifiers for taxa names
# otu_table_controls_top20 <- otu_table_controls_top20 %>%
#   separate(Taxon, into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ";") %>%
#   dplyr::select(-c(Domain, Phylum, Class, Order, Family, Genus)) %>%
#   column_to_rownames("Species")

# rownames(otu_table_controls_top20) <- gsub("s__", "", rownames(otu_table_controls_top20))
# rownames(otu_table_controls_top20) <- gsub("_", " ", rownames(otu_table_controls_top20))

# Transpose table and calculate an Other category for anything not in the Top 20
otu_table_controls_top20$Sum = NULL
otu_table_controls_top20 <- as.data.frame(t(otu_table_controls_top20))

otu_table_controls_top20 <- otu_table_controls_top20 %>%
  rownames_to_column("SampleID") %>%
  mutate(Sum = rowSums(.[2:21])) %>%
  mutate(Other = 1 - Sum) %>%
  column_to_rownames("SampleID")

otu_table_controls_top20$Sum = NULL

# Create metadata categories to add into this table for plotting purposes
otu_table_controls_top20$Control_Type <- ifelse(grepl("BM.POS", rownames(otu_table_controls_top20)), "Benchmark Positive",
                                                ifelse(grepl("BM.NEG", rownames(otu_table_controls_top20)), "Benchmark Negative",
                                                       ifelse(grepl("EXT.POS", rownames(otu_table_controls_top20)), "Extraction Positive", "Extraction Negative")))

otu_table_controls_top20$Control_Type <- factor(otu_table_controls_top20$Control_Type,
                                                levels = c("Benchmark Positive", "Benchmark Negative",
                                                           "Extraction Positive", "Extraction Negative"))

otu_table_controls_top20 <- otu_table_controls_top20 %>%
  rownames_to_column("SampleID")

# Melt dataframe for plotting purposes
otu_table_controls_top20_melt <- melt(otu_table_controls_top20, id.vars = c("SampleID", "Control_Type"))

# Plot taxa stacked barplots
stacked_barplot <- ggplot(otu_table_controls_top20_melt, aes(x = SampleID,
                                                             y = value,
                                                             fill = variable)) +
  geom_bar(position = "stack", stat = "identity") +
  theme_cowplot(12) +
  scale_fill_manual(values = colors(21)) +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        strip.background = element_rect(colour = "black", fill = "#FFFFFF")) +
  ylab("Relative Abundance") +
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  guides(fill = guide_legend(title = "Taxon", nrow = 21, byrow = TRUE)) +
  facet_wrap(. ~ Control_Type, scales = "free_x", nrow = 1)

pdf("Outputs/Taxa Stacked Bar Plots for Controls_CCF Project.pdf", height = 9, width = 15)
print(stacked_barplot)
dev.off()
