# Import Data
otu_table_controls <- read.delim(file = "data/taxatable-raw-absolute.tsv", sep = "\t")

# Set color palettes
colors <- colorRampPalette(c("#BC3C29FF", "#0072B5FF", "#E18727FF", "#20854EFF", "#7876B1FF",
                             "#6F99ADFF", "#FFDC91FF", "#EE4C97FF", "#AAAAAA"))
