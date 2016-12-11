############################################
# Short overview
###########################################

# required packages
library(pacman) 
p_load(readxl, 
       dplyr,                 
       ggplot2, RColorBrewer,
       extrafont)

# importing data
ebay_cartier <- read_excel("E:/Data_science_projects/eBay_data/eBay_auction_cartier.xlsx")

# and transforming into tibble for easiest view
ebay_cartier %>% tbl_df()

# number of auctions
n_distinct(ebay_cartier$auctionid) 

# average final price of an item
mean(ebay_cartier$price) 

# distribution of prices
ggplot(ebay_cartier, 
       aes(x = price)) + 
  geom_histogram(binwidth = 85, 
                 fill = "#008B8B", 
                 col = "white") +
  geom_vline(aes(xintercept = median(ebay_cartier$price)), 
             col = "yellow",
             size = 1.5) +
  labs(title = "Distribution of final prices",
       x = "Final prices",
       y = "Frequency") +
  text_theme

#############################################
# Data analysis: Fake vs Original Cartier set
############################################

# adding new factor variable
ebay_cartier$auction_type_f <- factor(ebay_cartier$auction_type, 
                                      labels = c("3 days", "5 days", "7 days"))

# text theme for every plot in this project
text_theme <- theme(text = element_text(size = 10, 
                                        family = "Verdana", 
                                        face = "italic"),
                    plot.title = element_text(hjust = 0.5))

# distribution of auction types
ggplot(ebay_cartier, 
       aes(x = factor(1), 
           fill = auction_type_f)) +
  geom_bar(width = 3) +
  ggtitle("Auctions' bid distribution per type") +
  coord_polar(theta = "y") +
  labs(fill= "Type of auction") +
  scale_fill_brewer(palette = "PRGn") +
  xlab(NULL) + ylab(NULL) +
  text_theme 

# variables we have
names(ebay_cartier)

# what we need for further analysis
auctions <- ebay_cartier %>%
  select(contains('id'), price, auction_type_f, -bidtime)

# auctions with a bid > median
original <- auctions %>% 
  filter(bid > 620)

# bidders who didn't bid more than the median for an item 
# which was not sold at more than the median price
fake <- auctions %>%
  anti_join(original, by = "auctionid") %>%
  filter(price <= 620)

# check if divided well
n_distinct(original$auctionid); n_distinct(fake$auctionid)

###########################################
# Bidders: I want them all
###########################################

# number of bids per auction for both sets
fake_bids <- fake %>% 
  group_by(auctionid, 
           auction_type_f, 
           openbid) %>%
  summarize(bids_per_auction = n()) %>%
  arrange(desc(bids_per_auction))

original_bids <- original %>% 
  group_by(auctionid, 
           auction_type_f, 
           openbid) %>%
  summarize(bids_per_auction = n()) %>%
  arrange(desc(bids_per_auction))

# few ggplot complements 
bar_auction_type <- geom_bar(aes(fill = auction_type_f), 
                             stat = "count")

fake_palette <- scale_fill_brewer(palette = "Reds") 
original_palette <- scale_fill_brewer(palette = "Blues") 

# distribution of auction types for (likely) fake items
ggplot(fake_bids, 
       aes(x = auction_type_f)) + 
  bar_auction_type +
  fake_palette + 
  labs(title = "Fake Cartier set", 
       x = "Type of an eBay auction", 
       y = "Number of auctions") +
  guides(fill = F) +
  text_theme

# distribution of auction types for (likely) original items ON THE SAME SCALE
ggplot(original_bids,
       aes(x = auction_type_f)) + 
  bar_auction_type + 
  guides(fill = F) +
  original_palette + 
  ggtitle("Original Cartier set") +
  xlab("Type of an eBay auction") + 
  ylab("Number of auctions") +
  text_theme + 
  coord_cartesian(ylim = c(0, 50))

# boxplot complement
boxplot_auction_type <- geom_boxplot(aes(fill = auction_type_f), 
                                     outlier.colour = "grey", 
                                     color = "darkgrey")

# distrubution of bids per auction type: for fake and original sets
ggplot(fake_bids, 
       aes(x = auction_type_f, y = bids_per_auction)) + 
  boxplot_auction_type +
  fake_palette + 
  labs(title = "Fake Cartier set",
       x = "Type of an eBay auction", 
       y = "Number of bids per auction") +
  guides(fill = F) +
  text_theme + 
  coord_cartesian(ylim = c(0, 40))

ggplot(original_bids, 
       aes(x = auction_type_f, y = bids_per_auction)) + 
  boxplot_auction_type +
  original_palette + 
  labs(title = "Original Cartier set", 
       x = "Type of an eBay auction", 
       y = "Number of bids per auction") +
  guides(fill = F) +
  text_theme

# distribution of openbid per "fake" auction
ggplot(fake, 
       aes(x = auction_type_f, y = openbid)) + 
  geom_violin(aes(fill = auction_type_f), 
              col = "white", 
              scale = "area",
              alpha = 0.5) +
  geom_point(col = "aquamarine", alpha = 0.2) +
  fake_palette + 
  labs(title = "Fake: distribution of starting prices",
       x = "Type of an eBay auction", 
       y = "Starting price per auction") +
  guides(fill = F) +
  text_theme +
  coord_cartesian(ylim = c(0, 400)) # few outliers masked

# distribution of openbid per "original" auction
ggplot(original, 
       aes(x = auction_type_f, y = openbid)) + 
  geom_violin(aes(fill = auction_type_f), 
              col = "white", 
              scale = "area",
              alpha = 0.5) +
  geom_point(col = "aquamarine", alpha = 0.2) +
  original_palette + 
  labs(title = "Original: distribution of starting prices",
       x = "Type of an eBay auction", 
       y = "Starting price per auction") +
  guides(fill = F) +
  text_theme + 
  coord_cartesian(ylim = c(0, 1500)) # few outliers masked

# relationship between final price per auction and auction type
ggplot(auctions, 
       aes(x = auction_type_f, y = price)) + 
  boxplot_auction_type +
  labs(title = "Final price per action", 
       x = "Type of an auction", 
       y = "Final price") +
  guides(fill = F) +
  scale_fill_brewer(palette = "PuBuGn") + 
  text_theme

# add new column to auctions: seller_gain 
auctions <- auctions %>% 
  mutate(seller_gain = price - openbid)

# relationship between seller gain and final price
ggplot(auctions, 
       aes(x = seller_gain, y = price)) + 
  geom_jitter(alpha = 0.3, col = "lightgrey") +
  geom_smooth(se = F, 
              aes(col = auction_type_f)) +
  labs(title = "Ratio gain/price per auction", 
       x = "Total seller gain", 
       y = "Final price",
       colour = "Auction type") +
  text_theme +
  ylim(0, 4000)

# R style guide on http://adv-r.had.co.nz/Style.html
