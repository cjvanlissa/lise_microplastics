current_env <- ls()
df <- read.csv("data_thesis_essential columns.csv", sep = ";", dec = ",", na.strings = c("NA", "na", ""), stringsAsFactors = FALSE)
names(df)[1] <- "current"
df$A..mm. <- gsub(",", ".", df$A..mm., fixed = TRUE)
df$H..mm. <- gsub(",", ".", df$H..mm., fixed = TRUE)
df <- df[, !grepl("^CSF", names(df))]
missingness <- rowSums(is.na(df))
#table(missingness)
df <- df[missingness < (ncol(df)-1), ]

missingness <- colSums(is.na(df))
#table(missingness)

df <- df[, !missingness > nrow(df)]

illegal_values <- function(x){
  suppressWarnings(
    table(x[!is.na(x) & is.na(as.numeric(x))])  
  )
}

df[c("S", "N", "L..mm.", "W..mm.")] <- lapply(df[c("S", "N", "L..mm.", "W..mm.")], as.numeric)

# Reorder L and W
#all(df$L..mm. >= df$W..mm.)
df[,c("L..mm.", "W..mm.")] <- t(apply(df[,c("L..mm.", "W..mm.")], 1, function(x){c(max(x), min(x))}))
#all(df$L..mm. >= df$W..mm.)

#illegal_values(df$A..mm.)
df$A..mm.[df$A..mm. %in% c("#VALUE!", "na")] <- NA
df$A..mm. <- as.numeric(df$A..mm.)

#illegal_values(df$H..mm.)
df$H..mm.[df$H..mm. %in% c("na", "foutje")] <- NA
df$H..mm. <- as.numeric(df$H..mm.)

# Remove imputed H..5mm. values
has_h <- !is.na(df$H..5mm.) & !df$H..5mm. == ""
h <- df$H..5mm.[has_h]
num_decimals <- nchar(gsub("^.+\\.", "", h))
df$H..5mm.[has_h][num_decimals > 1] <- NA

df$H..5mm. <- as.numeric(df$H..5mm.)

re_categorize <- function(x, categories){
  x <- as.character(x)
  out <- x
  for(i in names(categories)){
    out[x %in% categories[[i]]] <- i
  }
  out[!out %in% c(NA, names(categories), unlist(categories))] <- "other"
  out
}


# Categorize PT -----------------------------------------------------------
categories <- read.table("categories.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE)
categories <- read.table("categories_LM.csv", sep = ";", stringsAsFactors = FALSE, header = TRUE)
# Make empty cells NA
df$PT[df$PT == ""] <- NA
if(!all(df$PT %in% categories$Original)){
  stop("Couldn't match some cats")
}
df$PT[df$PT %in% categories$Original[is.na(categories$Rename)]] <- NA
for(this_cat in unique(c(NA, categories$Rename))[-1]){
  #this_cat = "Other"
  # if(!(all(categories$Original[which(categories$Rename == this_cat)] %in% df$PT))){
  #   stop("Couldn't match some cats")
  # }
  df$PT[which(df$PT %in% categories$Original[which(categories$Rename == this_cat)])] <- this_cat
}
#table(df$PT, tmp, useNA = "always")
# Code any cell containing "PP" as "PP"
df$PT[grepl("PP", df$PT)] <- "PP"
# Code any cell containing with *(optional)PE as "PE"
df$PT[grepl("^\\*?PE\\s{0,}$", df$PT)] <- "PE"
table(df$PT, useNA = "always")

PT_cat <- list(
  PP = grep("PP", levels(df$PT), value = TRUE),
  PE = grep("^*?PE\\s{0,}$", levels(df$PT), value = TRUE)
)

df$cat <- factor(df$category)
Cat_cat <- list(
  Film = grep("ilm", levels(df$cat), value = TRUE),
  Foam = grep("foam", levels(df$cat), value = TRUE),
  Fragment = grep("fragment", levels(df$cat), value = TRUE),
  Line = grep("line", levels(df$cat), value = TRUE),
  Pellet = grep("pellet", levels(df$cat), value = TRUE)
)
df$category <- re_categorize(df$cat, Cat_cat)
df$category[df$category %in% c(c("Foam", "Pellet"))] <- "Fragment"
table(df$category)
#table(df$cat, df$category)

names(df)[match(c("S", "L..mm.", "W..mm.", "A..mm.", "H..mm.", "H..5mm.", "PT"), names(df))] <- c("sample", "length", "width", "area", "height_est", "height_obs", "poly_type")

# For lines, calculate length from area divided by average diameter of a sample of lines,
# to account for potential curvature of lines inside bounding box
calculated_lengths <- df$area[df$category == "Line"]/0.65
calculated_lengths[is.na(calculated_lengths)] <- df$length[df$category == "Line"][which(is.na(calculated_lengths))]
df$length[df$category == "Line"] <- calculated_lengths


# Fix lise's problem about 50 duplicates in 5mm cat -----------------------

# dups <- df$sample %in% c(22, 23, 24, 25, 27)
# dups <- dups & trimws(df$L.cat.) == "0.5mm"
# dups <- dups& sample == 22
# df$length2 <- NA
# df$length2[df$category == "Line"] <- df$area[df$category == "Line"]/0.42
# 
# tmp <- df[df$category == "Line", c("length", "length2")]
# names(tmp) <- paste0("Variable.",names(tmp))
# tmp <- reshape(tmp, direction = "long", varying = names(tmp))
# ggplot(tmp, aes(x = Variable, colour = time, fill = time)) + geom_density(alpha = .2)
#cor(tmp$length, tmp$length_line, use = "pairwise.complete.obs")
df <- df[, c("current", "sample", "length", "width", "height_est", "height_obs", "category", "poly_type")]

# Imputed height correlates poorly with actual height, even for the fragments
# sapply(unique(df$category), function(x){
#   #x <- "Fragment"
#   tmp <- df[df$category == x, c("height_est", "height_obs")]
#   c(x, sum(complete.cases(tmp)), cor(tmp$height_est, tmp$height_obs, use = "pairwise.complete.obs"))
# })

tmp <- df[!is.na(df$height_obs) & df$category == "Fragment", ]
#cor(tmp[, c("length", "width", "height_obs")])
res <- lm(height_obs~length + I(length^2)+width+I(width^2), tmp)
res <- lm(height_obs~length + width, tmp)
#summary(res)
p <- ggplot(tmp, aes(x= width, y = height_obs))+geom_point()
width <- seq(0, max(tmp$width), length.out = 1000)
height_obs <- predict(res, newdata = data.frame(length = mean(tmp$length), width = width))
#p + geom_path(data = data.frame(width = width, height_obs = height_obs))
df_pc <- df[df$category == "Fragment", c("length", "width")]
pc <- principal(df_pc, nfactors = 2)
#plot(pc$scores[,1], pc$scores[,2])

#pc$values
# And for Fragment, LxWxH carry very similar information

#table(df$height_est[df$category %in% c("Film", "Line")], df$category[df$category %in% c("Film", "Line")])

df$Two_dim <- as.numeric(df$category %in% c("Film", "Line"))
df$Film <- as.numeric(df$category == "Film")
df$Line <- as.numeric(df$category == "Line")

# Remove outliers:
check_vars <- c("length", "width")

outliers <- lapply(list(
  "Film",
  "Line",
  c("Foam", "Fragment", "Pellet")
), function(cat){
  #cat <- "Line"
  select_cases <- which(df$category %in% cat)
  if(cat[1] == "Line"){
    outl <- scale(df[select_cases, "length"])
    select_cases[abs(as.vector(outl))>3]
  } else {
    mah <- mahalanobis(df[select_cases, check_vars], colMeans(df[select_cases, check_vars], na.rm = TRUE), cov(df[select_cases, check_vars]))
    select_cases[mah > 13.82]
  }
  })

df <- df[-unlist(outliers), ]
# Remove particles shorter than 1 mm
df <- df[-which(df$length < 1), ]
# Checking for outliers , we see that the majority of our data for length falls within a 0 1  -10 mm range, with a maximum of 325.8 mm,  and a median of 2.73 mm and a range of 324.79 mm.  For width, we see find a minimum of 0.2 mm, maximum of 55 mm,  and median of 1.66 mm and a range of 54.8 mm . 

tmp <- tidySEM::descriptives(df[, c("length", "width", "height_obs", "category", "poly_type")])
write.csv(tmp, "descriptives.csv", row.names = FALSE)
write.csv(df, "df.csv", row.names = FALSE)
new_env <- ls()
rm(list = new_env[!new_env %in% current_env])
rm("new_env")