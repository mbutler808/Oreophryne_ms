## ---- tablepackages --------

library(tidyverse)
library(flextable)
library(officer)
library(dplyr)

## ---- flextablesetup --------

set_flextable_defaults(
  theme_fun = theme_booktabs,
  big.mark = " ", 
  font.color = "#666666",
  border.color = "#666666",
  padding = 2,
)
landscape_properties <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 8.3, height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar()
)

portrait_properties <- prop_section(
  page_size = page_size(
    orient = "portrait",
    width = 8.5, height = 11
  ),
  type = "continuous",
  page_margins = page_mar()
)

# Function to add table captions for docx
addcap <- function(.flextable, table_caption) {
  newtable <- .flextable |>
  add_header_lines(values = table_caption) |>
    bold(part = "header", i = 1) |>
    align(part = "header", i = c(1:length(table_caption)), align = "left") |>
  border(part = "head", i = c(1:length(table_caption)),
         border = list("width" = 0, color = "black", style = "solid")) |>
  border(part = "head", i = length(table_caption),
         border.bottom = list("width" = 1, color = "black", style = "solid")) 
  return(newtable)
}


## ---- localities --------
dat <- read.csv("../Data/Raw_data/locality.csv")

lt <- dat |> 
  flextable() |> 
  separate_header() |> 
  italic(part="body", j=c("Species")) |>
  autofit() |>
  set_header_labels(Latitude.Longitude = "Latitude, Longitude")

table_caption = c("Table 1.", "Locality information for samples sequenced in this study. See Hill, et al., 2023, Table 1 for additional metadata.")

doclt <- lt |>
  addcap(table_caption) |>
	width(j=1:4, width=c(0.93, 2.03, 2.26, 1.32))  

save_as_docx(
  doclt, 
  path = "../Results/table1.localities.docx",
  pr_section = portrait_properties
)  

saveRDS(lt, file="../Data/Processed_data/localities.RDS")


## ---- characterdef --------
dat <- read.csv("../Data/Raw_data/character_key.csv")
dat$Column <- c(1:11, "", "", "")
dat <- dat[c("Column", "Trait", "Definition")]

kt <- dat |> 
  flextable() |> 
  separate_header() |> 
  autofit() |>
  add_footer_lines("*Corresponding column in Table 3.") |>
  set_header_labels(Column = "Column*")

table_caption = c("Table 2.", "Description of traits surveyed. Several traits are traditionally noted for Asterophryinae species: presence of two palatal folds, horizontal pupil, and lack of vomerine teeth (Boettger 1895; VanKampen 1923; Parker 1934).")

dockt <- kt |>
  addcap(table_caption) |>
 	width(j=1:3, width=c(0.79, 1.51, 4.23))  

save_as_docx(
  dockt, 
  path = "../Results/table2.characterkey.docx",
  pr_section = portrait_properties
)  
saveRDS(kt, file="../Data/Processed_data/character_key.RDS")

## ---- charactermatrix --------
dat <- read.csv("../Data/Raw_data/character_matrix.csv")
dat$pectoral.connector <- trimws(dat$pectoral.connector)
not_examined <- dat$not_examined
genus <- dat$genus

dat <- dat[c("genus", "species", "procoracoid.cartilage", "clavicles", "omosternum", "sternum", "call.type", "finger.toe.width", "pectoral.connector", "toe.length", "toe.webbing", "tympanum.eye.diameter", "reference")]
		
dat <- dat |> 
		mutate( procoracoid.cartilage = "CR") |>
		mutate( clavicles="CR") |>
		mutate( omosternum ="\u2013") |>
		mutate( sternum = ifelse( sternum=="cartilaginous",                        "cart", sternum)) |>
		mutate( pectoral.connector = 
					case_when( pectoral.connector=="cartilaginous" ~ "cart",
								     pectoral.connector=="ligamentous" ~ "lig",
								     TRUE ~ ""
							     )
			    )|>
		mutate( call.type = 
					case_when( call.type=="peeping" ~ "peep",
						    		 call.type=="chattering" ~ "chatter",
								     TRUE ~ call.type
							     )
			    )|>
		mutate( toe.webbing = ifelse( toe.webbing=="none", 
                                  "\u2013", 
                                  toe.webbing
                                )
          )

names(dat) <- c("Genus", "species", "proc", "clav", "omo", "ster", "call type", "fing: toe width", "pec lig", "rel toe len", "toe web", "tymp:eye diam", "reference")


mt <- dat |> 
  flextable() |> 
  separate_header() |> 
  autofit() |>
	add_header_row( values = c("", "", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)", "(11)")) |>
  theme_booktabs(bold_header = TRUE) |>
	merge_v(j = 1) |> 
	valign(j = 1, valign = "top") |> 
	italic(part="body", j=c("Genus", "species")) |>
	hline(i=c(18, 28), part="body") |>
	footnote(i = not_examined, 
           j = 2, 
           part = "body",
           ref_symbols = c("1"), 
           value = as_paragraph(
            c("Character states obtained from the literature, specimens not available for examination in this study.")
            )
          ) |> 
  footnote(i = 19,
           j = 1,
           part = "body",
           ref_symbols = c("2"),
           value = as_paragraph(
            "Hill et al. 2022 places ", as_i("Aphantophryne pansa"), " within ", as_i("Auparoparo"), ", but the status of ", as_i("Aphantophryne"), " requires further study with samples from additional species and localities."
            )
          ) |>
   add_footer_lines("*External characters were examined from specimens, internal characters and pupil shape taken from the literature.") |> 
   add_footer_lines("Columns: (1) Procoracoids and (2) Clavicles CR= curved reduced, (3) Omasternum , (4) Sternum, (5) Call Type, (6) Finger to Toe Ratio, (7) Pectoral Connector Type, (8) Relative Toe Length of the fifth to third toe, (9) Toe Webbing, (10) Typanum to Eye Diameter Ratio, (11) Reference. See Table 2 for character states.")

table_caption = c("Table 3.", "Morphological character analysis.")
              
docmt <- mt |>
  addcap(table_caption) |>
	width(j=1:13, width=c(1.03, 1.42, .56, .53, .58, .52, .67, .67, .53, .55, .74, .64, 1.21))  

save_as_docx(
  docmt, 
  path = "../Results/table3.charactermatrix.docx",
  pr_section = landscape_properties
)  
saveRDS(mt, file="../Data/Processed_data/character_matrix.RDS")


# library(palmerpenguins)

# dat <- penguins |> 
  # select(species, island, ends_with("mm")) |> 
  # group_by(species, island) |> 
  # summarise(
    # across(
      # where(is.numeric), 
      # .fns = list(
        # avg = ~ mean(.x, na.rm = TRUE),
        # sd = ~ sd(.x, na.rm = TRUE)
      # )
    # ),
    # .groups = "drop") |> 
  # rename_with(~ tolower(gsub("_mm_", "_", .x, fixed = TRUE)))

# set_header_labels(ft, Solar.R = "Solar R (lang)",     Temp = "Temperature (degrees F)", Wind = "Wind (mph)",    Ozone = "Ozone (ppb)" )
#  align(align = "center", part = "all", j = 3:8) |> 
#  colformat_double(digits = 2) |>

## ---- genustypes --------

gt <- read.csv("../Data/genus_type.csv") |>
  flextable() |>
  separate_header() |>
  italic(part="body", j=c("Genus")) |>
  autofit() |>
  add_footer_lines("*We note that the monophyly of Austrochaperina, Liophryne, Oxydactyla, and Sphenophryne (and reciprocal monophyly of Genyophryne [Genyophryne thomsoni Boulenger, 1890] with respect to Liophryne + Oxytactyla + Sphenophryne) has not yet been established.") 

table_caption = c("Table 4.", "Genera of Asterophryinae and their type species.")

docgt <- gt |>
  addcap(table_caption) 

save_as_docx(
  docgt, 
  path = "../Results/table4.genustypes.docx",
  pr_section = portrait_properties
)  
saveRDS(gt, file = "../Data/Processed_data/genus_types.RDS")
write.csv(dat, "../Data/Processed_data/genus_types.csv", row.names=F)

## ---- taxonomy --------
dat <- read.csv("../Data/Raw_data/Oreophryne_sensu_lato.csv", as.is=T)

names(dat) <- c("orig", "prev", "curr", "sp", "notes")
dat <- dat |> 
#	mutate(notes = if_else (notes=="", "Phylogenetic evidence needed", notes)) |>
	mutate(curr = if_else (curr=="Incertae sedis", "incertae sedis", curr)) |>
	separate("orig", c("orig", "auth"), sep = "\\(") |>
	mutate(auth = paste0("(", auth)) |>
	mutate( sp = gsub("^([^ ]*)\\s([^ ]*)\\s(.*)$","\\2", orig)) 
# REGEX explanation:
# ^ start of string
# ([^ ]*) = word (non-space character)
# \\s = single space
# (.*)$ = all characters til the end$

dat$sp[dat$sp == "anulatus"] <- "anulata"
dat$sp[dat$sp == "achatina"] <- "moluccensis"
hlinerows <- table(dat$curr)[c("Oreophryne", "Auparoparo", "incertae sedis")]
hlinerows <- c(hlinerows[1], sum(hlinerows[c(1,2)]))

ft <- dat |> 
  flextable() |> 
  separate_header() |> 
  autofit() |>
  set_header_labels(
    orig = "Original designation",
    auth = "Citation",
    prev = "Previous generic placement", 
    curr = "Current generic placement",
    sp = "Species",
    notes = "Notes") |> 
  hline(i=hlinerows, part="body") |>
  italic(part="body", j=c("orig", "prev", "curr", "sp")) 

table_caption = c("Table 5.", "Classification of Auparoparo and Oreophryne based on phylogenetic evidence from two mitochondrial gene fragments (CytB, ND4) and three nuclear loci (BDNF, SIA, NXC). Incertae sedis taxa require phylogenetic evidence for classification.")

docft <- ft |>
  addcap(table_caption) |>
    width(j=1:6, width=c(2.13, 1.75, 1.2, 1.2, 1.5, 2)) 
 

save_as_docx(
  docft, 
  path = "../Results/table5.taxonomy.docx",
  pr_section = landscape_properties
)  
saveRDS(ft, file = "../Data/Processed_data/taxonomy.RDS")
write.csv(dat, "../Data/Processed_data/Oreophryne_sensu_lato.csv", row.names=F)


## ---- typelocalities --------

gps <- read.csv("../Data/Raw_data/Oreophryne_Type_Localities_v20250513.csv", skip=1)
names(gps) <- c("orig", "latitude", "longitude")

gps$sp <- gsub("^([^ ]*)\\s([^ ]*)\\s(.*)$","\\2", gps$orig )
gps$citation <- gsub("^([^ ]*)\\s([^ ]*)\\s(.*)$","\\3", gps$orig )

dat2 <- merge(dat, gps, by="sp", all=T)
write.csv(dat2, "../Data/Processed_data/oreophryne-type-localities.csv")
  


