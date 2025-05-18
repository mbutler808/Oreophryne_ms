library(tidyverse)
library(flextable)
library(dplyr)

set_flextable_defaults(
  theme_fun = theme_booktabs,
  big.mark = " ", 
  font.color = "#666666",
  border.color = "#666666",
  padding = 3,
)
## ---- sensulato --------
dat <- read.csv("../Data/Raw_data/Oreophryne_sensu_lato.csv", as.is=T)

names(dat) <- c("orig", "prev", "curr", "sp", "notes")
dat <- dat %>% separate("orig", c("orig", "auth"), sep = "\\(") %>%
	mutate(auth = paste0("(", auth)) %>%
	mutate( sp = gsub("^([^ ]*)\\s([^ ]*)\\s(.*)$","\\2", orig))
# REGEX explanation:
# ^ start of string
# ([^ ]*) = word (non-space character)
# \\s = single space
# (.*)$ = all characters til the end$

dat$sp[dat$sp == "anulatus"] <- "anulata"
dat$sp[dat$sp == "achatina"] <- "moluccensis"


gps <- read.csv("../Data/Raw_data/Oreophryne_Type_Localities_v20250513.csv", skip=1)
names(gps) <- c("orig", "latitude", "longitude")

gps$sp <- gsub("^([^ ]*)\\s([^ ]*)\\s(.*)$","\\2", gps$orig )
gps$citation <- gsub("^([^ ]*)\\s([^ ]*)\\s(.*)$","\\3", gps$orig )

dat2 <- merge(dat, gps, by="sp", all=T)
write.csv(dat2, "dat2.csv")
## ---- transfers --------
dat <- read.csv("../Data/taxonomic_transfers.csv", as.is=T)

names(dat) <- c("orig", "prev", "curr", "sp", "notes")
dat <- dat %>% separate("orig", c("orig", "auth"), sep = "\\(") %>%
	mutate(auth = paste0("(", auth)) 
	
tt <- dat |> 
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
	italic(part="body", j=c("orig", "prev", "curr", "sp"))

## ---- unknown --------
dat <- read.csv("../Data/remaining_sensu_lato.csv", as.is=T)[1]

names(dat) <- c("orig")
dat <- dat %>% separate("orig", c("orig", "auth"), sep = "\\(") %>%
	mutate(auth = paste0("(", auth)) 
	
tt <- dat |> 
	flextable() |> 
	separate_header() |> 
	autofit() |>
	set_header_labels(
		orig = "Original designation",
		auth = "Citation") |> 
	italic(part="body", j=c("orig")) |>
	set_caption("Remaining named species of _Oreophryne_ sensu lato which require phylogenetic evidence to classify into _Oreophryne_ vs. _Auparoparo_.")


  
## ---- charactermatrix --------
dat <- read.csv("../Data/character_matrix.csv")
not_examined <- dat$not_examined
genus <- dat$genus

dat <- dat[c("genus", "species", "procoracoid.cartilage", "clavicles", "omosternum", "sternum", "call.type", "finger.toe.width", "pectoral.connector", "toe.length", "toe.webbing", "tympanum.eye.diameter", "reference")]
names(dat) <- c("Genus", "Species", "Procoracoids", "Clavicles", "Omosternum", "Sternum", "Call type", "Finger: toe width", "Pectoral ligament", "Rel. toe length", "Toe webbing", "Tympanum: eye diameter", "Reference")

mt <- dat |> 
  flextable() |> 
  separate_header() |> 
  autofit() 
  
mt <- mt |>
    theme_booktabs(bold_header = TRUE) |>
	merge_v(j = 1) |> 
	valign(j = 1, valign = "top") |> 
	italic(part="body", j=c("Genus", "Species")) |>
	hline(i=c(18, 28), part="body") |>
	footnote(i = not_examined, 
           j = 2, 
           part = "body",
           ref_symbols = c("1"), 
           value = as_paragraph(
            c("Character states obtained from the literature, specimens not available for examination in this study.")
            )
          ) |> 
   add_footer_lines("*External characters were examined from specimens, internal characters and pupil shape taken from the literature.")
              

saveRDS(mt, file="../Products/Tables/character_matrix.RDS")

## ---- characterdef --------
dat <- read.csv("../Data/character_key.csv")

kt <- dat |> 
  flextable() |> 
  separate_header() |> 
  autofit() 

saveRDS(kt, file="../Products/Tables/character_key.RDS")

## ---- localities --------
dat <- read.csv("../Data/locality.csv")

lt <- dat |> 
  flextable() |> 
  separate_header() |> 
  italic(part="body", j=c("Species")) |>
  autofit() 

saveRDS(lt, file="../Products/Tables/localities.RDS")

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



#| label: tbl-morphcharacters
#| tbl-cap: "Description of traits surveyed."
#| echo: FALSE
library(flextable)
magrittr

morph_key <- readRDS("Tables/character_key.RDS")
morph_key

