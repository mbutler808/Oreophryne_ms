# Oreophryne data

README.md

# wrong name! Searched and replaced sp.2 or sp.3 -> gagneorum

20571
20572
20538
20532

Oreophryne_B sp.2
Oreophryne_B sp.3
Oreophryne_B sp.3
Oreophryne_B sp.3

Oreophryne gagneorum
Oreophryne gagneorum
Oreophryne gagneorum
Oreophryne gagneorum

Oreophryne

```{r}
#| label: tbl-transfers
#| tbl-cap: "Classification of _Auparoparo_ and _Oreophryne_ based on phylogenetic evidence from two mitochondrial gene fragments (CytB, ND4) and three nuclear loci (BDNF, SIA, NXC)."
#| echo: FALSE
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

tt  
```

```{r}
#| label: tbl-remaining
#| tbl-cap: "Remaining named species of _Oreophryne_ sensu lato which require phylogenetic evidence to classify into _Oreophryne_ vs. _Auparoparo_."
#| echo: FALSE
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

tt
```