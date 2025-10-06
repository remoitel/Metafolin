# table output

library(flextable)
library(janitor)

#FK
import_tab <- import |> 
  filter(str_detect(`Batch`, "FK")) |> 
  pivot_wider(names_from = Parameter, values_from = Value,values_fn = first)  |>    #vectors in cells: use first value in vector
  select(Batch,Temp,Months, `Appearance color` ,`Appearance texture` , `Appearance odor`,pH,`Water content`, `Assay Folinate as is (acid)`, 
         `Assay H2O- and solv. free Folinate-Ca`, `Assay H2O free Folinate-Ca`,`Assay Imp. [2] (A) / ABGA`, 
         `Assay Imp. [4] (B) / DFTHFA`,`Assay Imp. [6] (C) / FA`,`Assay Imp. [5] (D) / FFA`, `Assay Imp. [1] (E) / FTHPA`,
         `Assay Imp. [3] (F) / FDHFA`,  `Assay Imp. [12] (G) / DHFA`,`Assay sum [7a] / [7b]`,`Other individual known RC`,
         `Individual known RC`, `Individual unknown RC`, `Individual unspecified RC`, `Sum of all related compounds`) |> 
arrange(Batch,Temp, Months) %>%
  group_by(Batch)

import_tab$`pH` <- round_half_up(as.numeric(import_tab$`pH`), 2)
import_tab$`Water content` <- round_half_up(as.numeric(import_tab$`Water content`), 2)
import_tab$`Assay Folinate as is (acid)` <- round_half_up(as.numeric(import_tab$`Assay Folinate as is (acid)`), 2)
import_tab$`Assay H2O free Folinate-Ca` <- round_half_up(as.numeric(import_tab$`Assay H2O free Folinate-Ca`), 2)
import_tab$`Assay Imp. [2] (A) / ABGA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [2] (A) / ABGA`), 3)
import_tab$`Assay Imp. [4] (B) / DFTHFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [4] (B) / DFTHFA`), 3)
import_tab$`Assay Imp. [6] (C) / FA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [6] (C) / FA`), 3)
import_tab$`Assay Imp. [5] (D) / FFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [5] (D) / FFA`), 3)
import_tab$`Assay Imp. [1] (E) / FTHPA`  <- round_half_up(as.numeric(import_tab$`Assay Imp. [1] (E) / FTHPA`),3)
import_tab$`Assay Imp. [3] (F) / FDHFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [3] (F) / FDHFA`), 3)
import_tab$`Assay Imp. [12] (G) / DHFA`  <- round_half_up(as.numeric(import_tab$`Assay Imp. [12] (G) / DHFA`), 3)
import_tab$`Assay sum [7a] / [7b]` <- round_half_up(as.numeric(import_tab$`Assay sum [7a] / [7b]`), 2)
import_tab$`Other individual known RC` <- round_half_up(as.numeric(import_tab$`Other individual known RC`), 3)
import_tab$`Individual known RC` <- round_half_up(as.numeric(import_tab$`Individual known RC`), 2)
import_tab$`Individual unknown RC` <- round_half_up(as.numeric(import_tab$`Individual unknown RC`), 2)
import_tab$`Individual unspecified RC` <- round_half_up(as.numeric(import_tab$`Individual unspecified RC`), 2)
import_tab$`Sum of all related compounds`  <- round_half_up(as.numeric(import_tab$`Sum of all related compounds`), 2)

# import_tab$`Assay Imp. [7a+b] (I) / CH2THFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [7a+b] (I) / CH2THFA`), 3)
# import_tab$`Assay Imp. THFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. THFA`), 3)
# import_tab$`Assay Imp. FTHFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. FTHFA`), 3)
# import_tab$`Purity Folinate` <- round_half_up(as.numeric(import_tab$`Purity Folinate`), 2)
# import_tab$`Assay Imp. [10] / HL` <- round_half_up(as.numeric(import_tab$`Assay Imp. [10] / HL`), 3)
# import_tab$`Sum unknown RC` <- round_half_up(as.numeric(import_tab$`Sum unknown RC`), 2)
# import_tab$`Assay Imp. [13]` <- round_half_up(as.numeric(import_tab$`Assay Imp. [13]`), 3)
# import_tab$`Sum of unspecified RC` <- round_half_up(as.numeric(import_tab$`Sum of unspecified RC`), 2)
# import_tab$`Assay Imp. [8] / THP5O` <- round_half_up(as.numeric(import_tab$`Assay Imp. [8] / THP5O`), 3)

# import_tab$`` <- round_half_up(as.numeric(import_tab$``), 2)

import_tab <- as_grouped_data(x = import_tab, groups = c("Batch", "Temp"))

tab1 <- flextable(import_tab)
tab1 <- set_header_labels(tab1, Temp = "Storage Condition", Months = "Duration [months]")

tab1 <- theme_vanilla(tab1)
tab1

save_as_docx(tab1, path = "~/SAP/SAP GUI/FK_tab.docx")


#FA
import_tab <- import %>%
  filter(str_detect(`Batch`, "FA")) |> 
  pivot_wider(names_from = Parameter, values_from = Value,values_fn = first) |>  #vectors in cells: use first value in vector
select("Batch","Temp","Months", "Appearance color" ,"Appearance texture" ,pH,`Water content`, `Assay H2O- and solv. free Folinate-Ca`, 
       `Assay Imp. [2] (A) / ABGA`, `Assay Imp. [4] (B) / DFTHFA`,`Assay Imp. [6] (C) / FA`,`Assay Imp. [5] (D) / FFA`,
       `Assay Imp. [1] (E) / FTHPA`,`Assay Imp. [3] (F) / FDHFA`,  `Assay Imp. [12] (G) / DHFA`,`Assay Imp. [7a+b] (I) / CH2THFA`,
       `Individual unspecified RC`, `Sum of all rel. Compounds`) |> 
  arrange(Batch,Temp, Months) %>%
  group_by(Batch)

import_tab$`pH` <- round_half_up(as.numeric(import_tab$`pH`), 2)
import_tab$`Water content` <- round_half_up(as.numeric(import_tab$`Water content`), 2)
import_tab$`Assay H2O- and solv. free Folinate-Ca` <- round_half_up(as.numeric(import_tab$`Assay H2O- and solv. free Folinate-Ca`), 2)
import_tab$`Assay Imp. [2] (A) / ABGA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [2] (A) / ABGA`), 3)
import_tab$`Assay Imp. [4] (B) / DFTHFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [4] (B) / DFTHFA`), 3)
import_tab$`Assay Imp. [6] (C) / FA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [6] (C) / FA`), 3)
import_tab$`Assay Imp. [5] (D) / FFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [5] (D) / FFA`), 3)
import_tab$`Assay Imp. [1] (E) / FTHPA`  <- round_half_up(as.numeric(import_tab$`Assay Imp. [1] (E) / FTHPA`),3)
import_tab$`Assay Imp. [3] (F) / FDHFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [3] (F) / FDHFA`), 3)
import_tab$`Assay Imp. [12] (G) / DHFA`  <- round_half_up(as.numeric(import_tab$`Assay Imp. [12] (G) / DHFA`), 3)
import_tab$`Assay Imp. [7a+b] (I) / CH2THFA` <- round_half_up(as.numeric(import_tab$`Assay Imp. [7a+b] (I) / CH2THFA`), 2)
import_tab$`Individual unspecified RC` <- round_half_up(as.numeric(import_tab$`Individual unspecified RC`), 2)
import_tab$`Sum of all rel. Compounds`  <- round_half_up(as.numeric(import_tab$`Sum of all rel. Compounds`), 2)

# import_tab$`` <- round_half_up(as.numeric(import_tab$``), 2)

import_tab <- as_grouped_data(x = import_tab, groups = c("Batch", "Temp"))

tab1 <- flextable(import_tab)
tab1 <- set_header_labels(tab1, Temp = "Storage Condition", Months = "Duration [months]")

tab1 <- theme_vanilla(tab1)
tab1

save_as_docx(tab1, path = "~/SAP/SAP GUI/FA_tab.docx")

