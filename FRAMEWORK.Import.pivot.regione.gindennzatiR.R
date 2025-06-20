

##### 1 · LIBRERIE -----------------------------------------------------------------
library(tidyverse)   # dplyr, readr, tidyr, ggplot2 ...
library(janitor)     # clean_names()
library(lubridate)   # gestione date
library(writexl)     # export xlsx

##### 2 · PATH ---------------------------------------------------------------------
# Imposta qui la cartella principale del progetto
BASE_PATH <- setwd("/Users/danigiovanardi/Desktop/INAIL_OPENDATA/dati_originali")

# Sotto-cartelle (rispetta la struttura usata per la tesi)
PATH_MENSILI    <- file.path(BASE_PATH, "dati_originali", "dati_mensili")
PATH_SEMESTRALI <- file.path(BASE_PATH, "dati_originali", "dati_semestrali")
PATH_DECODIFICA <- file.path(BASE_PATH, "dati_originali", "dataset_decodifica")
PATH_LOG        <- file.path(BASE_PATH, "log")
PATH_OUT        <- file.path(BASE_PATH, "dati_processati")
dir.create(PATH_LOG, showWarnings = FALSE)
dir.create(PATH_OUT, showWarnings = FALSE)

##### 3 · FUNZIONI DI SERVIZIO ------------------------------------------------------
log_file <- file.path(PATH_LOG, "FRAMEWORK_import_v1.txt")
scrivi_log <- function(msg) {
  cat(sprintf("[%s] %s\n", Sys.time(), msg), file = log_file, append = TRUE)
}

importa_csv <- function(folder, cadenza) {
  files <- list.files(folder, "\\.csv$", full.names = TRUE)
  scrivi_log(sprintf("→ %s: %d file trovati", basename(folder), length(files)))
  
  purrr::map_dfr(files, ~ {
    nome_file <- basename(.x)
    regione   <- str_remove_all(nome_file, "(Semestrale|Mensile|\\.csv)")
    
    df <- tryCatch(
      read_delim(.x, delim = ";", show_col_types = FALSE) %>%
        clean_names() %>%
        mutate(
          regione  = regione,
          cadenza  = cadenza
        ),
      error = function(e) {
        scrivi_log(paste("ERRORE import:", .x))
        NULL
      }
    )
  })
}

uniforma_tipi <- function(df) {
  chr_cols <- c("identificativoinfortunato", "identificativocaso",
                "identificativodatorelavoro", "posizioneassicurativaterritoriale",
                "gestione", "gestionetariffaria", "gradomenomazione")
  num_cols <- c("eta", "giorniindennizzati")
  
  df %>%
    mutate(across(any_of(chr_cols),  as.character),
           across(any_of(num_cols),  ~ suppressWarnings(as.numeric(.x))))
}

converti_date <- function(df, col_data = "dataaccadimento") {
  if (col_data %in% names(df)) {
    df %>% 
      mutate(
        {{col_data}} := dmy({{ col_data }}),
        anno         = year({{ col_data }}),
        mese         = month({{ col_data }}),
        anno_mese    = format({{ col_data }}, "%Y-%m"),
        semestre     = if_else(mese <= 6, "H1", "H2"),
        anno_semestre= paste0(anno, "_", semestre)
      )
  } else df
}

##### 4 · IMPORT --------------------------------------------------------------------
scrivi_log("== START IMPORT ==")
dati_sem  <- importa_csv(PATH_SEMESTRALI, "semestrale")
dati_mens <- importa_csv(PATH_MENSILI,    "mensile")
dati_cod  <- importa_csv(PATH_DECODIFICA, "decodifica")
scrivi_log("== END IMPORT ==")

##### 5 · PULIZIA BASE -------------------------------------------------------------
dati_sem  <- dati_sem  %>% uniforma_tipi() %>% converti_date()
dati_mens <- dati_mens %>% uniforma_tipi() %>% converti_date()
scrivi_log("Uniformati tipi di dato e convertite le date")

##### 6 · EXPORT RData -------------------------------------------------------------
save(dati_sem,  file = file.path(PATH_OUT, "INAIL_Semestrale.RData"))
save(dati_mens, file = file.path(PATH_OUT, "INAIL_Mensile.RData"))
save(dati_cod,  file = file.path(PATH_OUT, "INAIL_Codifica.RData"))
scrivi_log("Salvati i tre dataset processati")

##### 7 · CHECK DI COERENZA --------------------------------------------------------
check_intervallo <- function(df) {
  if ("dataaccadimento" %in% names(df)) {
    intervallo <- range(df$dataaccadimento, na.rm = TRUE)
    scrivi_log(paste("Range date:", paste(intervallo, collapse = " → ")))
  }
}
check_intervallo(dati_sem)
check_intervallo(dati_mens)

##### 8 · RIEPILOGO “Giorni indennizzati” PER REGIONE E ANNO -----------------------
pivot_infortuni <- dati_sem %>% 
  filter(anno %in% 2019:2023) %>% 
  group_by(regione, anno) %>% 
  summarise(giorni_indennizzati = sum(giorniindennizzati, na.rm = TRUE), .groups = "drop") %>% 
  pivot_wider(names_from = anno, values_from = giorni_indennizzati) %>% 
  mutate(media_2019_2023 = rowMeans(across(`2019`:`2023`), na.rm = TRUE)) %>% 
  arrange(desc(media_2019_2023))

write_xlsx(pivot_infortuni, file.path(PATH_OUT, "Infortuni_PIVOT_con_Media.xlsx"))

##### 9 · PLOT FACET REGIONALE ------------------------------------------------------
plot_facet <- pivot_infortuni %>% 
  pivot_longer(cols = `2019`:`2023`, names_to = "anno", values_to = "giorni_indennizzati") %>% 
  ggplot(aes(x = anno, y = giorni_indennizzati)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~ regione, scales = "free_y") +
  labs(title = "Giorni indennizzati per Regione (2019–2023)",
       x = "Anno", y = "Giorni indennizzati") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(PATH_OUT, "facet_giorni_regione_2019_2023.png"),
       plot_facet, width = 12, height = 8, dpi = 300)

##### 10 · DOT PLOT DELLE MEDIE REGIONALI -----------------------------------------
media_nazionale <- mean(pivot_infortuni$media_2019_2023, na.rm = TRUE)

dot_media <- ggplot(pivot_infortuni, aes(x = media_2019_2023,
                                         y = reorder(regione, media_2019_2023))) +
  geom_point(size = 3, colour = "steelblue") +
  geom_vline(xintercept = media_nazionale, colour = "red",
             linetype = "dashed", linewidth = 1) +
  labs(title = "Media giorni indennizzati per Regione (2019–2023)",
       x = "Media giorni indennizzati",
       y = "Regione") +
  theme_minimal()

ggsave(file.path(PATH_OUT, "dot_media_giorni_regione.png"),
       dot_media, width = 10, height = 7, dpi = 300)

scrivi_log("Workflow completato con successo")
