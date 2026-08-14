library(httr)
library(rvest)
library(jsonlite)
library(stringr)

start_time <- Sys.time()
token <- Sys.getenv("TELEGRAM_TOKEN")
chat_id <- Sys.getenv("TELEGRAM_CHAT_ID")

send_telegram <- function(msg) {
  url <- paste0("https://api.telegram.org/bot", token, "/sendMessage")
  res <- POST(url, body = list(chat_id = chat_id, text = msg), encode = "form")
  if (status_code(res) != 200) {
    stop(paste("Telegram rechazó el mensaje con código", status_code(res)))
  }
}

# Función auxiliar para extraer números decimales de un texto
parse_number_dr <- function(text) {
  clean_text <- gsub("[^0-9.,]", "", text)
  # Si usa formato 1,081.065928
  clean_text <- gsub(",", "", clean_text)
  as.numeric(clean_text)
}

tryCatch({
  state_file <- "estado_fondos.json"
  
  old_state <- list()
  if (file.exists(state_file) && file.info(state_file)$size > 2) {
    tryCatch({ old_state <- fromJSON(state_file) }, error = function(e) list())
  }
  new_state <- old_state

  fecha_hoy <- format(Sys.Date(), "%d/%m/%Y")

  # ==========================================
  # 1. FIDUCIARIA RESERVAS (FOP Multiplaza)
  # ==========================================
  url_fiduciaria <- "https://www.fiduciariareservas.com/proyectos-oferta-publica/fideicomiso-de-oferta-publica-de-valores-multiplaza-fr-n02/"
  web_fid <- read_html(url_fiduciaria)
  
  # Buscar el texto que contiene "Valor patrimonial"
  node_fid <- html_element(web_fid, xpath = "//*[contains(text(), 'Valor patrimonial') or contains(text(), '1,08')]")
  
  if (!is.null(node_fid)) {
    val_fid_raw <- html_text(node_fid)
    val_fid <- parse_number_dr(val_fid_raw)
    
    if (!is.na(val_fid)) {
      prev_fid <- old_state[["multiplaza_valor"]]
      
      if (!is.null(prev_fid) && val_fid != prev_fid) {
        inv_fid <- val_fid * 20
        msg_fid <- paste0(
          fecha_hoy, "\n",
          "Valor cuota FOP Multiplaza RD$", format(val_fid, nsmall = 6), "\n",
          "Valor inversión RD$", format(inv_fid, big.mark = ",", nsmall = 2)
        )
        send_telegram(msg_fid)
      }
      new_state[["multiplaza_valor"]] <- val_fid
    }
  }

  # ==========================================
  # 2. AFI UNIVERSAL (Universal Liquidez)
  # ==========================================
  url_afi <- "https://www.afiuniversal.com.do/universal-liquidez/"
  web_afi <- read_html(url_afi)
  
  # Estructura de fondos a monitorear
  fondos_afi <- list(
    list(key = "uni_liq", name = "Cuota Universal Liquidez", mult = 57, match = "Liquidez"),
    list(key = "dep_flex", name = "Cuota Dep. Financiero Flexible", mult = 1, match = "Flexible"),
    list(key = "plazo_dol", name = "Cuota Plazo mensual dólar", mult = 1, match = "Dólar")
  )

  texto_pagina_afi <- html_text(web_afi)
  
  # Extraer todos los números con formato de cuota (ej: 1,727.399462)
  numeros_cuotas <- str_extract_all(texto_pagina_afi, "\\b\\d{1,3}(,\\d{3})*\\.\\d{4,6}\\b")[[1]]
  
  if (length(numeros_cuotas) >= 3) {
    valores_detectados <- sapply(numeros_cuotas[1:3], parse_number_dr)
    
    for (idx in seq_along(fondos_afi)) {
      f <- fondos_afi[[idx]]
      val_actual <- valores_detectados[idx]
      prev_val <- old_state[[f$key]]
      
      if (!is.null(prev_val) && val_actual != prev_val) {
        val_inv <- val_actual * f$mult
        msg_afi <- paste0(
          fecha_hoy, "\n",
          f$name, " ", format(val_actual, nsmall = 6), "\n",
          "Valor inversion ", format(val_inv, big.mark = ",", nsmall = 2)
        )
        send_telegram(msg_afi)
      }
      new_state[[f$key]] <- val_actual
    }
  }

  # ==========================================
  # 3. CEVALDOM (Mercado OTC)
  # ==========================================
  url_cevaldom <- "https://www.cevaldom.com/mercado/otc/"
  web_cevaldom <- read_html(url_cevaldom)
  
  # Buscar filas de tabla que contengan el ISIN objetivo
  filas_isin <- html_elements(web_cevaldom, xpath = "//tr[contains(., 'DO9035100120')]")
  
  if (length(filas_isin) > 0) {
    for (fila in filas_isin) {
      columnas <- html_elements(fila, "td") %>% html_text(trim = TRUE)
      
      if (length(columnas) >= 3) {
        hora_pacto <- columnas[1] # Ajustar índice según la columna de la tabla
        precio_limpio <- columnas[length(columnas)] # Ajustar según columna de precio
        
        trade_id <- paste0("DO9035100120_", hora_pacto, "_", precio_limpio)
        trades_vistos <- old_state[["otc_trades_vistos"]]
        if (is.null(trades_vistos)) trades_vistos <- c()

        if (!(trade_id %in% trades_vistos)) {
          msg_otc <- paste0(
            "FOP Multiplaza OTC\n",
            "Hora pacto ", hora_pacto, "\n",
            "Precio limpio ", precio_limpio
          )
          send_telegram(msg_otc)
          
          trades_vistos <- c(trades_vistos, trade_id)
          new_state[["otc_trades_vistos"]] <- tail(trades_vistos, 50) # Guardar últimos 50
        }
      }
    }
  }

  # Guardar estado actualizado
  write_json(new_state, state_file, auto_unbox = TRUE, pretty = TRUE)

}, error = function(e) {
  send_telegram(paste0("Error en script_fondos.R: ", e$message))
  stop(e)
})
