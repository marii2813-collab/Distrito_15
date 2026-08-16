############################################################
# DISTRITO ELECTORAL 15 - ALCALDÍA IZTACALCO
#
# SHINY + LEAFLET
# SIN TMAP
# SIN TERRA
#
# REGLAS DE COLOR:
#
# DIFERENCIA <= -10       = ROJO
# -10 < DIFERENCIA < 10   = GRIS
# DIFERENCIA >= 10        = AZUL
#
# VISITADO = SI            = VERDE
#
# VISITADO TIENE PRIORIDAD
# SOBRE LA DIFERENCIA.
############################################################


# ==========================================================
# 1. PAQUETES
# ==========================================================

library(shiny)
library(leaflet)
library(sf)
library(readr)
library(dplyr)
library(here)


# ==========================================================
# 2. COLORES DEFINITIVOS
# ==========================================================

COLOR_ROJO  <- "#E31A1C"
COLOR_GRIS  <- "#808080"
COLOR_AZUL  <- "#1976D2"
COLOR_VERDE <- "#2E7D32"


# ==========================================================
# 3. CARGAR SHAPEFILE DEL DISTRITO
# ==========================================================

shpFILES_dist15 <- list.files(
  path = here(
    "data",
    "Distrito electoral 15"
  ),
  pattern = "\\.shp$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(shpFILES_dist15) == 0) {
  
  stop(
    "No se encontró el shapefile del Distrito Electoral 15."
  )
  
}

shp_dist15 <- st_read(
  shpFILES_dist15[1],
  quiet = TRUE
)


# ==========================================================
# 4. CARGAR SHAPEFILE DE SECCIONES
# ==========================================================

shpFILES_sec_elec <- list.files(
  path = here(
    "data",
    "Secciones electorales"
  ),
  pattern = "\\.shp$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(shpFILES_sec_elec) == 0) {
  
  stop(
    "No se encontró el shapefile de Secciones Electorales."
  )
  
}

shp_sec_elec <- st_read(
  shpFILES_sec_elec[1],
  quiet = TRUE
)


# ==========================================================
# 5. PREPARAR SHAPEFILES
# ==========================================================

shp_dist15 <- shp_dist15 %>%
  
  st_zm(
    drop = TRUE,
    what = "ZM"
  ) %>%
  
  st_transform(4326)


shp_sec_elec <- shp_sec_elec %>%
  
  st_zm(
    drop = TRUE,
    what = "ZM"
  ) %>%
  
  st_transform(4326) %>%
  
  select(
    SECCION,
    geometry
  ) %>%
  
  mutate(
    
    SECCION =
      trimws(
        as.character(SECCION)
      )
    
  )


# ==========================================================
# 6. INTERFAZ
# ==========================================================

ui <- fluidPage(
  
  titlePanel(
    "Distrito Electoral 15 - Alcaldía Iztacalco"
  ),
  
  sidebarLayout(
    
    # ======================================================
    # PANEL LATERAL
    # ======================================================
    
    sidebarPanel(
      
      h4("Archivo de datos"),
      
      fileInput(
        
        inputId =
          "archivo_csv",
        
        label =
          "Selecciona tu archivo CSV:",
        
        accept =
          c(
            ".csv",
            "text/csv"
          )
        
      ),
      
      actionButton(
        
        inputId =
          "procesar",
        
        label =
          "Procesar CSV",
        
        icon =
          icon("refresh"),
        
        class =
          "btn-primary"
        
      ),
      
      hr(),
      
      h4("Estado"),
      
      textOutput(
        "estado_csv"
      ),
      
      hr(),
      
      h4("Resumen"),
      
      tableOutput(
        "resumen"
      ),
      
      hr(),
      
      h4("Simbología"),
      
      tags$div(
        
        tags$span(
          
          style =
            paste0(
              "color:",
              COLOR_ROJO,
              "; font-size:22px;"
            ),
          
          "●"
          
        ),
        
        " Diferencia ≤ -10"
        
      ),
      
      tags$div(
        
        tags$span(
          
          style =
            paste0(
              "color:",
              COLOR_GRIS,
              "; font-size:22px;"
            ),
          
          "●"
          
        ),
        
        " Diferencia entre -10 y 10"
        
      ),
      
      tags$div(
        
        tags$span(
          
          style =
            paste0(
              "color:",
              COLOR_AZUL,
              "; font-size:22px;"
            ),
          
          "●"
          
        ),
        
        " Diferencia ≥ 10"
        
      ),
      
      tags$div(
        
        tags$span(
          
          style =
            paste0(
              "color:",
              COLOR_VERDE,
              "; font-size:22px;"
            ),
          
          "●"
          
        ),
        
        " Visitada"
        
      ),
      
      br(),
      
      helpText(
        
        "Una sección solo se muestra en verde cuando VISITADO = SI."
        
      ),
      
      width = 3
      
    ),
    
    
    # ======================================================
    # MAPA
    # ======================================================
    
    mainPanel(
      
      leafletOutput(
        
        outputId =
          "mapa",
        
        height =
          "750px"
        
      )
      
    )
    
  )
  
)


# ==========================================================
# 7. SERVIDOR
# ==========================================================

server <- function(
    input,
    output,
    session
) {
  
  
  # ========================================================
  # 7.1 LEER CSV
  # ========================================================
  
  datos_csv <- eventReactive(
    
    input$procesar,
    
    {
      
      req(
        input$archivo_csv
      )
      
      
      datos <- tryCatch(
        
        read_csv(
          
          file =
            input$archivo_csv$datapath,
          
          locale =
            locale(
              encoding = "latin1"
            ),
          
          show_col_types =
            FALSE
          
        ),
        
        error = function(e) {
          
          showNotification(
            
            paste(
              "Error al leer el CSV:",
              e$message
            ),
            
            type =
              "error",
            
            duration =
              10
            
          )
          
          return(NULL)
          
        }
        
      )
      
      
      req(
        !is.null(datos)
      )
      
      
      # ====================================================
      # COLUMNAS NECESARIAS
      # ====================================================
      
      columnas_necesarias <- c(
        
        "Sección electoral",
        
        "DIFERENCIA %",
        
        "VISITADO",
        
        "NO DE FIRMAS"
        
      )
      
      
      columnas_faltantes <-
        
        setdiff(
          
          columnas_necesarias,
          
          names(datos)
          
        )
      
      
      if (
        length(columnas_faltantes) > 0
      ) {
        
        showNotification(
          
          paste(
            
            "Faltan estas columnas:",
            
            paste(
              
              columnas_faltantes,
              
              collapse = ", "
              
            )
            
          ),
          
          type =
            "error",
          
          duration =
            10
          
        )
        
        
        return(NULL)
        
      }
      
      
      # ====================================================
      # LIMPIAR DATOS
      # ====================================================
      
      datos <- datos %>%
        
        mutate(
          
          # -----------------------------------------------
          # SECCIÓN
          # -----------------------------------------------
          
          `Sección electoral` =
            
            trimws(
              
              as.character(
                
                `Sección electoral`
                
              )
              
            ),
          
          
          # -----------------------------------------------
          # DIFERENCIA
          # -----------------------------------------------
          
          `DIFERENCIA %` =
            
            suppressWarnings(
              
              parse_number(
                
                as.character(
                  
                  `DIFERENCIA %`
                  
                )
                
              )
              
            ),
          
          
          # -----------------------------------------------
          # VISITADO
          # -----------------------------------------------
          
          VISITADO =
            
            toupper(
              
              trimws(
                
                as.character(
                  
                  VISITADO
                  
                )
                
              )
              
            ),
          
          
          # -----------------------------------------------
          # FIRMAS
          # -----------------------------------------------
          
          `NO DE FIRMAS` =
            
            suppressWarnings(
              
              parse_number(
                
                as.character(
                  
                  `NO DE FIRMAS`
                  
                )
                
              )
              
            )
          
        )
      
      
      datos
      
    }
    
  )
  
  
  # ========================================================
  # 7.2 UNIR CSV CON SHAPEFILE
  # ========================================================
  
  datos_mapa <- reactive({
    
    req(
      datos_csv()
    )
    
    
    datos <-
      datos_csv()
    
    
    mapa <-
      
      shp_sec_elec %>%
      
      left_join(
        
        datos,
        
        by = c(
          
          "SECCION" =
            "Sección electoral"
          
        )
        
      )
    
    
    # ======================================================
    # 7.3 DETERMINAR VISITADO
    # ======================================================
    
    mapa <- mapa %>%
      
      mutate(
        
        visitado_real =
          
          !is.na(VISITADO) &
          
          VISITADO %in% c(
            
            "SI",
            
            "SÍ",
            
            "YES",
            
            "TRUE",
            
            "1"
            
          )
        
      )
    
    
    # ======================================================
    # 7.4 ASIGNAR COLOR DIRECTAMENTE
    # ======================================================
    
    mapa <- mapa %>%
      
      mutate(
        
        color_seccion = case_when(
          
          # ----------------------------------------------
          # VISITADA = VERDE
          # ----------------------------------------------
          
          visitado_real == TRUE ~
            
            COLOR_VERDE,
          
          
          # ----------------------------------------------
          # DIFERENCIA <= -10 = ROJO
          # ----------------------------------------------
          
          !is.na(
            `DIFERENCIA %`
          ) &
            
            `DIFERENCIA %` <= -10 ~
            
            COLOR_ROJO,
          
          
          # ----------------------------------------------
          # DIFERENCIA >= 10 = AZUL
          # ----------------------------------------------
          
          !is.na(
            `DIFERENCIA %`
          ) &
            
            `DIFERENCIA %` >= 10 ~
            
            COLOR_AZUL,
          
          
          # ----------------------------------------------
          # ENTRE -10 Y 10 = GRIS
          # ----------------------------------------------
          
          !is.na(
            `DIFERENCIA %`
          ) &
            
            `DIFERENCIA %` > -10 &
            
            `DIFERENCIA %` < 10 ~
            
            COLOR_GRIS,
          
          
          # ----------------------------------------------
          # SIN INFORMACIÓN = GRIS
          # ----------------------------------------------
          
          TRUE ~
            
            COLOR_GRIS
          
        )
        
      )
    
    
    mapa
    
  })
  
  
  # ========================================================
  # 7.5 ESTADO
  # ========================================================
  
  output$estado_csv <- renderText({
    
    if (
      is.null(
        input$archivo_csv
      )
    ) {
      
      return(
        "No se ha seleccionado ningún CSV."
      )
      
    }
    
    
    if (
      is.null(
        datos_csv()
      )
    ) {
      
      return(
        "El archivo aún no ha sido procesado."
      )
      
    }
    
    
    mapa <-
      datos_mapa()
    
    
    visitadas <-
      
      sum(
        
        mapa$visitado_real,
        
        na.rm = TRUE
        
      )
    
    
    paste0(
      
      "✓ CSV procesado correctamente. ",
      
      "Secciones visitadas: ",
      
      visitadas
      
    )
    
  })
  
  
  # ========================================================
  # 7.6 RESUMEN
  # ========================================================
  
  output$resumen <- renderTable({
    
    req(
      datos_mapa()
    )
    
    
    mapa <-
      datos_mapa()
    
    
    total_secciones <-
      nrow(mapa)
    
    
    total_visitadas <-
      
      sum(
        
        mapa$visitado_real,
        
        na.rm = TRUE
        
      )
    
    
    total_rojas <-
      
      sum(
        
        mapa$color_seccion ==
          COLOR_ROJO,
        
        na.rm = TRUE
        
      )
    
    
    total_azules <-
      
      sum(
        
        mapa$color_seccion ==
          COLOR_AZUL,
        
        na.rm = TRUE
        
      )
    
    
    total_grises <-
      
      sum(
        
        mapa$color_seccion ==
          COLOR_GRIS,
        
        na.rm = TRUE
        
      )
    
    
    total_firmas <-
      
      sum(
        
        mapa$`NO DE FIRMAS`,
        
        na.rm = TRUE
        
      )
    
    
    firmas_visitadas <-
      
      sum(
        
        mapa$`NO DE FIRMAS`[
          mapa$visitado_real
        ],
        
        na.rm = TRUE
        
      )
    
    
    data.frame(
      
      Indicador = c(
        
        "Total de secciones",
        
        "Secciones visitadas",
        
        "Diferencia ≤ -10",
        
        "Diferencia ≥ 10",
        
        "Empate / intermedio",
        
        "Total de firmas",
        
        "Firmas en secciones visitadas"
        
      ),
      
      Total = c(
        
        total_secciones,
        
        total_visitadas,
        
        total_rojas,
        
        total_azules,
        
        total_grises,
        
        total_firmas,
        
        firmas_visitadas
        
      ),
      
      check.names =
        FALSE
      
    )
    
  })
  
  
  # ========================================================
  # 7.7 MAPA BASE
  # ========================================================
  
  output$mapa <- renderLeaflet({
    
    centro <-
      
      st_centroid(
        
        st_union(
          
          shp_dist15
          
        )
        
      )
    
    
    coordenadas <-
      
      st_coordinates(
        
        centro
        
      )
    
    
    leaflet(
      
      options =
        
        leafletOptions(
          
          minZoom =
            10,
          
          maxZoom =
            19
          
        )
      
    ) %>%
      
      addProviderTiles(
        
        providers$CartoDB.Positron,
        
        group =
          "Mapa"
        
      ) %>%
      
      setView(
        
        lng =
          coordenadas[1],
        
        lat =
          coordenadas[2],
        
        zoom =
          12
        
      ) %>%
      
      addPolygons(
        
        data =
          shp_dist15,
        
        fill =
          FALSE,
        
        color =
          "black",
        
        weight =
          3,
        
        opacity =
          1,
        
        group =
          "Distrito Electoral 15"
        
      )
    
  })
  
  
  # ========================================================
  # 7.8 DIBUJAR SECCIONES
  # ========================================================
  
  observe({
    
    req(
      datos_mapa()
    )
    
    
    mapa <-
      datos_mapa()
    
    
    # ======================================================
    # POPUPS
    # ======================================================
    
    popup <-
      
      paste0(
        
        "<div style='font-size:14px;'>",
        
        "<b>Sección electoral:</b> ",
        
        mapa$SECCION,
        
        
        "<br><b>Diferencia %:</b> ",
        
        ifelse(
          
          is.na(
            mapa$`DIFERENCIA %`
          ),
          
          "Sin información",
          
          paste0(
            
            mapa$`DIFERENCIA %`,
            
            "%"
            
          )
          
        ),
        
        
        "<br><b>Visitado:</b> ",
        
        ifelse(
          
          mapa$visitado_real,
          
          "SI",
          
          "NO"
          
        ),
        
        
        "<br><b>Número de firmas:</b> ",
        
        ifelse(
          
          is.na(
            mapa$`NO DE FIRMAS`
          ),
          
          "0",
          
          mapa$`NO DE FIRMAS`
          
        ),
        
        
        "<br><b>Estatus:</b> ",
        
        case_when(
          
          mapa$visitado_real == TRUE ~
            
            "VISITADA",
          
          !is.na(
            mapa$`DIFERENCIA %`
          ) &
            
            mapa$`DIFERENCIA %` <= -10 ~
            
            "DIFERENCIA ≤ -10",
          
          !is.na(
            mapa$`DIFERENCIA %`
          ) &
            
            mapa$`DIFERENCIA %` >= 10 ~
            
            "DIFERENCIA ≥ 10",
          
          TRUE ~
            
            "EMPATE / INTERMEDIO"
          
        ),
        
        "</div>"
        
      )
    
    
    # ======================================================
    # DIBUJAR SECCIONES
    #
    # CONTORNO DISCRETO
    # ======================================================
    
    leafletProxy(
      "mapa"
    ) %>%
      
      clearGroup(
        "Secciones"
      ) %>%
      
      addPolygons(
        
        data =
          mapa,
        
        # --------------------------------------------------
        # COLOR DIRECTO
        # --------------------------------------------------
        
        fillColor =
          mapa$color_seccion,
        
        fillOpacity =
          0.60,
        
        # --------------------------------------------------
        # CONTORNO DE SECCIONES
        # MÁS FINO Y DISCRETO
        # --------------------------------------------------
        
        color =
          "#D0D0D0",
        
        weight =
          0.5,
        
        opacity =
          0.7,
        
        popup =
          popup,
        
        # --------------------------------------------------
        # AL PASAR EL CURSOR
        # --------------------------------------------------
        
        highlightOptions =
          
          highlightOptions(
            
            weight =
              2,
            
            color =
              "black",
            
            fillOpacity =
              0.80,
            
            bringToFront =
              TRUE
            
          ),
        
        group =
          "Secciones"
        
      )
    
  })
  
}


# ==========================================================
# 8. EJECUTAR APLICACIÓN
# ==========================================================

shinyApp(
  
  ui =
    ui,
  
  server =
    server
  
)