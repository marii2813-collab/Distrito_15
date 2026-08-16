############################################################
# DISTRITO ELECTORAL 15 - ALCALDÍA IZTACALCO
# Aplicación Shiny + Leaflet
# Compatible con Posit Connect Cloud
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
# 2. CARGA DE SHAPEFILES
# ==========================================================

# ----------------------------------------------------------
# Shapefile Distrito Electoral 15
# ----------------------------------------------------------

shpFILES_dist15 <- list.files(
  path = here("data", "Distrito electoral 15"),
  pattern = "\\.shp$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(shpFILES_dist15) == 0) {
  stop("No se encontró el shapefile del Distrito Electoral 15.")
}

shp_dist15 <- st_read(
  shpFILES_dist15[1],
  quiet = TRUE
)


# ----------------------------------------------------------
# Shapefile Secciones Electorales
# ----------------------------------------------------------

shpFILES_sec_elec <- list.files(
  path = here("data", "Secciones electorales"),
  pattern = "\\.shp$",
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(shpFILES_sec_elec) == 0) {
  stop("No se encontró el shapefile de Secciones Electorales.")
}

shp_sec_elec <- st_read(
  shpFILES_sec_elec[1],
  quiet = TRUE
)


# ==========================================================
# 3. PREPARACIÓN DE LOS SHAPEFILES
# ==========================================================

# El mapa de Leaflet necesita WGS84
shp_dist15 <- shp_dist15 %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(4326)


shp_sec_elec <- shp_sec_elec %>%
  st_zm(drop = TRUE, what = "ZM") %>%
  st_transform(4326) %>%
  mutate(
    SECCION = trimws(as.character(SECCION))
  )


# ==========================================================
# 4. INTERFAZ SHINY
# ==========================================================

ui <- fluidPage(

  # --------------------------------------------------------
  # TÍTULO
  # --------------------------------------------------------

  titlePanel(
    "Distrito Electoral 15 - Alcaldía Iztacalco"
  ),


  sidebarLayout(

    # ======================================================
    # PANEL LATERAL
    # ======================================================

    sidebarPanel(

      h4("Carga de información"),

      fileInput(
        inputId = "archivo_csv",
        label = "Selecciona tu archivo CSV:",
        accept = c(
          ".csv",
          "text/csv"
        )
      ),

      helpText(
        "Columnas requeridas:"
      ),

      tags$ul(
        tags$li("Sección electoral"),
        tags$li("DIFERENCIA %"),
        tags$li("VISITADO"),
        tags$li("NO DE FIRMAS")
      ),

      actionButton(
        inputId = "procesar",
        label = "Procesar CSV",
        icon = icon("refresh"),
        class = "btn-primary"
      ),

      hr(),

      # ----------------------------------------------------
      # ESTADO
      # ----------------------------------------------------

      h4("Estado"),

      textOutput("estado_csv"),

      hr(),

      # ----------------------------------------------------
      # RESUMEN
      # ----------------------------------------------------

      h4("Resumen"),

      tableOutput("resumen"),

      hr(),

      # ----------------------------------------------------
      # SIMBOLOGÍA
      # ----------------------------------------------------

      h4("Simbología"),

      tags$div(
        tags$span(
          style = "color:green; font-size:20px;",
          "●"
        ),
        " Visitado"
      ),

      tags$div(
        tags$span(
          style = "color:red; font-size:20px;",
          "●"
        ),
        " Diferencia ≤ -10"
      ),

      tags$div(
        tags$span(
          style = "color:blue; font-size:20px;",
          "●"
        ),
        " Diferencia ≥ 10"
      ),

      tags$div(
        tags$span(
          style = "color:gray; font-size:20px;",
          "●"
        ),
        " Empate / diferencia entre -10 y 10"
      ),

      width = 3
    ),


    # ======================================================
    # MAPA
    # ======================================================

    mainPanel(

      leafletOutput(
        outputId = "mapa",
        height = "750px"
      )

    )
  )
)


# ==========================================================
# 5. SERVIDOR
# ==========================================================

server <- function(input, output, session) {


  # ========================================================
  # 5.1 LECTURA DEL CSV
  # ========================================================

  datos_csv <- eventReactive(
    input$procesar,
    {

      req(input$archivo_csv)


      datos <- tryCatch(

        read_csv(
          file = input$archivo_csv$datapath,
          locale = locale(
            encoding = "latin1"
          ),
          show_col_types = FALSE
        ),

        error = function(e) {

          showNotification(
            paste(
              "Error al leer el CSV:",
              e$message
            ),
            type = "error",
            duration = 10
          )

          return(NULL)
        }
      )


      req(!is.null(datos))


      # ----------------------------------------------------
      # COLUMNAS NECESARIAS
      # ----------------------------------------------------

      columnas_necesarias <- c(
        "Sección electoral",
        "DIFERENCIA %",
        "VISITADO",
        "NO DE FIRMAS"
      )


      columnas_faltantes <- setdiff(
        columnas_necesarias,
        names(datos)
      )


      if (length(columnas_faltantes) > 0) {

        showNotification(
          paste(
            "El CSV no contiene:",
            paste(
              columnas_faltantes,
              collapse = ", "
            )
          ),
          type = "error",
          duration = 10
        )

        return(NULL)
      }


      # ----------------------------------------------------
      # LIMPIEZA
      # ----------------------------------------------------

      datos <- datos %>%

        mutate(

          `Sección electoral` =
            trimws(
              as.character(
                `Sección electoral`
              )
            ),

          # parse_number permite valores como:
          # 10
          # 10%
          # -10
          # -10%
          `DIFERENCIA %` =
            suppressWarnings(
              parse_number(
                as.character(
                  `DIFERENCIA %`
                )
              )
            ),

          VISITADO =
            toupper(
              trimws(
                as.character(
                  VISITADO
                )
              )
            ),

          `NO DE FIRMAS` =
            suppressWarnings(
              parse_number(
                as.character(
                  `NO DE FIRMAS`
                )
              )
            )
        ) %>%

        # Evita duplicar secciones
        distinct(
          `Sección electoral`,
          .keep_all = TRUE
        )


      datos
    }
  )


  # ========================================================
  # 5.2 UNIR CSV CON SECCIONES
  # ========================================================

  datos_mapa <- reactive({

    req(datos_csv())

    datos <- datos_csv()


    mapa <- shp_sec_elec %>%

      left_join(
        datos,
        by = c(
          "SECCION" =
            "Sección electoral"
        )
      )


    # ======================================================
    # CLASIFICACIÓN
    # ======================================================

    mapa <- mapa %>%

      mutate(

        categoria_color = case_when(

          # ------------------------------------------------
          # VERDE = VISITADO
          # ------------------------------------------------

          !is.na(VISITADO) &
            VISITADO %in%
            c(
              "SI",
              "SÍ",
              "YES",
              "TRUE",
              "1"
            ) ~ "green",


          # ------------------------------------------------
          # ROJO = DIFERENCIA <= -10
          # ------------------------------------------------

          !is.na(`DIFERENCIA %`) &
            `DIFERENCIA %` <= -10 ~
            "red",


          # ------------------------------------------------
          # AZUL = DIFERENCIA >= 10
          # ------------------------------------------------

          !is.na(`DIFERENCIA %`) &
            `DIFERENCIA %` >= 10 ~
            "blue",


          # ------------------------------------------------
          # GRIS = EMPATE / INTERMEDIO
          # ------------------------------------------------

          !is.na(`DIFERENCIA %`) &
            `DIFERENCIA %` > -10 &
            `DIFERENCIA %` < 10 ~
            "gray",


          # ------------------------------------------------
          # SIN INFORMACIÓN
          # ------------------------------------------------

          TRUE ~ "gray"
        )
      )


    mapa
  })


  # ========================================================
  # 5.3 ESTADO DEL CSV
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


    "✓ CSV procesado correctamente"
  })


  # ========================================================
  # 5.4 RESUMEN
  # ========================================================

  output$resumen <- renderTable({

    req(datos_mapa())


    mapa <- datos_mapa()


    # ------------------------------------------------------
    # TOTAL DE SECCIONES
    # ------------------------------------------------------

    total_secciones <- nrow(mapa)


    # ------------------------------------------------------
    # VISITADAS
    # ------------------------------------------------------

    visitadas <- sum(
      !is.na(mapa$VISITADO) &
        mapa$VISITADO %in%
        c(
          "SI",
          "SÍ",
          "YES",
          "TRUE",
          "1"
        )
    )


    # ------------------------------------------------------
    # FIRMAS
    # ------------------------------------------------------

    firmas_totales <- sum(
      mapa$`NO DE FIRMAS`,
      na.rm = TRUE
    )


    # ------------------------------------------------------
    # FIRMAS EN SECCIONES VISITADAS
    # ------------------------------------------------------

    firmas_visitadas <- sum(
      mapa$`NO DE FIRMAS`[
        !is.na(mapa$VISITADO) &
          mapa$VISITADO %in%
          c(
            "SI",
            "SÍ",
            "YES",
            "TRUE",
            "1"
          )
      ],
      na.rm = TRUE
    )


    # ------------------------------------------------------
    # ROJAS
    # ------------------------------------------------------

    rojas <- sum(
      mapa$categoria_color == "red",
      na.rm = TRUE
    )


    # ------------------------------------------------------
    # AZULES
    # ------------------------------------------------------

    azules <- sum(
      mapa$categoria_color == "blue",
      na.rm = TRUE
    )


    # ------------------------------------------------------
    # GRISES
    # ------------------------------------------------------

    grises <- sum(
      mapa$categoria_color == "gray",
      na.rm = TRUE
    )


    # ------------------------------------------------------
    # PENDIENTES
    # ------------------------------------------------------

    pendientes <- total_secciones - visitadas


    # ------------------------------------------------------
    # TABLA
    # ------------------------------------------------------

    data.frame(

      Indicador = c(
        "Total de secciones",
        "Secciones visitadas",
        "Secciones pendientes",
        "Diferencia ≤ -10",
        "Diferencia ≥ 10",
        "Empate / intermedio",
        "Firmas totales",
        "Firmas en visitadas"
      ),

      Total = c(
        total_secciones,
        visitadas,
        pendientes,
        rojas,
        azules,
        grises,
        firmas_totales,
        firmas_visitadas
      ),

      check.names = FALSE
    )
  })


  # ========================================================
  # 5.5 MAPA
  # ========================================================

  output$mapa <- renderLeaflet({

    # ------------------------------------------------------
    # CENTRO DEL MAPA
    # ------------------------------------------------------

    centro <- st_centroid(
      st_union(
        shp_dist15
      )
    )


    coordenadas <- st_coordinates(
      centro
    )


    # ------------------------------------------------------
    # MAPA BASE
    # ------------------------------------------------------

    mapa_base <- leaflet(
      options = leafletOptions(
        minZoom = 10,
        maxZoom = 19
      )
    ) %>%

      addProviderTiles(
        providers$CartoDB.Positron,
        group = "Mapa"
      ) %>%

      setView(
        lng = coordenadas[1],
        lat = coordenadas[2],
        zoom = 12
      )


    # ------------------------------------------------------
    # DISTRITO ELECTORAL
    # ------------------------------------------------------

    mapa_base %>%

      addPolygons(

        data = shp_dist15,

        fill = FALSE,

        color = "black",

        weight = 3,

        opacity = 1,

        group = "Distrito Electoral 15"
      ) %>%

      addLayersControl(

        baseGroups = c(
          "Mapa"
        ),

        overlayGroups = c(
          "Distrito Electoral 15",
          "Secciones"
        ),

        options = layersControlOptions(
          collapsed = FALSE
        )
      )
  })


  # ========================================================
  # 5.6 ACTUALIZACIÓN DEL MAPA
  # ========================================================

  observe({

    req(datos_mapa())


    mapa <- datos_mapa()


    # ------------------------------------------------------
    # PALETA
    # ------------------------------------------------------

    pal <- colorFactor(

      palette = c(
        "green",
        "red",
        "blue",
        "gray"
      ),

      domain = c(
        "green",
        "red",
        "blue",
        "gray"
      )
    )


    # ------------------------------------------------------
    # POPUP
    # ------------------------------------------------------

    popup <- paste0(

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
        is.na(
          mapa$VISITADO
        ),
        "Sin información",
        mapa$VISITADO
      ),

      "<br><b>Número de firmas:</b> ",
      ifelse(
        is.na(
          mapa$`NO DE FIRMAS`
        ),
        "Sin información",
        mapa$`NO DE FIRMAS`
      ),

      "<br><b>Categoría:</b> ",

      case_when(

        mapa$categoria_color ==
          "green" ~
          "Visitado",

        mapa$categoria_color ==
          "red" ~
          "Diferencia ≤ -10",

        mapa$categoria_color ==
          "blue" ~
          "Diferencia ≥ 10",

        TRUE ~
          "Empate / intermedio"
      ),

      "</div>"
    )


    # ------------------------------------------------------
    # ACTUALIZAR MAPA
    # ------------------------------------------------------

    leafletProxy(
      "mapa"
    ) %>%

      clearGroup(
        "Secciones"
      ) %>%

      addPolygons(

        data = mapa,

        fillColor =
          pal(
            mapa$categoria_color
          ),

        fillOpacity = 0.55,

        color = "white",

        weight = 1,

        opacity = 1,

        popup = popup,

        highlightOptions =
          highlightOptions(
            weight = 3,
            color = "black",
            fillOpacity = 0.75,
            bringToFront = TRUE
          ),

        group = "Secciones"
      )
  })
}


# ==========================================================
# 6. EJECUTAR APLICACIÓN
# ==========================================================

shinyApp(
  ui = ui,
  server = server
)
