############################################################
# DISTRITO ELECTORAL 15 - ALCALDÍA IZTACALCO
############################################################

# ==========================================================
# 1. PAQUETES
# ==========================================================

library(shiny)
library(tmap)
library(sf)
library(readr)
library(dplyr)
library(here)


# ==========================================================
# 2. CARGAR SHAPEFILE DEL DISTRITO
# ==========================================================

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


# ==========================================================
# 3. CARGAR SHAPEFILE DE SECCIONES
# ==========================================================

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
# 4. PREPARAR SHAPEFILE
# ==========================================================

shp_sec_elec <- shp_sec_elec %>%
  mutate(
    SECCION = trimws(
      as.character(SECCION)
    )
  )


# ==========================================================
# 5. MODO INTERACTIVO TMAP
# ==========================================================

# IMPORTANTE:
# Se ejecuta solamente una vez.
# NO ponerlo dentro de server().

tmap_mode("view")


# ==========================================================
# 6. INTERFAZ
# ==========================================================

ui <- fluidPage(

  titlePanel(
    "Distrito Electoral 15 - Alcaldía Iztacalco"
  ),

  sidebarLayout(

    sidebarPanel(

      # ----------------------------------------------------
      # CARGA DEL CSV
      # ----------------------------------------------------

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

      textOutput(
        "estado_csv"
      ),

      hr(),

      # ----------------------------------------------------
      # RESUMEN
      # ----------------------------------------------------

      h4("Resumen"),

      tableOutput(
        "resumen"
      ),

      hr(),

      # ----------------------------------------------------
      # FIRMAS
      # ----------------------------------------------------

      h4("Firmas"),

      textOutput(
        "total_firmas"
      ),

      textOutput(
        "firmas_visitadas"
      ),

      hr(),

      # ----------------------------------------------------
      # SIMBOLOGÍA
      # ----------------------------------------------------

      h4("Simbología"),

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
          style = "color:green; font-size:20px;",
          "●"
        ),
        " Visitado"
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

    # ------------------------------------------------------
    # MAPA
    # ------------------------------------------------------

    mainPanel(

      tmapOutput(
        "mapa",
        height = "750px"
      )
    )
  )
)


# ==========================================================
# 7. SERVIDOR
# ==========================================================

server <- function(input, output, session) {


  # ========================================================
  # LEER CSV
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


      # ====================================================
      # VERIFICAR COLUMNAS
      # ====================================================

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
            "Faltan las siguientes columnas:",
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


      # ====================================================
      # PREPARAR CSV
      # ====================================================

      datos <- datos %>%

        mutate(

          `Sección electoral` =
            trimws(
              as.character(
                `Sección electoral`
              )
            ),

          `DIFERENCIA %` =
            suppressWarnings(
              as.numeric(
                `DIFERENCIA %`
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
              as.numeric(
                `NO DE FIRMAS`
              )
            )
        ) %>%

        distinct(
          `Sección electoral`,
          .keep_all = TRUE
        )


      datos
    }
  )


  # ========================================================
  # ESTADO DEL CSV
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
        "Presiona 'Procesar CSV'."
      )
    }


    paste(
      "✓ CSV procesado:",
      input$archivo_csv$name
    )
  })


  # ========================================================
  # UNIR CSV + SHAPEFILE
  # ========================================================

  datos_mapa <- reactive({

    req(
      datos_csv()
    )


    # IMPORTANTE:
    # shp_sec_elec es el objeto SF.
    # Al hacer left_join() sobre él,
    # se conserva la geometría.

    mapa <- shp_sec_elec %>%

      left_join(

        datos_csv(),

        by = c(
          "SECCION" =
            "Sección electoral"
        )
      ) %>%

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
              "TRUE"
            ) ~
            "green",


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

          TRUE ~
            "gray"
        )
      )


    mapa
  })


  # ========================================================
  # RESUMEN
  # ========================================================

  output$resumen <- renderTable({

    req(
      datos_mapa()
    )


    datos <- datos_mapa() %>%
      st_drop_geometry()


    data.frame(

      Categoría = c(
        "Total de secciones",
        "Visitadas",
        "Diferencia ≤ -10",
        "Diferencia ≥ 10",
        "Empate / intermedio"
      ),

      Total = c(

        nrow(datos),

        sum(
          datos$categoria_color == "green",
          na.rm = TRUE
        ),

        sum(
          datos$categoria_color == "red",
          na.rm = TRUE
        ),

        sum(
          datos$categoria_color == "blue",
          na.rm = TRUE
        ),

        sum(
          datos$categoria_color == "gray",
          na.rm = TRUE
        )
      )
    )
  })


  # ========================================================
  # TOTAL DE FIRMAS
  # ========================================================

  output$total_firmas <- renderText({

    req(
      datos_mapa()
    )


    total <- datos_mapa() %>%

      st_drop_geometry() %>%

      summarise(
        total = sum(
          `NO DE FIRMAS`,
          na.rm = TRUE
        )
      ) %>%

      pull(total)


    paste(
      "Total de firmas:",
      format(
        total,
        big.mark = ",",
        scientific = FALSE
      )
    )
  })


  # ========================================================
  # FIRMAS EN SECCIONES VISITADAS
  # ========================================================

  output$firmas_visitadas <- renderText({

    req(
      datos_mapa()
    )


    total <- datos_mapa() %>%

      st_drop_geometry() %>%

      filter(
        categoria_color == "green"
      ) %>%

      summarise(
        total = sum(
          `NO DE FIRMAS`,
          na.rm = TRUE
        )
      ) %>%

      pull(total)


    paste(
      "Firmas en secciones visitadas:",
      format(
        total,
        big.mark = ",",
        scientific = FALSE
      )
    )
  })


  # ========================================================
  # MAPA
  # ========================================================

  output$mapa <- renderTmap({

    # ------------------------------------------------------
    # ANTES DE CARGAR CSV
    # ------------------------------------------------------

    if (
      is.null(
        datos_csv()
      )
    ) {

      return(

        tm_shape(
          shp_dist15
        ) +

          tm_borders(
            col = "black",
            lwd = 2
          ) +

          tm_title(
            "Distrito Electoral 15 - Alcaldía Iztacalco"
          )
      )
    }


    # ------------------------------------------------------
    # MAPA CON DATOS
    # ------------------------------------------------------

    tm_shape(
      datos_mapa()
    ) +

      tm_polygons(

        fill =
          "categoria_color",

        fill.scale =
          tm_scale_categorical(

            values = c(
              "red" = "red",
              "blue" = "blue",
              "green" = "green",
              "gray" = "gray"
            )
        ),

        fill_alpha = 0.5,

        col = "white",

        lwd = 0.5,

        popup =
          tm_popup(

            vars = c(

              "Sección electoral" =
                "SECCION",

              "Diferencia %" =
                "DIFERENCIA %",

              "Estatus" =
                "VISITADO",

              "Número de firmas" =
                "NO DE FIRMAS"
            )
          )
      ) +

      # ----------------------------------------------------
      # LÍMITE DEL DISTRITO
      # ----------------------------------------------------

      tm_shape(
        shp_dist15
      ) +

      tm_borders(
        col = "black",
        lwd = 2
      ) +

      # ----------------------------------------------------
      # TÍTULO
      # ----------------------------------------------------

      tm_title(
        "Distrito Electoral 15 - Alcaldía Iztacalco"
      )
  })
}


# ==========================================================
# 8. EJECUTAR APLICACIÓN
# ==========================================================

shinyApp(
  ui = ui,
  server = server
)