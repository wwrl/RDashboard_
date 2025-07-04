library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(forcats)
library(ggiraph)
library(plotly)
library(shinydashboard)
library(leaflet)
library(purrr)
library(sf)
library(corrplot)
library(ggcorrplot)
library(factoextra)

load("C:/Users/wikto/Downloads/przejazdy.RData")
load("C:/Users/wikto/Downloads/punkty_pomiarowe.RData")

dni_pomiarowe <- przejazdy %>%
  group_by(Stacja) %>%
  summarise(Data = n_distinct(Data))

dni_pomiarowe <- dni_pomiarowe %>%
  mutate(Stacja = fct_reorder(Stacja, Data))

przejazdy$Ciśnienie_morze[przejazdy$Ciśnienie_morze == 0] <- NA
przejazdy$Ciśnienie_woda[przejazdy$Ciśnienie_woda == 0] <- NA
przejazdy$Wilgotność[przejazdy$Wilgotność == 0] <- NA

names(punkty)[names(punkty) == "stacja"] <- "Stacja"
df_merged <- merge(przejazdy,punkty, by = "Stacja")

df_merged$Lon <- sapply(df_merged$geometry, function(x) x[1])
df_merged$Lat <- sapply(df_merged$geometry, function(x) x[2])
df_merged <- df_merged %>% select(-geometry)

  corr_data <- df_merged %>%
    select(Temperatura, Opady_dzień, Opady_noc,
           Ciśnienie_stacja, Ciśnienie_morze,
           Wiatr, Wilgotność)
  
zmienne_pogodowe <- c("Temperatura", "Wilgotność", "Wiatr", "Ciśnienie_morze", "Ciśnienie_stacja", "Opady_dzień", "Opady_noc")
pary <- combn(zmienne_pogodowe, 2, simplify = FALSE)
etykiety_par <- sapply(pary, function(x) paste(x[1], "vs", x[2]))
names(pary) <- etykiety_par
  
###### Wykres zadania 1 ######
Wykres1 <- plot_ly(
  data = dni_pomiarowe,
  x = ~Data,
  y = ~Stacja,
  type = "bar",
  orientation = "h",
  text = ~paste0("<b>Stacja:</b> ", Stacja, "<br><b>Liczba dni:</b> ", Data),
  hoverinfo = "text",
  textposition = "none",
  marker = list(
    color = "#4FC3F7",
    line = list(color = "#0288D1", width = 1))) %>%
  
  layout(
    xaxis = list(
      title = "Liczba dni",
      titlefont = list(size = 16, family = "Arial", color = "#333333"),
      tickfont = list(size = 13, family = "Arial", color = "#333333"),
      gridcolor = "#eeeeee"),
    
    yaxis = list(
      title = "",
      tickfont = list(size = 13, family = "Arial", color = "#333333")),
    plot_bgcolor = "#ffffff",
    paper_bgcolor = "#ffffff",
    hoverlabel = list(
      bgcolor = "#ffffff",
      font = list(color = "#000000", size = 13, family = "Arial"),
      bordercolor = "#cccccc")) %>%
  config(displayModeBar = FALSE) 

###### UI Dashboardu ######
ui <- dashboardPage(
  dashboardHeader(title = ""),
  
###### Panel Boczny ######
   dashboardSidebar(
    sidebarMenu(
      menuItem("Zadanie 1", tabName = "task1", icon = icon("")),
      menuItem("Zadanie 2", tabName = "task2", icon = icon("")),
      menuItem("Zadanie 3", tabName = "task3", icon = icon("")),
      menuItem("Zadanie 4 i 5", tabName = "task45", icon = icon("")),
      menuItem("Zadanie 6 i 7", tabName = "task67", icon = icon("")),
      menuItem("Zadanie 8", tabName = "map", icon = icon("")),
      menuItem("Zadanie 9", tabName = "task9", icon = icon(""))
      )
    ),
  
  # Ciało aplikacji
  dashboardBody(tags$style(HTML(
      ".box .box-header .box-title { text-align: center; width: 100%; }")),
    tabItems(
      
###### Zadanie 1 Liczba dni pomiarowych w punktach ######
      tabItem(
        tabName = "task1",
        fluidRow(
          column(
            width = 12,  # Kolumna na cały ekran
            box(
              title = "Liczba dni pomiarowych w punktach", 
              status = "primary", 
              solidHeader = TRUE, 
              width = NULL, 
              height = "90vh",
              plotlyOutput("Wykres1", height = "80vh")
              )
            )
          )
        ),
      
###### Zadanie 2 Rozkład przejazdów ######
      tabItem(
        tabName = "task2",
        fluidRow(
          column(
            width = 12,
            box(
              title = "Rozkład przejazdów", 
              status = "primary", 
              solidHeader = TRUE, 
              width = NULL, 
              height = "15vh",
              selectInput("punktSelect", 
                          "Wybierz punkt:", 
                          choices = unique(przejazdy$Stacja)))
            ), 
          #histogram1
          column(width = 12,
                 box(
                   title = "Rozkład przejazdów", 
                   status = "primary", 
                   solidHeader = TRUE, 
                   width = NULL, 
                   height = "60vh",
              plotlyOutput("histogram1", height = "50vh"))
              ),
          #Boxplot
          column(width = 6, 
                 box(
                   title = "Boxplot", 
                   status = "primary", 
                   solidHeader = TRUE, 
                   width = NULL, 
                   height = "60vh",
              plotlyOutput("boxplot", height = "50vh"))),
          #Density plot
          column(width = 6, 
                box(
                  title = "Density Plot", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = NULL, 
                  height = "60vh",# Wykres na pełnej wysokości
                  plotlyOutput("densityPlot", height = "50vh")
                  )
                )
          )
        ),
  
###### Zadanie 3 Porónanie natężenia ######
      tabItem(
        tabName = "task3",
        fluidRow(
          #histogram1 1
          column(
            width = 6,  
            box(
              title = "Punkt 1:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = NULL, 
              height = "85vh",
              selectInput("punktSelect1", 
                          "Wybierz punkt:", 
                          choices = unique(przejazdy$Stacja)),
            plotlyOutput("histogram2", height = "65vh"))),
          #histogram1 2
          column(
            width = 6,
            box(
              title = "Punkt 2:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = NULL, 
              height = "85vh",
              selectInput("punktSelect2", 
                          "Wybierz punkt:", 
                          choices = unique(przejazdy$Stacja)),
            plotlyOutput("histogram12", height = "65vh")
              )
            )
          )
        ),

###### Zadanie 4 & 5 ######
      tabItem(
        tabName = "task45",
        fluidRow(
          #Punkt 1
          column(
            width = 6, 
            box(
              title = "Punkt 1:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = NULL, 
              height = "85vh",
              selectInput("punktSelect1", 
                          "Wybierz punkt:", 
                          choices = unique(przejazdy$Stacja)),
              selectInput("podzialSelect1",
                          "Wybierz podział:",
                          choices = c("Miesiąc", "Dzień tygodnia", "Typ dnia")),
              plotlyOutput("histogram14", height = "55vh")
            )),
          #Punkt 2
          column(
            width = 6,
            box(
              title = "Punkt 2:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = NULL, 
              height = "85vh",
              selectInput("punktSelect2", 
                          "Wybierz punkt:", 
                          choices = unique(przejazdy$Stacja)),
              selectInput("podzialSelect2",
                          "Wybierz punkt:",
                          choices = c("Miesiąc", "Dzień tygodnia", "Typ dnia" )),
              plotlyOutput("histogram15", height = "55vh")
              )
            )
          )
        ),

###### Zadanie 6 & 7 ######
      tabItem(
        tabName = "task67",
        fluidRow(
          #Punkt 1
          column(
            width = 6,  # Kolumna na cały ekran
            box(
              title = "Punkt 1:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = NULL, 
              height = "85vh",
              # Dodanie Select Input do wyboru punktu
              selectInput("punktSelect3", 
                          "Wybierz punkt:", 
                          choices = unique(przejazdy$Stacja)),
              selectInput("podzialSelect3",
                          "Wybierz warunki pogodowe:",
                          choices = c("Zachmurzenie", 
                                      "Wiatr",
                                      "Temperatura",
                                      "Średnie dobowe ciśnienie pary wodnej [hPa]" = "Ciśnienie_woda", 
                                      "Wilgotność", 
                                      "Średnie dobowe ciśnienie pary wodnej na poziomie stacji [hPa]" = "Ciśnienie_stacja" ,
                                      "Średnie dobowe ciśnienie pary wodnej na poziomie morza [hPa]" = "Ciśnienie_morze" , 
                                      "Suma opadu na dzień [mm]" = "Opady_dzień", 
                                      "Suma opadu noc [mm]" = "Opady_noc" )),
              plotlyOutput("histogram16", height = "55vh")
              )
            ),
          #Punkt 2
          column(
            width = 6,
            box(
              title = "Punkt 2:", 
              status = "primary", 
              solidHeader = TRUE, 
              width = NULL, 
              height = "85vh",
              # Dodanie Select Input do wyboru punktu
              selectInput("punktSelect4", 
                          "Wybierz punkt:", 
                          choices = unique(przejazdy$Stacja)),
              selectInput("podzialSelect4",
                          "Wybierz warunki pogodowe:",
                          choices = c("Zachmurzenie", 
                                      "Wiatr",
                                      "Temperatura",
                                      "Średnie dobowe ciśnienie pary wodnej [hPa]" = "Ciśnienie_woda", 
                                      "Wilgotność", 
                                      "Średnie dobowe ciśnienie pary wodnej na poziomie stacji [hPa]" = "Ciśnienie_stacja" ,
                                      "Średnie dobowe ciśnienie pary wodnej na poziomie morza [hPa]" = "Ciśnienie_morze" , 
                                      "Suma opadu na dzień [mm]" = "Opady_dzień", 
                                      "Suma opadu noc [mm]" = "Opady_noc" )),
              plotlyOutput("histogram17", height = "55vh")
              )
            )
          )
        ),

###### Zadanie 8 ######
      tabItem(
        tabName = "map",
        fluidRow(
          box(
            title = "Wybór statystyki i stacji",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            selectInput("zmienna_przejazdy", "Wybierz statystykę przejazdów:",
                        choices = c(
                          "Średnia liczba przejazdów" = "mean",
                          "Maksymalna liczba przejazdów" = "max",
                          "Minimalna liczba przejazdów" = "min")),
            ),
          box(
            title = "Mapa pomiarów",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            leafletOutput("map", height = 450)
            )
          )
        ),

###### Zadanie 9 ######
      tabItem(
        tabName = "task9",
        fluidRow(
          box(
            title = "Macierz korelacji zmiennych pogodowych",
            status = "primary", solidHeader = TRUE,
            width = 6,
            plotOutput("macierz_korelacji", height = "450px")
          ),
          
          box(
            title = "PCA – analiza głównych składowych",
            status = "primary", solidHeader = TRUE,
            width = 6,
            plotOutput("pca", height = "450px")
          )
        ),
          fluidRow(
            box(
              title = "Regresja",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              height = "520px",
              style = "margin-left: auto; margin-right: auto;",
              selectInput("paraZmienne", 
                          "Wybierz parę zmiennych:", 
                          choices = etykiety_par),
              plotOutput("regresja", height = "390px")
            ),
            box(
              title = "Sezonowość",
              status = "primary",
              solidHeader = TRUE,
              width = 6,
              height = "520px",
              style = "margin-left: auto; margin-right: auto;",
              selectInput("paraZmienne1", 
                          "Wybierz parę zmiennych:", 
                          choices = etykiety_par),
              plotOutput("sezonowosc", height = "390px")
            )
          )
        )
      )
    )
  )
        
###### server ######
server <- function(input, output) {
  
  ###### Wykres zadanie 1 ######
  output$Wykres1 <- renderPlotly({
    ggplotly(Wykres1, tooltip = "text")
  })
  
  ###### Wykresy zadanie 2 histogram1 ######
  output$histogram1 <- renderPlotly({
    p <- ggplot(dane_punkt(), aes(x = Data, y = Licznik, text = paste("Data:", Data, "<br>Licznik:", Licznik))) +
      geom_area(fill = "#4FC3F7", alpha = 0.6) +
      geom_line(color = "#0288D1") +
      labs(
        x = "Data",
        y = "Liczba przejazdów"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
    ggplotly(p, tooltip = "text") %>%  # używamy własnego opisu
      layout(
        xaxis = list(
          title = "Data",
          titlefont = list(size = 16, family = "Arial", color = "#333333"),
          tickfont = list(size = 13, family = "Arial", color = "#333333"),
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 13, family = "Arial", color = "#333333")
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hoverlabel = list(
          bgcolor = "#ffffff",
          font = list(color = "#000000", size = 13, family = "Arial"),
          bordercolor = "#cccccc"
        )
      )
  })
  
  ###### Wykresy zadanie 2 boxplot ######
  output$boxplot <- renderPlotly({
    p <- ggplot(dane_punkt(), aes(x = "", y = Licznik)) +
      geom_boxplot(fill = "#4FC3F7", color = "#0288D1") +
      labs(x = "", y = "Liczba przejazdów") +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(face = "bold",hjust = 0.5)
      )
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        xaxis = list(
          title = "",
          titlefont = list(size = 16, family = "Arial", color = "#333333"),
          tickfont = list(size = 13, family = "Arial", color = "#333333"),
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 13, family = "Arial", color = "#333333")
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hoverlabel = list(
          bgcolor = "#ffffff",
          font = list(color = "#000000", size = 13, family = "Arial"),
          bordercolor = "#cccccc"
        )
      )
  })
  
  ###### Wykresy zadanie 2 densityPlot ######
  output$densityPlot <- renderPlotly({
    p <- ggplot(dane_punkt(), aes(x = Licznik)) +
      geom_density(fill = "#4FC3F7", color = "#0288D1") +
      labs(x = "Liczba przejazdów", y = "Gęstość") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        xaxis = list(
          title = "",
          titlefont = list(size = 16, family = "Arial", color = "#333333"),
          tickfont = list(size = 13, family = "Arial", color = "#333333"),
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 13, family = "Arial", color = "#333333")
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hoverlabel = list(
          bgcolor = "#ffffff",
          font = list(color = "#000000", size = 13, family = "Arial"),
          bordercolor = "#cccccc"
        )
      )
  })
  
  # Reaktywne dane na podstawie wybranego punktu
  dane_punkt1 <- reactive({
    req(input$punktSelect1)  # Upewniamy się, że input$punkt jest dostępny
    przejazdy %>% filter(Stacja == input$punktSelect1)
  })
  # Reaktywne dane na podstawie wybranego punktu
  dane_punkt2 <- reactive({
    req(input$punktSelect2)  # Upewniamy się, że input$punkt jest dostępny
    przejazdy %>% filter(Stacja == input$punktSelect2)
  })
  # Reaktywne dane na podstawie wybranego punktu
  dane_punkt <- reactive({
    req(input$punktSelect)  # Upewniamy się, że input$punkt jest dostępny
    przejazdy %>% filter(Stacja == input$punktSelect)
  })
  
  ###### Wykresy zadanie 3 ######
  output$histogram2 <- renderPlotly({
    p <- ggplot(dane_punkt1(), aes(x = Data, y = Licznik)) +
      geom_area(fill = "#4FC3F7", alpha = 0.6) +
      geom_line(color = "#0288D1") +
      labs(
        title = paste(input$punktSelect1),
        x = "Data",
        y = "Liczba przejazdów"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        xaxis = list(
          title = "Data",
          titlefont = list(size = 16, family = "Arial", color = "#333333"),
          tickfont = list(size = 13, family = "Arial", color = "#333333"),
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 13, family = "Arial", color = "#333333")
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hoverlabel = list(
          bgcolor = "#ffffff",
          font = list(color = "#000000", size = 13, family = "Arial"),
          bordercolor = "#cccccc"
        )
      )
  })
  
  ###### Wykresy zadanie 3 v2 ######
  output$histogram12 <- renderPlotly({
    p <- ggplot(dane_punkt2(), aes(x = Data, y = Licznik)) +
      geom_area(fill = "#4FC3F7", alpha = 0.6) +
      geom_line(color = "#0288D1") +
      labs(
        title = paste(input$punktSelect2),
        x = "Data",
        y = "Liczba przejazdów"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5)
      )
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        xaxis = list(
          title = "Data",
          titlefont = list(size = 16, family = "Arial", color = "#333333"),
          tickfont = list(size = 13, family = "Arial", color = "#333333"),
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 13, family = "Arial", color = "#333333")
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hoverlabel = list(
          bgcolor = "#ffffff",
          font = list(color = "#000000", size = 13, family = "Arial"),
          bordercolor = "#cccccc"
        )
      )
  })
  
  ###### Wykresy zadanie 4 & 5 ######
  output$histogram14 <- renderPlotly({
    req(input$punktSelect1, input$podzialSelect1)
    
    df <- przejazdy %>% filter(Stacja == input$punktSelect1)
    
    p <- if (input$podzialSelect1 == "Miesiąc") {
      df <- df %>% 
        mutate(Miesiac = lubridate::month(Data, label = TRUE, abbr = FALSE)) %>%
        count(Miesiac)
      
      #Punkt 1
      ggplot(df, aes(x = Miesiac, y = n,
                     text = paste0("Miesiąc: ", Miesiac, "<br>Liczba przejazdów: ", n))) +
        geom_col(width = 0.7, fill = "#4FC3F7", color = "#0288D1") +
        labs(
          title = paste("Rozkład przejazdów wg miesiąca dla\n", input$punktSelect1),
          y = "Liczba przejazdów",
          x = "Miesiąc"
        )+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$podzialSelect1 == "Dzień tygodnia") {
      df <- df %>% 
        mutate(DzienTygodnia = lubridate::wday(Data, label = TRUE, abbr = FALSE)) %>%
        count(DzienTygodnia)
      ggplot(df, aes(x = DzienTygodnia, y = n, group = 1, 
                     text = paste0("Dzień: ", DzienTygodnia, "<br>Liczba przejazdów: ", n))) +
        geom_line(color = "#0288D1") +
        labs(title = paste("Rozkład przejazdów wg dnia tygodnia dla\n", input$punktSelect1),
             y = "Liczba przejazdów")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$podzialSelect1 == "Typ dnia") {
      df <- df %>% 
        mutate(TypDnia = ifelse(lubridate::wday(Data) %in% c(1, 7), "Weekend", "Dzień powszedni")) %>%
        count(TypDnia)
      
      ggplot(df, aes(x = TypDnia, y = n, 
                     text = paste0("Dzień: ", TypDnia, "<br>Liczba przejazdów: ", n))) +
        geom_col(width = 0.5, fill = "#4FC3F7", color = "#0288D1") +
        labs(
          title = paste("Rozkład przejazdów wg typu dnia dla\n", input$punktSelect1),
          y = "Liczba przejazdów",
          x = "Typ dnia"
        )+
        theme(plot.title = element_text(hjust = 0.5))
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          title = "Data",
          titlefont = list(size = 16, family = "Arial", color = "#333333"),
          tickfont = list(size = 13, family = "Arial", color = "#333333"),
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 13, family = "Arial", color = "#333333")
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hoverlabel = list(
          bgcolor = "#ffffff",
          font = list(color = "#000000", size = 13, family = "Arial"),
          bordercolor = "#cccccc"
        )
      )
  })
  
  #Punkt 2
  output$histogram15 <- renderPlotly({
    req(input$punktSelect2, input$podzialSelect2)
    
    df <- przejazdy %>% filter(Stacja == input$punktSelect2)
    
    p <- if (input$podzialSelect2 == "Miesiąc") {
      df <- df %>% 
        mutate(Miesiac = lubridate::month(Data, label = TRUE, abbr = FALSE)) %>%
        count(Miesiac)
      
      ggplot(df, aes(x = Miesiac, y = n,
                     text = paste0("Miesiąc: ", Miesiac, "<br>Liczba przejazdów: ", n))) +
        geom_col(width = 0.7, fill = "#4FC3F7", color = "#0288D1") +
        labs(
          title = paste("Rozkład przejazdów wg miesiąca dla\n", input$punktSelect2),
          y = "Liczba przejazdów",
          x = "Miesiąc"
        )+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$podzialSelect2 == "Dzień tygodnia") {
      df <- df %>% 
        mutate(DzienTygodnia = lubridate::wday(Data, label = TRUE, abbr = FALSE)) %>%
        count(DzienTygodnia)
      ggplot(df, aes(x = DzienTygodnia, y = n, group = 1, 
                     text = paste0("Dzień: ", DzienTygodnia, "<br>Liczba przejazdów: ", n))) +
        geom_line(color = "#0288D1") +
        labs(title = paste("Rozkład przejazdów wg dnia tygodnia dla\n", input$punktSelect2),
             y = "Liczba przejazdów")+
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        theme(plot.title = element_text(hjust = 0.5))
      
    } else if (input$podzialSelect2 == "Typ dnia") {
      df <- df %>% 
        mutate(TypDnia = ifelse(lubridate::wday(Data) %in% c(1, 7), "Weekend", "Dzień powszedni")) %>%
        count(TypDnia)
      
      ggplot(df, aes(x = TypDnia, y = n, 
                     text = paste0("Dzień: ", TypDnia, "<br>Liczba przejazdów: ", n))) +
        geom_col(width = 0.5, fill = "#4FC3F7", color = "#0288D1") +
        labs(
          title = paste("Rozkład przejazdów wg typu dnia dla\n", input$punktSelect2),
          y = "Liczba przejazdów",
          x = "Typ dnia"
        )+
        theme(plot.title = element_text(hjust = 0.5))
      
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          title = "Data",
          titlefont = list(size = 16, family = "Arial", color = "#333333"),
          tickfont = list(size = 13, family = "Arial", color = "#333333"),
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "",
          tickfont = list(size = 13, family = "Arial", color = "#333333")
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hoverlabel = list(
          bgcolor = "#ffffff",
          font = list(color = "#000000", size = 13, family = "Arial"),
          bordercolor = "#cccccc"
        )
      )
  })
  
  ###### Wykresy zadanie 6 & 7 ######
  #Punkt 1
  output$histogram16 <- renderPlotly({
    req(input$punktSelect3, input$podzialSelect3)

    df2 <- przejazdy %>%
      filter(Stacja == input$punktSelect3)
    
    weather_var <- input$podzialSelect3
    req(weather_var %in% names(df2))
    req("Licznik" %in% names(df2))

    df2$Pogoda_bin <- round(df2[[weather_var]],0)
    
    df_summary <- df2 %>%
      group_by(Pogoda_bin) %>%
      summarise(Srednia = mean(Licznik, na.rm = TRUE)) %>%
      ungroup()
    
    p <- ggplot(df_summary, aes(x = Pogoda_bin, y = Srednia)) +
      geom_line(color = "#0288D1", size = 1) +
      geom_point(
        aes(text = paste0(weather_var, ": ", Pogoda_bin, "<br>Średnia: ", round(Srednia, 2))),
        color = "transparent", size = 0.1
      )+
      labs(
        title = paste("Trend średniej liczby przejazdów w zależności od\n", weather_var),
        x = paste(weather_var, "(grupowane co 5)"),
        y = "Średnia liczba przejazdów"
      ) +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          title = weather_var,
          titlefont = list(size = 16, family = "Arial", color = "#333333"),
          tickfont = list(size = 13, family = "Arial", color = "#333333"),
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "Średnia liczba przejazdów",
          tickfont = list(size = 13, family = "Arial", color = "#333333")
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hoverlabel = list(
          bgcolor = "#ffffff",
          font = list(color = "#000000", size = 13, family = "Arial"),
          bordercolor = "#cccccc"
        )
      )
  })

  #Punkt 2
  output$histogram17 <- renderPlotly({
    req(input$punktSelect4, input$podzialSelect4)
    
    df1 <- przejazdy %>%
      filter(Stacja == input$punktSelect4)
    
    weather_var <- input$podzialSelect4
    req(weather_var %in% names(df1))
    req("Licznik" %in% names(df1))
    
    df1$Pogoda_bin <- round(df1[[weather_var]],0)
    
    df_summary <- df1 %>%
      group_by(Pogoda_bin) %>%
      summarise(Srednia = mean(Licznik, na.rm = TRUE)) %>%
      ungroup()
    
    p <- ggplot(df_summary, aes(x = Pogoda_bin, y = Srednia)) +
      geom_line(color = "#0288D1", size = 1) +
      geom_point(
        aes(text = paste0(weather_var, ": ", Pogoda_bin, "<br>Średnia: ", round(Srednia, 2))),
        color = "transparent", size = 0.1
      )+
      labs(
        title = paste("Trend średniej liczby przejazdów w zależności od\n", weather_var),
        x = paste(weather_var, "(grupowane co 5)"),
        y = "Średnia liczba przejazdów"
      ) +
      theme_minimal()+
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          title = weather_var,
          titlefont = list(size = 16, family = "Arial", color = "#333333"),
          tickfont = list(size = 13, family = "Arial", color = "#333333"),
          gridcolor = "#eeeeee"
        ),
        yaxis = list(
          title = "Średnia liczba przejazdów",
          tickfont = list(size = 13, family = "Arial", color = "#333333")
        ),
        plot_bgcolor = "#ffffff",
        paper_bgcolor = "#ffffff",
        hoverlabel = list(
          bgcolor = "#ffffff",
          font = list(color = "#000000", size = 13, family = "Arial"),
          bordercolor = "#cccccc"
        )
      )
  })

  ###### Wykresy zadanie 8 ######
  output$map <- renderLeaflet({
    req(input$zmienna_przejazdy)
    
    funkcja_stat <- switch(input$zmienna_przejazdy,
                           "mean" = mean,
                           "max" = max,
                           "min" = min)
    
    df_avg <- df_merged %>%
      group_by(Stacja, Lat, Lon) %>%
      summarise(wartosc = funkcja_stat(Licznik, na.rm = TRUE), .groups = "drop")
    
    min_w <- min(df_avg$wartosc, na.rm = TRUE)
    max_w <- max(df_avg$wartosc, na.rm = TRUE)
    
    df_avg <- df_avg %>%
      mutate(
        wartosc_scaled = 4 + (wartosc - min_w) / (max_w - min_w) * 6
      )
    
    pal <- colorBin(
      palette = "YlOrRd",
      domain = df_avg$wartosc,
      bins = 6,  
      pretty = TRUE
    )
    
    legenda_nazwa <- switch(input$zmienna_przejazdy,
                            "mean" = "Średnia liczba przejazdów",
                            "max" = "Maksymalna liczba przejazdów",
                            "min" = "Minimalna liczba przejazdów")
    
    labels <- lapply(seq_len(nrow(df_avg)), function(i) {
      htmltools::HTML(paste0(
        df_avg$Stacja[i], "<br>Liczba przejazdów: ", round(df_avg$wartosc[i], 2)
      ))
    })
    
    leaflet(df_avg) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~Lon, lat = ~Lat,
        radius = ~wartosc_scaled,
        color = ~pal(wartosc),
        fillColor = ~pal(wartosc),
        fillOpacity = 0.8,
        label = labels,
        stroke = FALSE
      ) %>%
      addLegend(
        position = "bottomright",
        pal = pal,
        values = df_avg$wartosc,
        title = legenda_nazwa,
        labFormat = labelFormat(
          prefix = "", suffix = "",
          between = " – ", digits = 0, big.mark = " "
        ),
        opacity = 1
      )
  })
  
  ###### Wykresy zadanie 9 ######
  # 1. Macierz korelacji
  output$macierz_korelacji <- renderPlot({
    library(ggcorrplot)
    cor_matrix <- cor(corr_data, use = "complete.obs")
    ggcorrplot(cor_matrix,
               method = "square",
               type = "full",
               lab = TRUE,
               lab_size = 4.5,
               colors = c("#0288D1", "white", "#e41a1c"),
               ggtheme = theme_minimal(base_family = "Arial")) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, color = "#333333"),
        axis.text.x = element_text(size = 13, color = "#333333", angle = 45, vjust = 1, hjust = 1),
        axis.text.y = element_text(size = 13, color = "#333333"),
        panel.grid.major = element_line(color = "#eeeeee"),
        panel.background = element_rect(fill = "#ffffff"),
        plot.background = element_rect(fill = "#ffffff", color = NA)
      )
  })
  
  # 3. Regresja liniowa
  output$regresja <- renderPlot({
    req(input$paraZmienne)
    
    zmienne <- strsplit(input$paraZmienne, " vs ")[[1]]
    req(length(zmienne) == 2)
    
    x_var <- zmienne[1]
    y_var <- zmienne[2]
    
    df_clean <- df_merged %>%
      filter(!is.na(.data[[x_var]]), !is.na(.data[[y_var]]))
    
    x <- df_clean[[x_var]]
    y <- df_clean[[y_var]]
    
    par(
      bg = "#ffffff",
      col.axis = "#333333",
      col.lab = "#333333",
      col.main = "#333333",
      family = "Arial",
      mar = c(5, 5, 4, 2) + 0.1,
      cex.axis = 1.1,
      cex.lab = 1.3,
      cex.main = 1.4
    )
    
    plot(
      x, y,
      pch = 16,
      col = rgb(2, 136, 209, 100, maxColorValue = 255),  
      xlab = x_var,
      ylab = y_var,
      main = paste(y_var, "względem", x_var),
      cex = 1.0
    )
    
    grid(col = "#eeeeee", lty = "dotted")
    
    model <- lm(y ~ x)
    abline(model, col = "#e74c3c", lwd = 2)
  })
  
  # 4. Sezonowość
  output$sezonowosc <- renderPlot({
    req(input$paraZmienne1)
    
    zmienne <- strsplit(input$paraZmienne1, " vs ")[[1]]
    req(length(zmienne) == 2)
    
    x <- zmienne[1]
    y <- zmienne[2]
    
    library(lubridate)
    
    df_sezon <- df_merged %>%
      mutate(Miesiąc = month(Data, label = TRUE)) %>%
      filter(!is.na(.data[[x]]), !is.na(.data[[y]]))
    
    ggplot(df_sezon, aes_string(x = x, y = y)) +
      geom_point(alpha = 0.4, color = "#0288D1", size = 1.2) +
      geom_smooth(method = "lm", se = FALSE, color = "#e74c3c", size = 0.6) +
      facet_wrap(~Miesiąc) +
      labs(
        title = paste(y, "vs.", x, "(wg miesięcy)"),
        x = x,
        y = y
      ) +
      theme_minimal(base_family = "Arial") +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold", color = "#333333"),
        axis.title = element_text(size = 14, color = "#333333"),
        axis.text = element_text(size = 11, color = "#333333"),
        strip.text = element_text(size = 13, face = "bold", color = "#2c3e50"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#eeeeee"),
        panel.background = element_rect(fill = "#ffffff", color = NA),
        plot.background = element_rect(fill = "#ffffff", color = NA)
      )
  })
  
  # 5. PCA
  output$pca <- renderPlot({
    library(factoextra)
    
    pca <- prcomp(na.omit(corr_data), scale. = TRUE)
    
    fviz_pca_biplot(
      pca,
      repel = TRUE,   
      col.var = "#E74C3C",            
      col.ind = "#0288D1",            
      geom.ind = "point",              
      pointsize = 1.5,
      labelsize = 4,
      arrowsize = 0.8
    ) +
      theme_minimal(base_family = "Arial")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)

