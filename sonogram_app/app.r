
### Shiny app: create seewave spectrogram/sonogram
### 28 Nov 2025 Rob van Bemmelen

### to-do ####

# a) have a nice name for the app,
# b) options to change the contrasts of the sonogram?
# c) ...

#### libraries ####

library(shiny)
library(seewave)
library(tuneR)
library(av)
library(base64enc)

#### options ####

#### ui ####
ui <- fluidPage(
  titlePanel(title=div(img(src='inornatus.png', height = 68, width = 68), "Sonomaker!")),
  sidebarLayout(
    sidebarPanel(
      # some text
      tags$div(class="header", checked=NA,
               "This app produces a sonogram of your sound file. Files should be smaller than 5MB and preferably in .wav or .mp3 format, but .mp4/.m4a or .aac will also work.
               Start time is limited to recordings of 10s; this is because the main purpose of this app is to use it in the field.
               If your bird is a Olive-backed or Tree Pipit, you may want to use",
               tags$a(href="https://longicaudus.shinyapps.io/treepipit_app/", "this identification tool."),
               "\n"
      ),
 
      h5(""),
      fileInput(
        inputId = 'wav', label = 'Sound file', 
        accept = c('.wav', '.WAV', '.mp3', '.MP3', '.aac', '.m4a', '.mp4')),
      sliderInput(
        inputId = "timestart", label = "Start time (s)", 
        value=0, min=0, max=10, step=0.1),
      sliderInput(
        inputId = "duration", label = "Duration (s)", 
        value=2, min=0, max = 3, step = 0.2),
      sliderInput(
        inputId = "freqrange", label = "Frequency range (kHz)", 
        value = c(0,8), min = 0, max = 16, dragRange = TRUE),
      sliderInput(
        inputId = "wl", label = "Window size for FFT", 
        value=512, min = 128, max = 2048, step = 128),
      sliderInput(
        inputId = "gamma", label = "Contrast",
        value = 1.7, min = 1.2, max = 5, step = .1),
      sliderInput(
        inputId = "filter", 
        label = "Cutoff for high-pass fiter (kHz)", 
        value = 0.5, min = 0, max = 4, step = 0.25)
    ),
    mainPanel(
      # sonogram
      plotOutput("sono", width="auto"),
      
      # play button
      uiOutput('playwav'),
      
      # download button
      downloadButton("downloadPlot", "Download sonogram (JPEG)"),
      
      # credits
      h6("This app uses the seewave, av and tuneR packages in R.", align = "right"),
      h6("Rob van Bemmelen, 28 Nov 2025", align = "right")
    )
  )
)

#### server ####
server <- function(input, output) {
  # debounce to avoid quick redraws 
  dbounce <- 1000
  # timestart, duration freqrange wl gamma filter
  timestart_react = reactive({
    input$timestart
  }) |> shiny::debounce(dbounce)
  duration_react = reactive({
    input$duration
  }) |> shiny::debounce(dbounce)
  freqrange_react = reactive({
    input$freqrange
  }) |> shiny::debounce(dbounce)
  wl_react = reactive({
    input$wl
  }) |> shiny::debounce(dbounce)
  gamma_react = reactive({
    input$gamma
  }) |> shiny::debounce(dbounce)
  filter_react = reactive({
    input$filter
  }) |> shiny::debounce(dbounce)
  
  # function for grey scale
  grey_palette <- function(n) {
    x_gamma <- seq(
      from = 1^gamma_react(), 
      to = 0, 
      length = n)^(1/gamma_react())
    gray(x_gamma)
  }
  # grey_palette <- function(n) {
  #   x <- seq(0, 1, length.out = n)
  #   # Sigmoïde functie
  #   sigmoid <- function(x, k) {
  #     1 / (1 + exp(-k * (x - 0.5)))
  #   }
  #   vals <- -1 * sigmoid(x, gamma_react()) + 1
  #   gray(vals)
  # }
  
  # function to read sound file ####
  loadAudio <- function(){
    # determine format
    form <- substr(input$wav$datapath, nchar(input$wav$datapath)-2, nchar(input$wav$datapath))
    dir.create("www/", showWarnings = FALSE)
    
    if ( form %in% c('mp3','MP3')) {
      r <- readMP3(input$wav$datapath)  ## MP3 file in working directory
      writeWave(r, "www/tmp.wav", extensible=FALSE)
    }
    if ( form %in% c("aac", "m4a", "mp4", "M4A", "MP4")) {
      av_audio_convert(
        input$wav$datapath,
        "www/tmp.wav",
        channels = 1, total_time = 10)
    }
    if ( form %in% c('wav','WAV')) {
      wav <- readWave(as.character(input$wav$datapath))
    } else {
      wav <- readWave("www/tmp.wav")
      file.remove("www/tmp.wav")
    }
    # write wav file
    writeWave(wav, "www/tmp.wav", extensible=FALSE)
  }

  # to avoid that older files are used...
  if("tmp.wav" %in% list.files(file.path(getwd(), "www"))) {
    file.remove("www/tmp.wav")
  }
  
  # sound player ####
  output$playwav <- renderUI({
    if (is.null(input$wav$datapath))
      return(NULL)
    loadAudio()
    tags$audio(
      controls = "controls",
      tags$source(
        src = dataURI(file = "www/tmp.wav", mime = "audio/wav"),
        type='audio/wav')
    )
  }
  )
  # sonogram ####
  output$sono <- renderPlot({
    # check if a sound file has been selected
    if (is.null(input$wav$datapath))
      return(NULL)
    loadAudio()
    wav <- readWave("www/tmp.wav")
    
    # filter the stuff
    w <- ffilter(wav, from = filter_react()*1000)
    
    # time axis adjustment in case of short recordings
    maxx <- length(wav@left)/wav@samp.rate
    if ( maxx < (timestart_react() + duration_react()) ) {
      limt <- c(timestart_react(), maxx-0.05)
    } else {
      limt <- c(timestart_react(), timestart_react()+duration_react())
    }

    # make spectrogram
    spectro(
      w, 
      f = 44100, 
      wl = wl_react(),
      ovlp = 50, 
      zp = 50,
      collevels = seq(-65, -0, 1), 
      palette = grey_palette, 
      flim = input$freqrange, 
      tlim = limt,
      scale = FALSE)

    # add horizontal dotted lines to help the eye
    abline(h = seq(1, 19, 1), col = 2, lty = 3)
  })
  
  # export sonogram as JPEG ####
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0("sonogram.jpeg") },
    content = function(file) {
      if (is.null(input$wav$datapath))
        return(NULL)
      loadAudio()
      wav <- readWave("www/tmp.wav")
      
      # filter stuff
      w <- ffilter(wav, from = filter_react()*1000)
      # time axis adjustment in case of short recordings
      maxx <- length(wav@left)/wav@samp.rate
      if ( maxx < (timestart_react()+duration_react()) ) {
        limt <- c(timestart_react(), maxx-0.05)
      } else {
        limt <- c(timestart_react(), timestart_react()+duration_react())
      }
      
      jpeg(
        file, 
        width = 8, 
        height = 5, 
        units = "in", 
        res = 150, 
        quality = 100)

      # make spectrogram
      spectro(
        w, 
        f = 44100,
        wl = wl_react(), 
        ovlp = 50, 
        zp = 50,
        collevels=seq(-65,-0,1), 
        palette = grey_palette, 
        flim = input$freqrange, 
        tlim = limt,
        scale = FALSE)

      abline(h=seq(1,19,1), col=2, lty=3)
      
      dev.off()
      },
    contentType = 'image/jpeg'
  )
}

shinyApp(ui=ui, server=server)

