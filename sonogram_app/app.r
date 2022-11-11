
### Shiny app: create seewave spectrogram/sonogram
### 04/11/2022 Rob van Bemmelen

### to-do ####

# a) have a nice name for the app,
# b) options to change the contrasts of the sonogram?
# c) ...

#### libraries ####

library(shiny)
library(seewave)
library(tuneR)
library(av)
    
#### options ####

#### ui ####
ui <- fluidPage(
  titlePanel(title=div(img(src='inornatus.png', height = 68, width = 68), "Make a sonogram!")),
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
      fileInput(inputId='wav', label='Sound file', accept=c('.wav', '.WAV', '.mp3', '.MP3', '.aac', '.m4a', '.mp4')),
      sliderInput(inputId="timestart", label="Start time (s)", value=0, min=0, max=10, step=0.1),
      sliderInput(inputId="duration", label="Duration (s)", value=2, min=0, max=3, step=0.2),
      sliderInput(inputId="freqrange", label="Frequency range (kHz)", value=c(0,8), min=0, max=16, dragRange=TRUE),
      sliderInput(inputId="wl", label="Window size for FFT", value=512, min=128, max=2048, step=128),
      sliderInput(inputId="filter", label="Cutoff for high-pass fiter (kHz)", value=0.5, min=0, max=4, step=0.25),
      radioButtons(inputId="clrs", label="Colour/greyscale", choices=c('greys', 'colours'))
    ),
    mainPanel(
      # sonogram
      plotOutput("sono", width="auto"),
      
      # play button
      uiOutput('playwav'),
      
      # download button
      downloadButton("downloadPlot", "Download sonogram (JPEG)"),
      
      # credits
      h6("This app uses the 'seewave' and 'tuneR' packages in R.", align = "right"),
      h6("Rob van Bemmelen, 19/07/2018", align = "right")
    )
  )
)

#### server ####
server <- function(input, output) {
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
    writeWave(wav, "www/tmp.wav", extensible=TRUE)
  }

  # sound player ####
  output$playwav <- renderUI({
    if (is.null(input$wav$datapath))
      return(NULL)
    loadAudio()
    tags$audio(
      controls = "controls",
      tags$source(
        src = markdown:::.b64EncodeFile("www/tmp.wav"),
        type='audio/wav')
    )
  }
  )
  # sonogram ####
  output$sono <- renderPlot({
    # # check if a sound file has been selected
    if (is.null(input$wav$datapath))
      return(NULL)
    loadAudio()
    wav <- readWave("www/tmp.wav")
    
    # filter the stuff
    w <- ffilter(wav, from=input$filter*1000)
    
    #time axis adjustment in case of short recordings
    maxx <- length(wav@left)/wav@samp.rate
    if ( maxx < (input$timestart+input$duration) ) {
      limt <- c(input$timestart, maxx-0.05)
    } else {
      limt <- c(input$timestart, input$timestart+input$duration)
    }
    # make spectrogram
    if ( input$clrs=='greys' ) { col.pal <- reverse.gray.colors.1 } else { col.pal <- spectro.colors}
    spectro(w, f=44100, wl=input$wl, ovlp=50, zp=50,
            collevels=seq(-65, -0, 1), palette=col.pal, 
            flim=input$freqrange, 
            tlim=limt,
            scale=FALSE)
    # add horizontal dotted lines to help the eye
    abline(h=seq(1,19,1), col=2, lty=3)
  })
  
  # export sonogram ####
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0("sonogram.jpeg") },
    content = function(file) {
      if (is.null(input$wav$datapath))
        return(NULL)
      loadAudio()
      wav <- readWave("www/tmp.wav")
      
      # filter stuff
      w <- ffilter(wav, from=input$filter*1000)
      # time axis adjustment in case of short recordings
      maxx <- length(wav@left)/wav@samp.rate
      if ( maxx < (input$timestart+input$duration) ) {
        limt <- c(input$timestart, maxx-0.05)
      } else {
        limt <- c(input$timestart, input$timestart+input$duration)
      }
      
      jpeg(file, width = 8, height = 5, units = "in", res = 150, quality=100)

      # make spectrogram
      if ( input$clrs=='greys' ) { col.pal <- reverse.gray.colors.1 } else { col.pal <- spectro.colors}
      spectro(w, f=44100, wl=input$wl, ovlp=50, zp=50,
              collevels=seq(-65,-0,1), palette=col.pal, 
              flim=input$freqrange, 
              tlim=limt,
              scale=FALSE)
      abline(h=seq(1,19,1), col=2, lty=3)
      dev.off()
      },
    contentType = 'image/jpeg'
  )
}

shinyApp(ui=ui, server=server)

