
### Shiny app: identify tree pipits
### v.0.1 18/07/2018 Rob van Bemmelen
### based on: https://avesrares.wordpress.com/2013/09/27/identification-of-olive-backed-and-tree-pipit-by-call/

### to-do

#### libraries ####
library(shiny)
library(seewave)
library(tuneR)

# path to temporary wav file
tmp_path <- file.path(getwd(), "www", "tmp.wav")

#### ui ####
ui <- fluidPage(
  titlePanel(
    #    title = tags$link(rel = "icon", type = "image/png", 
    #                      href = "/media/ddata/projects/R script library/sonogram_app/inornatus.png"), 
    "Identify your tree pipit!"),
  sidebarLayout(
    sidebarPanel(
      tags$div(class="header", checked=NA,
               "Based on ",
               tags$a(href="https://avesrares.wordpress.com/2013/09/27/identification-of-olive-backed-and-tree-pipit-by-call/", "Ralph Martins website,"),
               " this app lets you identify flight calls of Olive-backed and Tree Pipits. Upload your recording (max 5MB), click two positions (mean starting frequency and mean ending frequency) in the resulting sonogram and you'll get parameter values. These are then compared with",
               tags$a(href="https://avesrares.files.wordpress.com/2013/09/table_21.jpg", "table 2"),
               "of Martins' online paper. You´ll have to judge yourself what is the most likely identification. Good luck! \n"
      ),
      h5(" "),
      fileInput(inputId='wav', label='Select sound file', accept=c('.wav', '.WAV', '.mp3', '.MP3', '.aac', '.m4a', '.mp4')),
      sliderInput(inputId="timestart", label="Start time (s)", value=0, min=0, max=10, step=0.1),
      sliderInput(inputId="duration", label="Duration (s)", value=2, min=0, max=3, step=0.1),
      sliderInput(inputId="freqrange", label="Frequency range (kHz)", value=c(4,10), min=0, max=16, dragRange=TRUE),
      sliderInput(inputId="wl", label="Window size for FFT", value=256, min=128, max=2048/2, step=128),
      sliderInput(inputId="filter", label="Cutoff for high-pass fiter (kHz)", value=0.5, min=0, max=4, step=0.25),
      radioButtons(inputId="clrs", label="Colour/greyscale", choices=c('greys', 'colours'))
    ),
    mainPanel(
      tags$div(class="header", checked=NA,
               "After zooming in on the flight call, click two times: first on the mean starting frequency, then on the mean ending frequency. Select only the linearly declining part of the call. See",
               tags$a(href="https://avesrares.files.wordpress.com/2013/09/olive-backed-pipit_measure_points1.jpg", 
                      "here"), 
               "for an example."
      ),
      # sonogram
      plotOutput("sono", width="auto", click = "plot_click"),

      # play button
      uiOutput('playwav'),
      
      # download button
      actionButton("reset", "Reset"),
      downloadButton("downloadPlot", "Download (JPEG)"),
      
      # some white space
      br(),
      br(),
      
      # explanation of table
      tags$div(
        class="header", checked=NA,
        paste(
        "Notes to the table below. Parameter values and whether they fall within (TRUE) or outside (FALSE) the ranges for each species, following table 2 at Ralph Martins website. Note that estimation of the modulation duration and therefore also parameter 2 mostly works OK but remains somewhat uncertain. Check whether sufficient pairs of modulations peaks have been detected (blue stars). Use with caution in your final identification!", 
        sep= "\n")
        ),
      br(),
      br(),
      
      # table output
      tableOutput("table"),
      
      # credits
      h6("This app uses the 'seewave' and 'tuneR' packages in R.", align = "right"),
      h6("Rob van Bemmelen, 13/07/2018", align = "right")
      
    )
  )
)

#### server ####
server <- function(input, output, session) {
  # read sound file function
  loadAudio <- function(){
    if (is.null(input$wav$datapath))
      return(NULL)
    # determine format
    form <- substr(input$wav$datapath, nchar(input$wav$datapath)-2, nchar(input$wav$datapath))
    
    if ( form %in% c('mp3','MP3')) {
      r <- readMP3(input$wav$datapath)  ## MP3 file in working directory
      writeWave(r, tmp_path, extensible=FALSE)
    }
    if ( form %in% c("aac", "m4a", "mp4", "M4A", "MP4")) {
      av_audio_convert(
        input$wav$datapath,
        tmp_path,
        channels = 1, total_time = 10)
    }
    if ( form %in% c('wav','WAV')) {
      wav <- readWave(as.character(input$wav$datapath))
    } else {
      wav <- readWave(tmp_path)
    }
    file.remove(tmp_path)
    # write wav file
    writeWave(wav, tmp_path, extensible=TRUE)
  }
  
  # show player
  output$playwav <- renderUI({
    if(is.null(input$wav))
      return(NULL)
    observeEvent(input$wav, loadAudio())
    tags$audio(
      controls = "controls",
      tags$source(
        src = markdown:::.b64EncodeFile(tmp_path),
        type='audio/wav')
    )
  }
  )
  
  # sonogram
  output$sono <- renderPlot({
    # check if a sound file has been selected
    if (is.null(input$wav))
      return(NULL)
    # otherwise load file
    observeEvent(input$wav, loadAudio())
    wav <- readWave(tmp_path)
    w <- ffilter(wav, from=input$filter*1000)
    # colour or greys?
    if ( input$clrs=='greys' ) { col.pal <- reverse.gray.colors.1 } else { col.pal <- spectro.colors}
    
    # time axis adjustment in case of short recordings
    maxx <- length(wav@left)/wav@samp.rate
    if ( maxx < (input$timestart+input$duration) ) {
      limt <- c(input$timestart, maxx-0.05)
    } else {
      limt <- c(input$timestart, input$timestart+input$duration)
    }

    # make spectrogram
    spectro(w, f=wav@samp.rate, wl=input$wl, ovlp=75, zp=50,
            collevels=seq(-65,-0,1), palette=col.pal, 
            flim=input$freqrange, 
            tlim=limt,
            scale=FALSE)
    # add horizontal dotted lines to help the eye
    #abline(h=seq(1,19,1), col=2, lty=3)
    
    if ( !is.null(v$t1)) {
      # add lines
      abline(v=v$t1, col=2)
      abline(h=v$f1, col=2)      
    }
    if ( !is.null(v$t2)){
      # add lines
      abline(v=v$t2, col=2)
      abline(h=v$f2, col=2)      
      
      # fit line 
      res <- dfreq(w, f=wav@samp.rate, ovlp=75, threshold=6, type="l", col="red", lwd=2, 
                   tlim=c(v$t1, v$t2), 
                   bandpass=c(4,10)*1000, plot=FALSE, wl=256)
      # now select part between clicks (t) and between 3-9.8kHz to get the modulations...
#      res <- res[!is.na(res[,2]), ]# remove NAs
#      res <- res[res[,2] >= 4 & res[,2] <= 9.8,] # 9.8 is the highest recorded for hodgsoni, plus 0.5 kHz
      
      # max frequency (in the clicked part!)
      v$maxfreq <- as.numeric(max(res[res[,2] < 10,2], na.rm=TRUE))
      # add to plot
      points(res[,1]+v$t1, y=res[,2], col=2, type='o', pch=16, cex=0.5, lwd=3)
      abline(h=v$maxfreq, col='green')
      points(c(v$t1, v$t2), c(v$f1, v$f2), type='l', col=3, lwd=2)
      
      # peak detection
      fp <- fpeaks(res, amp=c(0.01, 0.01), plot=FALSE)
      if ( is.na(fp[[1]]) ) { 
        v$npeaks <- 1 
      } else { 
        v$npeaks <- nrow(fp) 
        points(x=fp[,1]+v$t1, y=rep(8,nrow(fp)), pch="*", col=4)
      }

      ### calculate the parameters
      # slope
      v$dfr <- (v$f1-v$f2)/(v$t2-v$t1) # make positive!
      
      # number of modulations/sec
      v$moddur <- median(fp[2:nrow(fp),1]-fp[1:(nrow(fp)-1),1], na.rm=TRUE)*1000
      #v$moddur <- (fp[nrow(fp),1]-fp[1,1])/(v$npeaks-1)*1000
    }
  }  )
  
  # reactive values
  v <- reactiveValues(
    t0 = NULL,
    t1 = NULL,
    t2 = NULL
  )
  
  # reset button
  observeEvent(input$reset, {
    v$t0 <- NULL
    v$t1 <- NULL
    v$t2 <- NULL
  })
  
  # Handle clicks on the plot
  observeEvent(input$plot_click, {
    if ( is.null(v$t0) ) {
      # We don't have a first click, so this is the first click
      v$t0 <- input$plot_click$x
      v$f0 <- input$plot_click$y
      # duplicate, so the first can be set to NULL at the end of this [is this crazy?]
      v$t1 <- input$plot_click$x
      v$f1 <- input$plot_click$y

    } else {
      # We already had a first click, so this is the second click.
      # Enter in v from the previous click and this one.
      v$t2 <- input$plot_click$x
      v$f2 <- input$plot_click$y
      # Clear results now the next click starts two new values.
      v$t0 <- NULL
    }
  })
  
  # Show results of the wicked plotclicking in a table
  output$table <- renderTable(      {
    if ( !is.null(v$t1) & !is.null(v$t2) & !is.null(v$moddur)  )
    data.frame(
      Parameters=c(
#        "Maximum frequency (kHz)",
        "Mean starting frequency (MSF, kHz)",
        "Gradient (kHz/s)",
        "Parameter 1 (MSF*slope)",
        "Duration of one modulation (ms)",
        "Parameter 2 (MSF*slope*mod.dur)"
        ),
      Values=c(
#        round(v$maxfreq,1), # max freq
        round(v$f1,1), # MSF
        round(v$dfr,1), # slope
        round(v$f1*v$dfr, 0), # param 1: MSF * slope
        round(v$moddur,1), # modulation duration
        round(v$f1*v$dfr*v$moddur,0)  # param 2: MSF * slope * moddur
      ),
      trivialis=c(
#        round(v$maxfreq,1) >= 6.5 & round(v$maxfreq,1) <= 8.4, 
        round(v$f1,1) >= 5.9 & round(v$f1,1) <= 7.6,
        round(v$dfr,1) >= 1.8   & round(v$dfr,1) <= 10.6,
        round(v$f1*v$dfr, 0) >= 12 & round(v$f1*v$dfr, 0) <= 72,
        round(v$moddur,1) >= 4.6 & round(v$moddur,1) <= 7.7,
        round(v$f1*v$dfr*v$moddur,0) >= 82 & round(v$f1*v$dfr*v$moddur,0) <= 440
      ),
      hodgsoni=c(
#        round(v$maxfreq,1) >= 7.7 & round(v$maxfreq,1) <= 9.3, 
        round(v$f1,1) >= 7.2 & round(v$f1,1) <= 8.6,
        round(v$dfr,1) >= 8.4 & round(v$dfr,1) <= 22,
        round(v$f1*v$dfr, 0) >= 62 & round(v$f1*v$dfr, 0) <= 187,
        round(v$moddur,1) >= 7.3 & round(v$moddur,1) <= 11.6,
        round(v$f1*v$dfr*v$moddur,0) >= 565 & round(v$f1*v$dfr*v$moddur,0) <= 2100
      )
      )
  }
  )

  # Downloadable png of plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste0("sonogram.jpeg") },
    content = function(file) {
      jpeg(file, width = 8, height = 5, units = "in", res = 150, quality=100)
      if (is.null(input$wav))
        return(NULL)
      # otherwise load file
      observeEvent(input$wav, loadAudio())
      wav <- readWave(tmp_path)
      w <- ffilter(wav, from=input$filter*1000)
      
      # greys or colour
      if ( input$clrs=='greys' ) { col.pal <- reverse.gray.colors.1 } else { col.pal <- spectro.colors}
      
      # time axis adjustment in case of short recordings
      maxx <- length(wav@left)/wav@samp.rate
      if ( maxx < (input$timestart+input$duration) ) {
        limt <- c(input$timestart, maxx-0.05)
      } else {
        limt <- c(input$timestart, input$timestart+input$duration)
      }
      
      # draw spectrogram for download
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



