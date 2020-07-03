#Shiny app for Small Mammal QC - Timothy Gilbert, Domain 14
###This app will take most current Fulcrum bout data (before uploaded to NEON database
#and QC for missing data (Hindfoot, weight, reference IDs, sample barcodes), pull species
#of interest (escaped, dead, species captured on plot for first time ever), check for
#duplicates (barcodes, tagIDs) within current bout and compared to NEON portal database,
#check 'sex' identifications for each tagID within bout and compared to NEON portal
#database [looks at NEON data and assigns 'sex' based on what individual has been called
#most - then compares to current bout tagIDs], checks species specific measurements based
#on species HF/W ranges - using 'mam_size_ranges_KMT.csv' from NEON teams project that can
#be updated for each domain based on HF/W measurement range preferences, and renders two
#plots to look for outlier measurements for each individuals HF vs W (1 plot for current
#bout data uploaded and 1 plot for all data collected at that particular site to compare)
###Note: This app pulls data from site from 01-2018 to present - can be altered by changing
#'startdate =' in line 112 for mam.raw to preferred date in similar format 'MM-YYYY'
###This app has not been published as of 7/2/2020###
#-------------------------------------------------------------------------------
#####TO RUN THIS APP SELECT ALL AND RUN or [Ctrl + A] / [Ctrl + Enter]#####
library(neonUtilities)  ##Loading Libraries##
library(dplyr)
library(tidyverse)
library(readr)
library(tidyr)
library(plotly)
library(shiny)

if (interactive()) {        #interactive app
  #---user interface--------------------------------------------------  
  ui <- fluidPage(
    titlePanel("Small Mammal QC Check"),
    sidebarLayout(
      sidebarPanel(
        #selection area for each site- can type in site and app will find from list below
        selectInput("Select", "Please type and select your site:",
                    choices = c("SRER", "JORN",   "BART", "HARV", "BLAN", "SCBI", "SERC", "DSNY", "JERC", "OSBS", "GUAN", "LAJA","STEI", "TREE", "UNDE","KONA", "KONZ", "UKFS", "GRSM", "MLBS", "ORNL","DELA", "LENO", "TALL", "DCFS", "NOGP", "WOOD", "CPER", "RMNP", "STER", "CLBJ", "OAES", "YELL", "MOAB", "NIWO", "JORN", "SRER", "ONAQ", "ABBY", "WREF", "SJER", "SOAP", "TEAK", "BARR", "TOOL", "BONA", "DEJU", "HEAL"), selected = F, multiple = T),
        fileInput("file1", "Choose CSV File for QC ['mam_pertrapnight.csv']",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        fileInput("file2", "Choose CSV File for Site Specific Measurements ['mam_size_ranges_KMT.csv']",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
      ),
      mainPanel("Please wait for app to access NEON data portal",
                tabsetPanel(
                  tabPanel("New Species",
                           tableOutput("newspecies")),        
                  tabPanel("Missing Data",
                           tableOutput("MissHF"),
                           tableOutput("MissW"),
                           tableOutput("MissIDref"),
                           tableOutput("MissBcode"),
                           tableOutput("MissFcode"),
                           tableOutput("MissEcode"),
                           tableOutput("MissHcode")),
                  tabPanel("Escaped or Died",
                           tableOutput("Escaped"),
                           tableOutput("Dead")),
                  tabPanel("Quality Check",
                           tableOutput("TagDuplicates"),
                           tableOutput("NEONTagDuplicates"),
                           tableOutput("BarcodeDuplicates"),
                           tableOutput("NEONBarcodeDuplicates"),
                           tableOutput("SexID"),
                           tableOutput("NEONSexID")),
                  tabPanel("Measurements",
                           tableOutput("HFmeasurements"),
                           tableOutput("Wmeasurements")),
                  tabPanel("Bout Data Plot",
                           plotlyOutput("MAMdata")),
                  tabPanel("All Data Plot",
                           plotlyOutput("MAMdataNEON"))
                )
      )
    )
  )
  #--Server----------------------------------------------------------------
  server <- function(input, output) {
    #Reactive files [dataFile1, dataFile2] and [Select] for site to run now and use later for rest of code
    datafile1 <- reactive({
      inFile <- input$file1 
      if (is.null(inFile))
        return(NULL)
      bout.data <- read_csv(inFile$datapath)
    })
    datafile2 <- reactive({
      inFile2 <- input$file2
      if (is.null(inFile2))
        return(NULL)
      measurements <- read_csv(inFile2$datapath) 
    })
    site_select <- reactive({
      site<- input$Select #saving site selection
      site<- input$Select
      req(input$Select)
      if (is.null(input$Select))
        return(NULL)
      #-uploaded NEON portal data for QC----------------------------------------------------------------    
      #Downloading NEON portal data since 2018 to present
      mam.data.raw <- loadByProduct(dpID = "DP1.10072.001", site = site, startdate = '2018-01', package = 'expanded', check.size = 'F' )
      #Getting raw mammal data - includes all sets / no sets
      mam.raw <- as_tibble(mam.data.raw$mam_pertrapnight)
    })
    #-QCing bout data uploaded (missing data)------------------------------------------------------------------------------------  
    output$MissHF <- renderTable({    
      bout.data <- datafile1()        #pulling data from bout file upload
      missing_HF<- bout.data %>%
        filter(!is.na(taxonID),       # samples w/taxonIDs
               is.na(hindfootLength)) # looking for missing hindfootLengths
      if(length(missing_HF$tagID) >0) {
        print(paste0(missing_HF$taxonID, " missing hindfootLength - ", missing_HF$tagID, " at ", missing_HF$plotID," on ", missing_HF$collectDate))
      }  
    })
    output$MissW <- renderTable({    
      bout.data <- datafile1()
      missing_W<- bout.data %>% 
        filter(!is.na(taxonID),   # samples w/taxonIDs
               is.na(weight))     # looking for missing weights
      if(length(missing_W$tagID) >0) {
        print(paste0(missing_W$taxonID, " missing weight - ", missing_W$tagID, " at ", missing_W$plotID," on ", missing_W$collectDate))
      }  
    })
    output$MissIDref <- renderTable({  
      bout.data <- datafile1()  
      missing_ID<- bout.data %>% 
        filter(!is.na(taxonID),                   # samples w/taxonIDs
               is.na(identificationReferences))   # looking for missind ID's
      if(length(missing_ID$tagID) >0) {
        print(paste0(missing_ID$taxonID, " missing referenceID - ", missing_ID$tagID, " at ", missing_ID$plotID," on ", missing_ID$collectDate))
      }
    })
    output$MissBcode <- renderTable({
      bout.data <- datafile1()  
      missing_Bcode<- bout.data %>% 
        filter(!is.na(bloodSampleID),       # samples w/bloodSampleIDs
               is.na(bloodSampleBarcode))   # looking for missing barcodes  
      if(length(missing_Bcode$tagID) >0) {
        print(paste0(missing_Bcode$taxonID, " missing .B barcode - ", missing_Bcode$tagID, " at ", missing_Bcode$plotID," on ", missing_Bcode$collectDate))
      }
    })
    output$MissFcode <- renderTable({
      bout.data <- datafile1()  
      missing_Fcode<- bout.data %>% 
        filter(!is.na(fecalSampleID),       # samples w/fecalSampleIDs
               is.na(fecalSampleBarcode))   # remove barcodes present - looking for missing barcodes 
      if(length(missing_Fcode$tagID) >0) {
        print(paste0(missing_Fcode$taxonID, " missing .F barcode - ", missing_Fcode$tagID, " at ", missing_Fcode$plotID," on ", missing_Fcode$collectDate))
      }  
    })
    output$MissHcode <- renderTable({
      bout.data <- datafile1()  
      missing_Hcode<- bout.data %>% 
        filter(!is.na(hairSampleID),       # samples w/hairSampleIDs
               is.na(hairSampleBarcode))   # looking for missing barcodes
      if(length(missing_Hcode$tagID) >0) {
        print(paste0(missing_Hcode$taxonID, " missing .H barcode - ", missing_Hcode$tagID, " at ", missing_Hcode$plotID," on ", missing_Hcode$collectDate))
      }
    })
    output$MissEcode <- renderTable({
      bout.data <- datafile1()
      missing_Ecode<- bout.data %>% 
        filter(!is.na(earSampleID),       # samples w/earSampleIDs
               is.na(earSampleBarcode))   # looking for missing barcodes  
      if(length(missing_Ecode$tagID) >0) {
        print(paste0(missing_Ecode$taxonID, " missing .E barcode - ", missing_Ecode$tagID, " at ", missing_Ecode$plotID," on ", missing_Ecode$collectDate))
      }    
    })
    #---esacped/dead species from bout
    output$Escaped <- renderTable({
      bout.data <- datafile1()  
      escaped<- bout.data %>%
        filter(fate == 'escaped')
      if(length(escaped$tagID) >0) {
        print(paste0(escaped$taxonID, " escaped - ", escaped$tagID, " at ", escaped$plotID," on ", escaped$collectDate))
      }
    })
    output$Dead <- renderTable({
      bout.data <- datafile1()  
      dead<- bout.data %>% 
        filter(fate == 'dead')
      if(length(dead$tagID) >0) {
        print(paste0(dead$taxonID, " died - ", dead$tagID, " at ", dead$plotID," on ", dead$collectDate))
      } 
    })
    #---looking for duplicated barcodes in current upload---
    output$BarcodeDuplicates <- renderTable({
      bout.data <- datafile1()
      barcode.data<- bout.data %>%  #selecting only barcode data to check for duplicates
        select(contains("Barcode"))  #columns w/ "Barcode"
      codes<- list()
      duplicated_barcodes<- list()
      bar_code_list<- c()
      finalBar<- c()
      i=1 #using i = 1:4 because going through 4 columns of barcode data
      while (i<5){  #saving each barcode column with filled in barcodes
        codes[[i]]<- barcode.data %>%
          filter(!is.na(barcode.data[[i]]))
        duplicated_barcodes[i]<- codes[[i]][i][duplicated(codes[[i]][i]),] #finds row of duplicated barcode
        bar_code_list<- c(bar_code_list,codes[[i]][i][[1]])   #compiling all 'new' barcodes
        i=i+1
      }
      i=1
      while (i<5) {
        if (length(duplicated_barcodes[[i]]) > 0) {
          finalBar<- c(finalBar,cat(paste0(duplicated_barcodes[[i]], " - duplicated barcode used within current bout data\n")))
        }
        i=i+1
      }
      finalBar
    })
    output$NEONBarcodeDuplicates <- renderTable({
      #--Previous code to get variable needed to compare barcodes 'bar_code_list'
      bout.data <- datafile1()
      barcode.data<- bout.data %>%  #selecting only barcode data to check for duplicates
        select(contains("Barcode"))  #columns w/ "Barcode"
      codes<- list()
      duplicated_barcodes<- list()
      bar_code_list<- c()
      finalBar<- c()
      i=1 #using i = 1:4 because going through 4 columns of barcode data
      while (i<5){  #saving each barcode column with filled in barcodes
        codes[[i]]<- barcode.data %>%
          filter(!is.na(barcode.data[[i]]))
        duplicated_barcodes[i]<- codes[[i]][i][duplicated(codes[[i]][i]),] #finds row of duplicated barcode
        bar_code_list<- c(bar_code_list,codes[[i]][i][[1]])   #compiling all 'new' barcodes
        i=i+1
      }
      #--end of previous data--
      mam.raw <- site_select()
      #checking barcodes w/previous NEONportal data
      NEON.barcode.data<- mam.raw %>%  #selecting only barcode data to check for duplicates
        select(contains("Barcode")) #columns w/ "Barcode"
      NEON_codes<- list()
      NEON_barcodes<- list()
      N_bar_code_list<- c()
      i=1
      while (i<5){  #saving each barcode column with filled in barcodes
        NEON_codes[[i]]<- NEON.barcode.data %>%
          filter(!is.na(NEON.barcode.data[[i]]))
        N_bar_code_list<- c(N_bar_code_list,NEON_codes[[i]][i][[1]])   #compiling all 'old' barcodes
        i=i+1
      }
      #finds barcodes already uploaded onto NEON portal with intersect()
      final_barcode_check<- c()
      final_barcode_check<- intersect(bar_code_list, N_bar_code_list)
      NfinalBar<- c()
      if (length(final_barcode_check) > 0) {
        NfinalBar<- c(NfinalBar,print(paste0(final_barcode_check, " - duplicated barcode used on NEON data portal already")))
      }
      NfinalBar
    })
    #---looking for duplicated tag IDs in current bout upload----------------------------------------
    output$TagDuplicates <- renderTable({
      bout.data <- datafile1()
      tagID.data<- bout.data %>%  
        select("tagID", "plotID") %>%   #selecting only tagID by plot data
        filter(!is.na(tagID))           #taking out NA's
      each_tag<- distinct(tagID.data)                           #looking for distinct tagID by plot
      duplicate_tags_per_bout<- each_tag[duplicated(each_tag),] #then looking for duplicates w/ same species and different plot
      Dup<- c()
      if (length(duplicate_tags_per_bout$tagID) > 0) {
        Dup<- c(Dup,print(paste0(duplicate_tags_per_bout, " - duplicated tagID within current bout data - on multiple plots")))
      }
      Dup
    })
    output$NEONTagDuplicates <- renderTable({
      #--Previous code to get variable needed to compare tagIDs 'each_tag'
      bout.data <- datafile1()
      tagID.data<- bout.data %>%  
        select("tagID", "plotID") %>%   #selecting only tagID by plot data
        filter(!is.na(tagID))           #taking out NA's
      each_tag<- distinct(tagID.data)
      #--end of previous code--
      #---looking for bout tag IDs compared to all data---
      mam.raw <- site_select()
      all.tagID.data<- mam.raw %>%  
        select("tagID", "plotID") %>%   #selecting only tagID by plot data
        filter(!is.na(tagID))           #taking out NA's
      all_tags<- distinct(all.tagID.data)                   #looking for distinct tagID by plot
      all_duplicate_tags<- all_tags[duplicated(all_tags),]  #then looking for duplicates tags w/ different plot
      tags_used_before<- intersect(each_tag,all_tags)       #comparing tags in NEON portal to current bout tags
      DupTags<- c()
      if (length(tags_used_before$tagID) > 0) {
        DupTags<- c(DupTags,print(paste0(tags_used_before$tagID, " - duplicated tagID used on NEON data portal already")))
      }
      DupTags
    })
    #---get recap- checking if sex/species the same for tagID (summary)---------------------------------------------------
    output$SexID <- renderTable({
      #For bout uploaded data
      bout.data <- datafile1()
      IDsex<- bout.data %>%
        group_by(sex,tagID) %>%  #grouping by sex and tagID
        summarise(count = n())    #counting number of times called male/female
      id_sex<- IDsex[duplicated(IDsex$tagID),]   #checking for tags called multiple sexes for current bout
      select_sexID<<- distinct(IDsex) %>%          #selecting tagID to compare with NEON data
        select("tagID", "sex")
      DupS<- c()
      if (length(id_sex$tagID) > 0) {
        DupS<- c(DupS,print(paste0(id_sex, " - called different 'sex' within current bout data")))
      }
      DupS
    })
    output$NEONSexID <- renderTable({
      #--Previous code to get variable needed to compare sexIDs 'select_sexID'
      bout.data <- datafile1()
      IDsex<- bout.data %>%
        group_by(sex,tagID) %>%  #grouping by sex and tagID
        summarise(count = n())    #counting number of times called male/female
      id_sex<- IDsex[duplicated(IDsex$tagID),]   #checking for tags called multiple sexes for current bout
      select_sexID<- distinct(IDsex) %>%          #selecting tagID to compare with NEON data
        select("tagID", "sex")
      #--end of previous code--
      mam.raw <- site_select()  #for NEON portal data
      NEON_IDsex<- mam.raw %>%
        group_by(sex,tagID) %>% #grouping by sex and tagID
        summarise(count = n())  #counting number of times called male/female
      multiple_sexIDs<- NEON_IDsex[duplicated(NEON_IDsex$tagID),]   #checking for tags called 'both'
      i=1
      per_tag<- list()  #want to assign one sex per tagID to compare to bout data - comparing highest count per 'sex'
      while (i <   length(multiple_sexIDs$tagID)+1) {         #looking at each species w/ multiple sex assigned
        per_tag[[i]]<- NEON_IDsex %>%                         #NEON data w/ sexID counts
          filter(tagID == multiple_sexIDs$tagID[i])
        if (per_tag[[i]]$count[[1]] > per_tag[[i]]$count[[2]]) {    #if tagID[1] 'sex' called more than other[2]
          a<- grep(per_tag[[i]]$tagID[1],NEON_IDsex$tagID, fixed=T) #looking for tagIDs with 'M' and 'F' 
          b<- NEON_IDsex[a,]                                        #tagID with multiple sexIDs
          NEON_IDsex<- NEON_IDsex[-a,]                              #getting rid of both
          c<- grep(per_tag[[i]]$sex[1], b$sex, fixed=T)             #looking for sex that has a smaller count
          corrected_sex<- b[-c,]                                    #taking out sex with lowest count
          NEON_IDsex<- union(corrected_sex, NEON_IDsex)             #added back the corrected 'sex' to NEON data
        }
        if (per_tag[[i]]$count[[1]] < per_tag[[i]]$count[[2]]) {    #if tagID[2] 'sex' called more than other[1]
          a<- grep(per_tag[[i]]$tagID[1],NEON_IDsex$tagID, fixed=T) #looking for tagIDs with 'M' and 'F' 
          b<- NEON_IDsex[a,]                                        #tagID with multiple sexIDs
          NEON_IDsex<- NEON_IDsex[-a,]                              #getting rid of both
          c<- grep(per_tag[[i]]$sex[2], b$sex, fixed=T)             #looking for sex that has a smaller count
          corrected_sex<- b[-c,]                                    #taking out sex with lowest count
          NEON_IDsex<- union(corrected_sex, NEON_IDsex)             #added back the corrected 'sex' to NEON data
        }
        if (per_tag[[i]]$count[[1]] == per_tag[[i]]$count[[2]]) {   #if tagID 'sex' equal to other
          a<- grep(per_tag[[i]]$tagID[1],NEON_IDsex$tagID, fixed=T) #looking for tagIDs with 'M' and 'F' 
          NEON_IDsex<- NEON_IDsex[-a,]                              #taking out these tagIDs, not sure what 'sex' id is 
        }
        i=i+1
      }
      NEON_IDsex<- NEON_IDsex %>% 
        filter(sex != 'U') %>%  #taking out unknown 'sex' == 'U'
        select("tagID", "sex")  #formating to comind corrected 'sex' w/ NEON data
      #comparing to uploaded bout data vs NEON portal 
      binded_data<- union(NEON_IDsex,select_sexID)                      #union gets rid of duplicates
      final_sexID_check<- binded_data[duplicated(binded_data$tagID),]   #saving any duplicated tagIDs- only present if multiple sexIDs 
      NDupS<- c()
      if (length(final_sexID_check$tagID) > 0) {
        NDupS<- c(NDupS,print(paste0(final_sexID_check$tagID, " - called different 'sex' compared to NEON data portal info")))
      }
      NDupS
    })
    #---New species per plot---
    output$newspecies <- renderTable({
      bout.data <- datafile1()
      mam.raw <- site_select()
      #Saving all traps with speciesID present from bout data - taking out missing data
      bout.mam.captures<- bout.data %>%
        filter(!is.na(taxonID))         # remove missing taxonID
      #Saving all traps with speciesID present NEON from portal data
      mam.captures<- mam.raw %>%
        filter(!is.na(taxonID))         # remove missing taxonID
      #Creating table of plots for data that was uploaded
      plot<- bout.data %>% 
        count(plotID)
      plot<- plot$plotID

      i=1
      per_plot<-list()
      while (i<7) {                               #seperating out plots for NEON data
        per_plot[[i]] <- mam.captures %>% 
          filter(plotID == plot[[i]])
        i=i+1 
      }
      i=1
      n.per_plot<- list()
      while (i<7) {                               #seperating out plots for bout data
        n.per_plot[[i]] <- bout.mam.captures %>% 
          filter(plotID == plot[[i]])
        i=i+1 
      } 
      #---looking for species 'new' to plot
      new_animal<- c()
      i=1
      while (i<7) {
        individual=1
        new_species<- setdiff(n.per_plot[[i]]$taxonID,per_plot[[i]]$taxonID)
        for (individual in seq_along(new_species)) {
          #'^' signifies start while '$' signifies end of what looking for
          finding<- grep(paste0("^",new_species[[individual]],"$"), n.per_plot[[i]]$taxonID)
          h<- n.per_plot[[i]][finding,]
          new_animal<- c(new_animal,print(paste0("New species '",h$scientificName,"' found on plot ",h$plotID," - ",h$collectDate," - ",h$tagID)))
        }
        i=i+1
      }
      new_animal
    })
    #---Checking HF and W measurements w/ known ranges---
    output$HFmeasurements <- renderTable({
      bout.data <- datafile1()
      measurements <- datafile2()
      hindfoot_range<- list()
      finalHF<- c()
      i=1
      while (i < length(measurements$Species)+1) {
        hindfoot_range[[i]]<- bout.data %>%
          filter(taxonID == (measurements$Species[i])) %>% 
          filter(!between(hindfootLength, measurements$HF_min[i], measurements$HF_max[i]))
        ind=1
        for(ind in seq_along(hindfoot_range[[i]][[ind]])) {
          finalHF<- c(finalHF,print(paste0("Out of Range - ",hindfoot_range[[i]]$taxonID[[ind]]," ", hindfoot_range[[i]]$tagID[[ind]], " listed at ",hindfoot_range[[i]]$hindfootLength[[ind]],"mm for hindfoot length")))
        }
        i=i+1
      }
      finalHF
    })    
    output$Wmeasurements <- renderTable({
      bout.data <- datafile1()
      measurements <- datafile2()
      weight_range<- list()
      finalW<- c()
      i=1
      while (i < length(measurements$Species)+1) {
        weight_range[[i]]<- bout.data %>%
          filter(taxonID == (measurements$Species[i])) %>% 
          filter(!between(weight, measurements$W_min[i], measurements$W_max[i]))
        ind=1
        for(ind in seq_along(weight_range[[i]][[ind]])) {
          finalW<- c(finalW,print(paste0("Out of Range - ",weight_range[[i]]$taxonID[[ind]]," ", weight_range[[i]]$tagID[[ind]], " listed at ",weight_range[[i]]$weight[[ind]],"g for weight")))
        }
        i=i+1
      }
      finalW
    })
    #---ploting each species HF vs W to look for outliers- prints from current data
    output$MAMdata <- renderPlotly({
      bout.data <- datafile1()
      bout.mam.captures<- bout.data %>%
        filter(!is.na(taxonID))         # remove missing taxonID  
      #ploting out current MAM bout measurements by species
      plot1<- plot_ly(data=bout.mam.captures,
                      type='scatter',
                      mode='markers',
                      x=~hindfootLength,
                      y=~weight,
                      color=~taxonID,
                      alpha = .7,
                      #create custom hovertext
                      text=~paste0("Taxon: ",taxonID,' TagID: ',tagID,'\n','HF Length: ',hindfootLength," Weight: ",weight,'\n',"   Date: ",collectDate, " SciName: ",scientificName), 
                      hoverinfo='text'
      )%>%
        layout(title='Measurement Check- Bout Data',yaxis=list(title='Weight (g)'),xaxis=list(title='HF Length (mm)'))
    })
    #---ploting each species HF vs W to look for outliers- prints form all data
    output$MAMdataNEON <- renderPlotly({
      mam.raw <- site_select()
      #Saving all traps with speciesID present NEON from portal data
      NEON.mam.captures<- mam.raw %>%
        filter(!is.na(taxonID))         # remove missing taxonID
      #ploting out current MAM bout measurements by species
      plot2<- plot_ly(data=NEON.mam.captures,
                      type='scatter',
                      mode='markers',
                      x=~hindfootLength,
                      y=~weight,
                      color=~taxonID,
                      alpha = .7,
                      #create custom hovertext
                      text=~paste0("Taxon: ",taxonID,' TagID: ',tagID,'\n','HF Length: ',hindfootLength," Weight: ",weight,'\n',"   Date: ",collectDate, " SciName: ",scientificName), 
                      hoverinfo='text'
      )%>%
        layout(title='Measurement Check - All Data',yaxis=list(title='Weight (g)'),xaxis=list(title='HF Length (mm)'))
    })
  }
  shinyApp(ui, server)
}
