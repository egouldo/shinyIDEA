clean_spreadsheets_helper_fun <- function(raw_spreadsheet_dataframe) {
  dat <- raw_spreadsheet_dataframe %>% dplyr::select(1:7) %>%
    dplyr::rename(Question = 1, Title = 2, Low = 3, High = 4, Best = 5, Conf = 6, Com = 7) %>%
    dplyr::mutate(Code_Name = nth(Question, 2) %>%
                    str_split(., ":") %>%
                    purrr::simplify() %>%
                    .[2] %>% trimws()) %>%
    dplyr::slice(4:n()) %>%
    dplyr::select(Code_Name, Title, Low, High, Best, Question, Conf, Com) %>%
    dplyr::mutate(Question = as.numeric(gsub("[^0-9\\.]", "", Question)),
                  Low = as.numeric(gsub("[^0-9\\.]", "", Low)),
                  High = as.numeric(gsub("[^0-9\\.]", "", High)),
                  Best = as.numeric(gsub("[^0-9\\.]", "", Best)),
                  Conf = as.numeric(gsub("[^0-9\\.]", "", Conf)),
                  Com = as.character(gsub("[^[:alnum:]///' ]", "",  Com)))
  return(dat)
}

clean_collate_raw_data <- function(file_path = file.path(), StConf = 90, Quests = 10, Round = 1) {
  temp <- list.files(path=file_path, pattern="*.csv")
  cat(temp)
  Experts<-length(temp) #this is just set automatically to the number of .csv files in the temp folder.
  
  list2env(
    lapply(setNames(paste(file_path, "/", temp,sep=""), 
                    make.names(gsub("*.csv$", "", paste("./data/Round1/", temp, sep="")))
                    ),
           read.csv), envir = .GlobalEnv)
  # first dataset
  Data1 <- read.csv(paste(file_path, "/", temp[1], sep=""), na.strings="NA", as.is=T)
  Round1 <- clean_spreadsheets_helper_fun(Data1)
  Round1$dataset <- temp[1]
  
  # other datasets
  
  for (i in 2:Experts) {
    
    Data1 <- read.csv(paste(file_path, temp[i], sep=""), na.strings="NA", as.is=T)
    Data2 <- clean_spreadsheets_helper_fun(Data1)
    Data2$dataset <- temp[i]
    Round1 <- dplyr::bind_rows(Round1, Data2)
  }
  Round1 <- Round1 %>%
    dplyr::mutate(Code_Name = as.character(Code_Name) %>% trimws()) %>%
    as_tibble() %>%
    dplyr::mutate(Round = Round)
  
  return(Round1) # rename all objects appropriately here, not Round1
}

aggregate_data <- function(raw_collated_clean_data, StConf = 90) {
  
  dat <- raw_collated_clean_data %>%
    dplyr::mutate(Conf = ifelse(is.na(Conf), StConf, Conf)) %>% # adjust blank confidence intervals
    dplyr::group_by(Round, Question, Code_Name) %>%  #ensure correct ordering of responses
    tidyr::gather(key = estimate_type, value = value, Low, High, Best) %>%
    dplyr::arrange(Round, Question, Code_Name, value) %>%
    mutate(value = ifelse(estimate_type == "Low", min(value),
                          ifelse(estimate_type == "High", max(value),
                                 nth(value, 2)))) %>%
    # create standardised confidence intervals
    tidyr::spread(key = estimate_type, value = value) %>%
    mutate(LCI = (Best - Low) * (StConf / Conf),
           UCI = (High - Best) * (StConf / Conf),
           LC_90 = Best - LCI,
           UC_90 = Best + UCI,
           LC_90 = ifelse(LC_90 < 0, 0,LC_90),
           AggType = "Individuals")
  
  standardised_data <- dat %>%
    group_by(Round, Question, Title) %>%
    dplyr::summarise(Low = mean(LC_90),
                     Best = mean(Best),
                     High = mean(UC_90)) %>%
    mutate(Code_Name = "Group Average",
           AggType = "Mean",
           Conf = StConf)
  
  standardised_data <- standardised_data %>%
    dplyr::left_join(dplyr::select(dat, Round, Question, Code_Name, Com)) # Join comments back in
  
  Round1_raw_and_summarised_data <-
    dat %>% dplyr::select(Round, Question, Title, Code_Name, AggType, Low, Best, High, Conf, Com) %>%
    bind_rows(standardised_data)
  return(Round1_raw_and_summarised_data)
}

plot_estimates <- function(aggregated_estimates_data, StConf = 90) {
  # Basic Plot
  p <- aggregated_estimates_data %>%
    ggplot(aes(x = Code_Name, y = Best, ymin = Low, ymax = High, colour = AggType)) +
    geom_errorbar(position = position_dodge(width = .75), width = 0.1, lwd=0.6, aes(linetype= AggType)) +
    facet_grid(cols = vars(AggType), scales = "free_x", space="free")
  # Add formatting
  p <- p +
    scale_linetype_manual(values = c("dashed", "dashed"),
                          breaks=c("1",  "3" ),
                          labels=c("R1 Exp", "R1 Agg")) +
    geom_point(position = position_dodge(width = .75),
               aes(shape= AggType, fill= AggType), size = 2) +
    scale_colour_manual(values= c("black", "black"),
                        breaks=c("1", "3"),
                        labels=c("R1 Exp", "R1 Agg")) +
    scale_fill_manual(values= c("turquoise", "red"),
                      breaks=c("1",  "3"),
                      labels=c("R1 Exp", "R1 Agg")) +
    scale_shape_manual(values= c(21, 21),
                       breaks=c("1",  "3"),
                       labels=c("R1 Exp","R1 Agg")) +
    theme(legend.text = element_text (size= 12)) +
    xlab("Participants") +
    ylab(paste0("Estimates (", StConf, "% CI)")) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5, size=10)) +
    theme(axis.text.y = element_text(size=10)) +
    theme(axis.title.y = element_text(vjust= 0.1, size= 10)) +
    theme(legend.position = "bottom",
          legend.background = element_rect(fill = "white", colour = NA))
  
  return(p)
}

table_estimates <- function(aggregated_estimates_data) {
  panderOptions('knitr.auto.asis', FALSE)
  panderOptions('table.split.table', Inf)
  panderOptions('digits',2)
  aggregated_estimates_data %>%
    dplyr::rename(Name = Code_Name, Lower = Low, Upper = High, Comments = Com) %>%
    dplyr::select(Name, Round, Lower, Upper, Best, Conf, Comments, AggType) %>%
    pander(justify='left')
}

print_function <- function(plot, data, question, label) {
  cat("\n##Question", question, ":", label, "?\n")
  print(plot)
  cat("\n")
  pluck(data) %>% table_estimates(.)
  cat("\n\n\\pagebreak\n")
}