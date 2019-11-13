library(shiny)
library(ggplot2)
library(DT)

Fall18NewFreshman = as.data.frame(read.csv("Fall18NewFreshman.csv"))
Fall19NewFreshman = as.data.frame(read.csv("Fall19NewFreshman.csv"))
Fall20NewFreshman = as.data.frame(read.csv("Fall20NewFreshman.csv"))

Fall18NewFreshman$DaysBeforeStartTerm = 0
Fall19NewFreshman$DaysBeforeStartTerm = 0
Fall20NewFreshman$DaysBeforeStartTerm = 0

Fall18NewFreshman$ENROLL_CURRENT_DATE = as.Date(as.character(Fall18NewFreshman$ENROLL_CURRENT_DATE),"%Y%m%d")
Fall19NewFreshman$ENROLL_CURRENT_DATE = as.Date(as.character(Fall19NewFreshman$ENROLL_CURRENT_DATE),"%Y%m%d")
Fall20NewFreshman$ENROLL_CURRENT_DATE = as.Date(as.character(Fall20NewFreshman$ENROLL_CURRENT_DATE),"%Y%m%d")

Fall18NewFreshman$DaysBeforeStartTerm = -as.integer(as.Date("2018-08-29","%Y-%m-%d") - as.Date(Fall18NewFreshman$ENROLL_CURRENT_DATE))
Fall19NewFreshman$DaysBeforeStartTerm = -as.integer(as.Date("2019-08-28","%Y-%m-%d") - as.Date(Fall19NewFreshman$ENROLL_CURRENT_DATE))
Fall20NewFreshman$DaysBeforeStartTerm = -as.integer(as.Date("2020-08-27","%Y-%m-%d") - as.Date(Fall20NewFreshman$ENROLL_CURRENT_DATE))

days_before = Fall20NewFreshman[nrow(Fall20NewFreshman), 'DaysBeforeStartTerm']
day_index_20 = which(Fall20NewFreshman$DaysBeforeStartTerm == days_before)
day_index_19 = which(Fall19NewFreshman$DaysBeforeStartTerm == days_before)
day_index_18 = which(Fall18NewFreshman$DaysBeforeStartTerm == days_before)

days_back = 30 - 1
first_diff_df = data.frame(matrix(ncol = 0, nrow = days_back + 1))
first_diff_df$DaysBeforeStartTerm = Fall20NewFreshman[(day_index_20 - days_back):day_index_20, ]$DaysBeforeStartTerm

first_diff_df$Freshmen_Applied_2020 = Fall20NewFreshman[(day_index_20 - days_back):day_index_20, ]$FRESHMEN_APPLIED
first_diff_df$AVG_Applied_Past = (Fall18NewFreshman[(day_index_18 - days_back):day_index_18, ]$FRESHMEN_APPLIED + Fall19NewFreshman[(day_index_19 - days_back):day_index_19, ]$FRESHMEN_APPLIED) / 2
first_diff_df$Applied_Difference = first_diff_df$Freshmen_Applied_2020 - first_diff_df$AVG_Applied_Past

first_diff_df$Freshmen_Admitted_2020 = Fall20NewFreshman[(day_index_20 - days_back):day_index_20, ]$FRESHMEN_ADMITTED
first_diff_df$AVG_Admitted_Past = (Fall18NewFreshman[(day_index_18 - days_back):day_index_18, ]$FRESHMEN_ADMITTED + Fall19NewFreshman[(day_index_19 - days_back):day_index_19, ]$FRESHMEN_ADMITTED) / 2
first_diff_df$Admitted_Difference = first_diff_df$Freshmen_Admitted_2020 - first_diff_df$AVG_Admitted_Past

second_diff_df = data.frame(matrix(ncol = 0, nrow = days_back + 1))
second_diff_df$DaysBeforeStartTerm = Fall20NewFreshman[(day_index_20 - days_back):day_index_20, ]$DaysBeforeStartTerm

second_diff_df$Freshmen_Confirmed_2020 = Fall20NewFreshman[(day_index_20 - days_back):day_index_20, ]$FRESHMEN_CONFIRMED
second_diff_df$AVG_Confirmed_Past = (Fall18NewFreshman[(day_index_18 - days_back):day_index_18, ]$FRESHMEN_CONFIRMED + Fall19NewFreshman[(day_index_19 - days_back):day_index_19, ]$FRESHMEN_CONFIRMED) / 2
second_diff_df$Confirmed_Difference = second_diff_df$Freshmen_Confirmed_2020 - second_diff_df$AVG_Confirmed_Past

second_diff_df$Freshmen_Registered_2020 = Fall20NewFreshman[(day_index_20 - days_back):day_index_20, ]$FRESHMEN_REGISTERED
second_diff_df$AVG_Registered_Past = (Fall18NewFreshman[(day_index_18 - days_back):day_index_18, ]$FRESHMEN_REGISTERED + Fall19NewFreshman[(day_index_19 - days_back):day_index_19, ]$FRESHMEN_REGISTERED) / 2
second_diff_df$Registered_Difference = second_diff_df$Freshmen_Registered_2020 - second_diff_df$AVG_Registered_Past

shinySserver = function(input,output){

  output$plot_applied = renderPlot({
    ggplot() + 
      geom_line(data = Fall18NewFreshman[(day_index_18 - 30):day_index_18, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_APPLIED, color = "Fall 18"),size = 2) +
      geom_line(data = Fall19NewFreshman[(day_index_19 - 30):day_index_19, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_APPLIED, color = "Fall 19"),size = 2) +
      geom_line(data = Fall20NewFreshman[(day_index_20 - 30):day_index_20, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_APPLIED, color = "Fall 20") ,size = 2) +
      scale_color_manual(values = c("Fall 18"="dodgerblue4","Fall 19"="dodgerblue3", "Fall 20"="red")) +
      xlab('Days Until Start of Term (Past 30 Days)') +
      ylab('Students Applied') +
      labs(colour="Status") +
      ggtitle("Applied") +
      theme_bw() +
      theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15, face = "bold"),
            legend.title = element_text(size = 15),
            legend.text=element_text(size = 11),
            plot.title = element_text(size = 16, face = "bold")
      ) 
  })
  
  output$plot_admitted = renderPlot({
    ggplot() + 
      geom_line(data = Fall18NewFreshman[(day_index_18 - 30):day_index_18, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_ADMITTED, color = "Fall 18"),size = 2) +
      geom_line(data = Fall19NewFreshman[(day_index_19 - 30):day_index_19, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_ADMITTED, color = "Fall 19"),size = 2) +
      geom_line(data = Fall20NewFreshman[(day_index_20 - 30):day_index_20, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_ADMITTED, color = "Fall 20") ,size = 2) +
      scale_color_manual(values = c("Fall 18"="dodgerblue4","Fall 19"="dodgerblue3", "Fall 20"="red")) +
      xlab('Days Until Start of Term (Past 30 Days)') +
      ylab('Students Admitted') +
      labs(colour="Status") +
      ggtitle("Admitted") +
      theme_bw() +
      theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15, face = "bold"),
            legend.title = element_text(size = 15),
            legend.text=element_text(size = 11),
            plot.title = element_text(size = 16, face = "bold")
      ) 
  })
  
  output$plot_confirmed = renderPlot({
    ggplot() + 
      geom_line(data = Fall18NewFreshman[(day_index_18 - 30):day_index_18, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_CONFIRMED, color = "Fall 18"),size = 2) +
      geom_line(data = Fall19NewFreshman[(day_index_19 - 30):day_index_19, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_CONFIRMED, color = "Fall 19"),size = 2) +
      geom_line(data = Fall20NewFreshman[(day_index_20 - 30):day_index_20, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_CONFIRMED, color = "Fall 20") ,size = 2) +
      scale_color_manual(values = c("Fall 18"="dodgerblue4","Fall 19"="dodgerblue3", "Fall 20"="red")) +
      xlab('Days Until Start of Term (Past 30 Days)') +
      ylab('Students Confirmed') +
      labs(colour="Status") +
      ggtitle("Confirmed") +
      theme_bw() +
      theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15, face = "bold"),
            legend.title = element_text(size = 15),
            legend.text=element_text(size = 11),
            plot.title = element_text(size = 16, face = "bold")
      ) 
  })
  
  output$plot_registered = renderPlot({
    ggplot() + 
      geom_line(data = Fall18NewFreshman[(day_index_18 - 30):day_index_18, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_REGISTERED, color = "Fall 18"),size = 2) +
      geom_line(data = Fall19NewFreshman[(day_index_19 - 30):day_index_19, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_REGISTERED, color = "Fall 19"),size = 2) +
      geom_line(data = Fall20NewFreshman[(day_index_20 - 30):day_index_20, ], aes(x = DaysBeforeStartTerm, y = FRESHMEN_REGISTERED, color = "Fall 20") ,size = 2) +
      scale_color_manual(values = c("Fall 18"="dodgerblue4","Fall 19"="dodgerblue3", "Fall 20"="red")) +
      xlab('Days Until Start of Term (Past 30 Days)') +
      ylab('Students Registered') +
      labs(colour="Status") +
      ggtitle("Registered") +
      theme_bw() +
      theme(axis.text = element_text(size = 13), axis.title = element_text(size = 15, face = "bold"),
            legend.title = element_text(size = 15),
            legend.text=element_text(size = 11),
            plot.title = element_text(size = 16, face = "bold")
      ) 
  })
  
  output$plot_app_adm = renderPlot({
    ggplot() + 
      geom_line(data = Fall18NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_APPLIED, color = "Fall 18 Applied"), size = 2) +
      geom_line(data = Fall19NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_APPLIED, color = "Fall 19 Applied"),size = 2) +
      geom_line(data = Fall20NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_APPLIED, color = "Fall 20 Applied"),size = 2) +
      geom_line(data = Fall18NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_ADMITTED, color = "Fall 18 Admitted"), size = 2) +
      geom_line(data = Fall19NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_ADMITTED, color = "Fall 19 Admitted"),size = 2) +
      geom_line(data = Fall20NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_ADMITTED, color = "Fall 20 Admitted"),size = 2) +
      scale_color_manual(values = c("Fall 18 Applied"="blue3","Fall 19 Applied"="dodgerblue4", "Fall 20 Applied" = "red3",
                                    "Fall 18 Admitted"="blue","Fall 19 Admitted"="dodgerblue", "Fall 20 Admitted"="red")) +
      xlab('Days Prior to the Beginning of the Term') +
      xlim(-344,0)+
      ylab('Number of Students') +
      labs(colour="Status") +
      ggtitle("Students Applied and Admitted for Fall 2018 and Fall 2019") +
      
      theme_bw() +
      theme(axis.text=element_text(size=13), axis.title=element_text(size=15, face = "bold"),
            legend.title = element_text(size = 15),
            legend.text=element_text(size=11),
            plot.title = element_text( size = 16, face = "bold")
      )  
  })
  
  output$plot_conf_reg = renderPlot({
    ggplot() +   
      geom_line(data = Fall18NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_CONFIRMED, color = "Fall 18 Confirmed"), size = 2) +
      geom_line(data = Fall19NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_CONFIRMED, color = "Fall 19 Confirmed"),size = 2) +
      geom_line(data = Fall20NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_CONFIRMED, color = "Fall 20 Confirmed"),size = 2) +
      geom_line(data = Fall18NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_REGISTERED, color = "Fall 18 Registered"), size = 2) +
      geom_line(data = Fall19NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_REGISTERED, color = "Fall 19 Registered"),size = 2) +
      geom_line(data = Fall20NewFreshman, aes(x = DaysBeforeStartTerm, y = FRESHMEN_REGISTERED, color = "Fall 20 Registered"),size = 2) +
      scale_color_manual(values = c("Fall 18 Confirmed"="blue3","Fall 19 Confirmed"="dodgerblue4", "Fall 20 Confirmed"="red3",
                                    "Fall 18 Registered"="blue","Fall 19 Registered"="dodgerblue", "Fall 20 Registered"="red")) +
      xlab('Days Prior to the Beginning of the Term') +
      xlim(-344,0)+
      ylab('Number of Students') +
      labs(colour="Status") +
      ggtitle("Students Confirmed and Registered for Fall 2018 and Fall 2019") +
      
      theme_bw() +
      theme(axis.text=element_text(size=13), axis.title=element_text(size=15, face = "bold"),
            legend.title = element_text(size = 15),
            legend.text=element_text(size=11),
            plot.title = element_text( size = 16, face = "bold")
      )
  })
  
  output$app_adm_table = DT::renderDataTable({
    out = as.data.frame(first_diff_df[c(nrow(first_diff_df):1),])
    datatable(out) %>% formatStyle(
      c('Applied_Difference', 'Admitted_Difference'),
      backgroundColor = styleInterval(c(-1, 1 ), c('red', 'white', 'lime')),
      fontWeight = 'bold'
    )
  })
  
  output$conf_reg_table = DT::renderDataTable({
    out = as.data.frame(second_diff_df[c(nrow(second_diff_df):1),])
    datatable(out) %>% formatStyle(
      c('Confirmed_Difference', 'Registered_Difference'),
      backgroundColor = styleInterval(c(-1, 1 ), c('red', 'white', 'lime')),
      fontWeight = 'bold'
    )
  })
  
}