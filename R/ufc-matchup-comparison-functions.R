#---------------------------------------------------------------------------------------------
# BUILD UFC MATCHUP COMPARISON FIGURE
#---------------------------------------------------------------------------------------------

UFC_Matchup_Comparison <- function(career_df, recent_df, f1_name, f2_name){
  
  #-----------
  # PREP DATA
  #-----------
  
  # subset data
  df <- career_df[Name %in% c(f1_name, f2_name)]
  stopifnot(df[, uniqueN(Link)] == 2)
  
  recent_df <- recent_df[Name %in% c(f1_name, f2_name)]
  stopifnot(recent_df[, uniqueN(Link)] == 2)
  
  # fill NAs
  df[is.na(Takedown_Defense), Takedown_Defense := 1]
  df[is.na(df)] <- 0
  
  # format data
  df[, Name := factor(Name, levels = c(f1_name, f2_name))]
  
  recent_df[, Name := factor(Name, levels = c(f1_name, f2_name))]
  recent_df[, Date := format.Date(Date, format = '%b %d, %Y')]
  recent_df[, Result_clean := fcase(Result_clean == 'Knockout', 'KO', 
                                 Result_clean == 'Submission', 'SUB', 
                                 grepl('Decision', Result_clean), 'DEC', 
                                 Result_clean == 'Other', 'Other')]
  recent_df[F1_Win == T, Outcome := paste0('Win - R', EndRound, ' ', Result_clean)]
  recent_df[F1_Win == F & F1_Draw == F, Outcome := paste0('Loss - R', EndRound, ' ', Result_clean)]
  recent_df[F1_Draw == T, Outcome := paste0('Draw - R', EndRound)]
  recent_df <- recent_df[, .(Date, Name, Opponent, Outcome)]
  recent_df[is.na(recent_df)] <- '-'
  
  # split data
  f1_recent_df <- recent_df[Name == f1_name]
  f1_recent_df[, Name := NULL]
  f2_recent_df <- recent_df[Name == f2_name]
  f2_recent_df[, Name := NULL]
  
  # set records
  f1_record <- df[Name == f1_name, paste0('(', Wins, '-', Losses, '-', Draws, ')')]
  f2_record <- df[Name == f2_name, paste0('(', Wins, '-', Losses, '-', Draws, ')')]
  
  # ensure proper number of rows
  n_recent <- 5
  
  f1_recent_df <- f1_recent_df[1:n_recent]
  f1_recent_df[is.na(f1_recent_df)] <- '-'
  f2_recent_df <- f2_recent_df[1:n_recent]
  f2_recent_df[is.na(f2_recent_df)] <- '-'
  
  #-----------------------
  # SET FIGURE PARAMETERS
  #-----------------------
  
  # set figure parameters
  f1_color <- brewer.pal(6, 'Paired')[6]
  f2_color <- brewer.pal(6, 'Paired')[2]
  f1_loss_color <- brewer.pal(6, 'Paired')[5]
  f2_loss_color <- brewer.pal(6, 'Paired')[1]
  
  fig_font_color <- 'white'
  fig_bg_color <- 'black'
  custom_theme <- theme(plot.title = element_text(size = 12, hjust = 0, face = 'bold'), 
                        plot.subtitle = element_text(size = 11, hjust = 0), 
                        axis.text = element_text(size = 10, color = fig_font_color), 
                        legend.text = element_text(size = 10), 
                        legend.key = element_blank(), 
                        plot.title.position = 'plot', 
                        plot.margin = margin(0.2, 0.2, 0.0, 0.2, 'cm'), 
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border = element_blank(),
                        panel.background = element_blank())
  
  #------------------------------------
  # FIGURE 1: aggregate fight outcomes
  #------------------------------------
  
  # aggregate fight outcomes
  plot_df <- df[, .(Fighter = Name, 
                    'SUB % of Losses' = SUBd_Pct_of_Ls, 
                    'KO % of Losses' = KOd_Pct_of_Ls, 
                    'SUB % of Wins' = SUB_Pct_of_Ws, 
                    'KO % of Wins' = KO_Pct_of_Ws, 
                    'UFC Win %' = Win_Pct)]
  
  # reshape and format data
  plot_df <- melt(plot_df, id.vars = 'Fighter')
  plot_df[, Fighter := factor(Fighter, levels = c(f2_name, f1_name))]
  plot_df[, Win := factor(grepl('Win', variable), levels = c(T, F))]
  
  # generate plot
  p1 <- ggplot(plot_df, aes(x = variable, y = value, color = Fighter, fill = Fighter, pattern = Win)) + 
    geom_bar_pattern(stat = 'identity', width = .7, position = position_dodge(width = .8), pattern_size = 2, pattern_color = 'black') + 
    labs(title = 'Aggregate UFC Fight Outcomes', 
         x = '', 
         y = '') + 
    scale_pattern_discrete(breaks = c(f1_name, f2_name), choices = c('none', 'stripe')) + 
    scale_y_continuous(breaks = seq(0, 1, .25), limits = c(0, 1.12), label = percent) + 
    geom_text(aes(x = variable, y = value, label = paste0(100 * round2(value, 2), '%')), size = 3, fontface = 'bold', hjust = -.2, 
              position = position_dodge(width = .8), show.legend = F) + 
    scale_color_manual(breaks = c(f1_name, f2_name), 
                       values = c(f1_color, f2_color), 
                       guide = NULL) + 
    scale_fill_manual(breaks = c(f1_name, f2_name), 
                      values = c(f1_color, f2_color), 
                      guide = NULL) + 
    coord_flip() + 
    dark_theme_gray() + 
    theme(legend.position = 'None', 
          legend.title = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank()) + 
    custom_theme
  
  #-----------------------------------
  # FIGURE 2: detailed fight outcomes
  #-----------------------------------
  
  # aggregate fight outcomes
  plot_df <- df[, .(Fighter = Name, 
                    'Decision Losses' = UDEC_Losses + SDEC_Losses, 
                    'Round 2+ Finish Losses' = R2_Finished + R3plus_Finished, 
                    'Round 1 Finish Losses' = R1_Finished, 
                    'Decision Wins' = UDEC_Wins + SDEC_Wins, 
                    'Round 2+ Finish Wins' = R2_Finishes + R3plus_Finishes, 
                    'Round 1 Finish Wins' = R1_Finishes)]
  
  # reshape and format data
  plot_df <- melt(plot_df, id.vars = 'Fighter')
  plot_df[, Fighter := factor(Fighter, levels = c(f2_name, f1_name))]
  plot_df[grepl('Win', variable), WL := 'Wins']
  plot_df[grepl('Loss', variable), WL := 'Losses']
  plot_df[, WL := factor(WL, levels = c('Wins', 'Losses'))]
  
  # refactor variable
  plot_df[grepl('Wins', variable), variable := paste0(' ', variable)]
  plot_df[, variable := gsub(' Wins| Losses', '', variable)]
  plot_df[, variable := factor(variable, 
                               levels = c('Decision', 
                                          'Round 2+ Finish', 
                                          'Round 1 Finish', 
                                          ' Decision', 
                                          ' Round 2+ Finish', 
                                          ' Round 1 Finish'))]
  
  # create character value labels
  plot_df[, value_str := as.character(value)]
  plot_df[nchar(value_str) == 1, value_str := paste0(' ', value_str)]
  
  # generate plot
  p2 <- ggplot(plot_df, aes(x = variable, y = value, color = Fighter, fill = Fighter, lty = WL)) + 
    geom_linerange(aes(xmin = variable, xmax = variable, ymin = 0, ymax = value, lty = WL), 
                   position = position_dodge(width = .7), size = 1.5, key_glyph = 'path') + 
    labs(title = 'Detailed UFC Fight Outcomes', 
         x = '', 
         y = '') + 
    scale_y_continuous(limits = c(0, plot_df[, max(value) * 1.1])) + 
    geom_text(aes(x = variable, y = value, label = value_str), size = 3, fontface = 'bold', hjust = -.6, 
              position = position_dodge(width = .7), show.legend = F) + 
    scale_color_manual(breaks = c(f1_name, f2_name), 
                       values = c(f1_color, f2_color), 
                       guide = NULL) + 
    scale_fill_manual(breaks = c(f1_name, f2_name), 
                      values = c(f1_color, f2_color), 
                      guide = NULL) + 
    scale_linetype_manual(breaks = c('Wins', 'Losses'), 
                          values = c(1, 3)) + 
    coord_flip() + 
    dark_theme_gray() + 
    theme(legend.position = 'right', 
          legend.title = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank()) + 
    custom_theme
  
  #--------------------------------
  # FIGURE 3: strength of schedule
  #--------------------------------
  
  # create table for figure
  plot_df <- df[, .(Name, Opp_Win_Pct_AmongWins, Opp_Win_Pct_AmongLosses)]
  
  # reshape and format data
  plot_df <- melt(plot_df, id.vars = 'Name')
  plot_df[, Name := factor(Name, levels = c(f2_name, f1_name))]
  plot_df[grepl('Win', variable), WL := 'Among UFC Wins']
  plot_df[grepl('Loss', variable), WL := 'Among UFC Losses']
  plot_df[, WL := factor(WL, levels = c('Among UFC Losses', 'Among UFC Wins'))]
  
  # collect opponent records
  f1_opp_record <- df[Name == f1_name, unique(Opp_Record)]
  f2_opp_record <- df[Name == f2_name, unique(Opp_Record)]
  
  f1_opp_record_wins <- df[Name == f1_name, unique(Opp_Record_AmongWins)]
  f2_opp_record_wins <- df[Name == f2_name, unique(Opp_Record_AmongWins)]
  
  f1_opp_record_losses <- df[Name == f1_name, unique(Opp_Record_AmongLosses)]
  f2_opp_record_losses <- df[Name == f2_name, unique(Opp_Record_AmongLosses)]
  
  # generate 1st figure
  p31 <- ggplot() + 
    # fighter 1 boxes
    geom_rect(aes(xmin = 16 - 0.3, xmax = 16 + nchar(f1_opp_record) - 0.7, ymin = 39, ymax = 50 - .5), fill = 'white') + 
    geom_rect(aes(xmin = 16 - 0.3, xmax = 16 + nchar(f1_opp_record) - 0.7, ymin = 19, ymax = 30 - .5), fill = 'white') + 
    geom_rect(aes(xmin = 16 - 0.3, xmax = 16 + nchar(f1_opp_record) - 0.7, ymin = -1, ymax = 9), fill = 'white') + 
    # fighter 2 boxes
    geom_rect(aes(xmin = 26 - 0.3, xmax = 26 + nchar(f2_opp_record) - 0.7, ymin = 39, ymax = 50 - .5), fill = 'white') + 
    geom_rect(aes(xmin = 26 - 0.3, xmax = 26 + nchar(f2_opp_record) - 0.7, ymin = 19, ymax = 30 - .5), fill = 'white') + 
    geom_rect(aes(xmin = 26 - 0.3, xmax = 26 + nchar(f2_opp_record) - 0.7, ymin = -1, ymax = 9), fill = 'white') + 
    # opponent record labels
    annotate('text', x = 0, y = 45, label = 'All UFC Bouts', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    annotate('text', x = 0, y = 25, label = 'Among UFC Wins', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    annotate('text', x = 0, y = 5, label = 'Among UFC Losses', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    # fighter 1 opponent records
    annotate('text', x = 16, y = 45, label = f1_opp_record, size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    annotate('text', x = 16, y = 25, label = f1_opp_record_wins, size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    annotate('text', x = 16, y = 5, label = f1_opp_record_losses, size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    # fighter 2 opponent records
    annotate('text', x = 26, y = 45, label = f2_opp_record, size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    annotate('text', x = 26, y = 25, label = f2_opp_record_wins, size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    annotate('text', x = 26, y = 5, label = f2_opp_record_losses, size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    # other geoms
    labs(x = '', y = '', title = 'Strength of UFC Schedule', subtitle = 'Total UFC Record of Opponents') + 
    scale_x_continuous(limits = c(0, 50), labels = NULL) + 
    scale_y_continuous(limits = c(-1, 51), labels = NULL) + 
    dark_theme_gray() + 
    custom_theme + 
    theme(plot.margin = margin(0, 0.2, 0.0, 0.2, 'cm'), 
          axis.ticks = element_blank(), 
          axis.text = element_blank(), 
          axis.title = element_blank())
  
  # generate 2nd figure
  p32 <- ggplot(plot_df, aes(x = WL, y = value, color = Name, fill = Name, lty = WL)) + 
    geom_linerange(aes(xmin = WL, xmax = WL, ymin = 0, ymax = value, lty = WL), 
                   position = position_dodge(width = .7), size = 1.5, key_glyph = 'path') + 
    labs(title = '', 
         subtitle = 'Total UFC Win % of Opponents', 
         x = '', 
         y = '') + 
    scale_y_continuous(limits = c(0, 1.12)) + 
    geom_text(aes(x = WL, y = value, label = paste0(100 * round2(value, 2), '%')), size = 3, fontface = 'bold', hjust = -.6, 
              position = position_dodge(width = .7), show.legend = F) + 
    scale_color_manual(breaks = c(f1_name, f2_name), 
                       values = c(f1_color, f2_color), 
                       guide = NULL) + 
    scale_fill_manual(breaks = c(f1_name, f2_name), 
                      values = c(f1_color, f2_color), 
                      guide = NULL) + 
    scale_linetype_manual(breaks = c('Among UFC Wins', 'Among UFC Losses'), 
                          values = c(1, 3), 
                          guide = NULL) + 
    coord_flip() + 
    dark_theme_gray() + 
    theme(legend.position = 'right', 
          legend.title = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank()) + 
    custom_theme + 
    theme(plot.margin = margin(0, 0.2, 0.2, 0.2, 'cm'), 
          axis.title = element_blank())
  
  # combine figures
  p3 <- ggarrange(p31, p32, nrow = 2, ncol = 1)
  
  #-------------------
  # FIGURE 4: control
  #-------------------
  
  # create table for figure
  plot_df <- df[, .(Name, Fight_Minutes, Distance_Pct, Control_Pct, Controlled_Pct, Control_Rate)]
  plot_df[, Fight_Minutes_str := formatC(round2(Fight_Minutes, 0), format = 'd', big.mark = ',')]
  plot_df[, Control_Rate_str := paste0(100 * round2(Control_Rate, 2), '%')]
  
  # collect lengths of strings
  f1_nchar <- plot_df[Name == f1_name, pmax(nchar(Fight_Minutes_str), nchar(Control_Rate_str))]
  f2_nchar <- plot_df[Name == f2_name, pmax(nchar(Fight_Minutes_str), nchar(Control_Rate_str))]
  
  # generate 1st figure
  p41 <- ggplot() + 
    # boxes
    geom_rect(aes(xmin = 16 - 0.3, xmax = 16 + f1_nchar + 1 - 0.6, ymin = 20 - 1.5, ymax = 30 + .6), fill = 'white') + 
    geom_rect(aes(xmin = 16 - 0.3, xmax = 16 + f1_nchar + 1 - 0.6, ymin = -1 - .5, ymax = 10 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 23 - 0.3, xmax = 23 + f2_nchar + 1 - 0.6, ymin = 20 - 1.5, ymax = 30 + .6), fill = 'white') + 
    geom_rect(aes(xmin = 23 - 0.3, xmax = 23 + f2_nchar + 1 - 0.6, ymin = -1 - .5, ymax = 10 + .7), fill = 'white') + 
    # statistic labels
    annotate('text', x = 0, y = 25.5, label = 'Total Fight Minutes', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    annotate('text', x = 0, y = 5.5, label = 'Control Rate', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    # fighter 1 stats
    annotate('text', x = 16, y = 25.5, label = plot_df[Name == f1_name, Fight_Minutes_str], size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    annotate('text', x = 16, y = 5.5, label = plot_df[Name == f1_name, Control_Rate_str], size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    # fighter 2 stats
    annotate('text', x = 23, y = 25.5, label = plot_df[Name == f2_name, Fight_Minutes_str], size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    annotate('text', x = 23, y = 5.5, label = plot_df[Name == f2_name, Control_Rate_str], size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    # other geoms
    scale_x_continuous(limits = c(0, 50), labels = NULL) + 
    scale_y_continuous(limits = c(-2, 36), labels = NULL) + 
    labs(x = '', y = '', title = 'Control Statistics', subtitle = 'Control Rate = Control Time / (Control + Controlled Time)') + 
    dark_theme_gray() + 
    custom_theme + 
    theme(plot.margin = margin(0, 0.2, 0.0, 0.2, 'cm'), 
          axis.ticks = element_blank(), 
          axis.text = element_blank(), 
          axis.title = element_blank())
  
  # generate 2nd figure
  plot_df <- melt(plot_df[, .(Name, 'Controlled %' = Controlled_Pct, 'Control %' = Control_Pct, 'Distance %' = Distance_Pct)], id.vars = 'Name')
  plot_df[, Name := factor(Name, levels = c(f2_name, f1_name))]
  
  p42 <- ggplot(plot_df, aes(x = variable, y = value, color = Name, fill = Name, lty = variable)) + 
    geom_linerange(aes(xmin = variable, xmax = variable, ymin = 0, ymax = value, lty = variable), 
                   position = position_dodge(width = .7), size = 1.5, key_glyph = 'path') + 
    labs(title = '', 
         subtitle = 'Partitioning Total Fight Minutes', 
         x = '', 
         y = '') + 
    scale_y_continuous(limits = c(0, 1.12)) + 
    geom_text(aes(x = variable, y = value, label = paste0(100 * round2(value, 2), '%')), size = 3, fontface = 'bold', hjust = -.6, 
              position = position_dodge(width = .7), show.legend = F) + 
    scale_color_manual(breaks = c(f1_name, f2_name), 
                       values = c(f1_color, f2_color), 
                       guide = NULL) + 
    scale_fill_manual(breaks = c(f1_name, f2_name), 
                      values = c(f1_color, f2_color), 
                      guide = NULL) + 
    scale_linetype_manual(breaks = c('Distance %', 'Control %', 'Controlled %'), 
                          values = c(1, 1, 3), 
                          guide = NULL) + 
    coord_flip() + 
    dark_theme_gray() + 
    theme(legend.position = 'right', 
          legend.title = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank()) + 
    custom_theme + 
    theme(plot.margin = margin(0, 0.2, 0.2, 0.2, 'cm'), 
          axis.title = element_blank())
  
  # combine figures
  p4 <- ggarrange(p41, p42, nrow = 2, ncol = 1, heights = c(1, 1.5))
  
  #-----------------------------
  # FIGURE 5: distance striking
  #-----------------------------
  
  # create table for figure
  plot_df <- df[, .(Name, Dist_SS_Diff_pDM, SS_Dist_Accuracy, SS_Land_Dist_Pct, Dist_SS_Landed_pDM, Dist_SS_Abosrbed_pDM)]
  plot_df[, Dist_SS_Diff_pDM := as.character(format(round2(Dist_SS_Diff_pDM, 1), nsmall = 1))]
  plot_df[as.numeric(Dist_SS_Diff_pDM) > 0, Dist_SS_Diff_pDM := gsub(' ', '', paste0('+', Dist_SS_Diff_pDM))]
  plot_df[, SS_Dist_Accuracy := paste0(100 * round2(SS_Dist_Accuracy, 2), '%')]
  plot_df[, SS_Land_Dist_Pct := paste0(100 * round2(SS_Land_Dist_Pct, 2), '%')]
  
  # collect lengths of strings
  f1_nchar <- plot_df[Name == f1_name, pmax(nchar(Dist_SS_Diff_pDM) + .1 * as.numeric(plot_df[Name == f1_name & grepl('\\+', Dist_SS_Diff_pDM), .N] > 0), 
                                            nchar(SS_Dist_Accuracy), 
                                            nchar(SS_Land_Dist_Pct))]
  f2_nchar <- plot_df[Name == f2_name, pmax(nchar(Dist_SS_Diff_pDM) + .1 * as.numeric(plot_df[Name == f2_name & grepl('\\+', Dist_SS_Diff_pDM), .N] > 0), 
                                            nchar(SS_Dist_Accuracy), 
                                            nchar(SS_Land_Dist_Pct))]
  
  # generate 1st figure
  p51 <- ggplot() + 
    # boxes
    geom_rect(aes(xmin = 25 - 0.3, xmax = 25 + f1_nchar - 0.6, ymin = 40 - 2.3, ymax = 50 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 25 - 0.3, xmax = 25 + f1_nchar - 0.6, ymin = 20 - 2.5, ymax = 30 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 25 - 0.3, xmax = 25 + f1_nchar - 0.6, ymin = -1 - 1, ymax = 10 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 32 - 0.3, xmax = 32 + f2_nchar - 0.6, ymin = 40 - 2.3, ymax = 50 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 32 - 0.3, xmax = 32 + f2_nchar - 0.6, ymin = 20 - 2.5, ymax = 30 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 32 - 0.3, xmax = 32 + f2_nchar - 0.6, ymin = -1 - 1, ymax = 10 + .7), fill = 'white') + 
    # statistic labels
    annotate('text', x = 0, y = 45.5, label = 'Differential per Minute at Distance', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    annotate('text', x = 0, y = 25.5, label = 'Strike Accuracy at Distance', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    annotate('text', x = 0, y = 5.5, label = '% of Strikes Landed at Distance', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    # fighter 1 stats
    annotate('text', x = 25, y = 45.5, label = plot_df[Name == f1_name, Dist_SS_Diff_pDM], size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    annotate('text', x = 25, y = 25.5, label = plot_df[Name == f1_name, SS_Dist_Accuracy], size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    annotate('text', x = 25, y = 5.5, label = plot_df[Name == f1_name, SS_Land_Dist_Pct], size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    # fighter 2 stats
    annotate('text', x = 32, y = 45.5, label = plot_df[Name == f2_name, Dist_SS_Diff_pDM], size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    annotate('text', x = 32, y = 25.5, label = plot_df[Name == f2_name, SS_Dist_Accuracy], size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    annotate('text', x = 32, y = 5.5, label = plot_df[Name == f2_name, SS_Land_Dist_Pct], size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    # other geoms
    scale_x_continuous(limits = c(0, 50), labels = NULL) + 
    scale_y_continuous(limits = c(-2, 54), labels = NULL) + 
    labs(x = '', y = '', title = 'Distance (Significant) Striking', subtitle = 'Offensive Efficiency & Prevalence') + 
    dark_theme_gray() + 
    custom_theme + 
    theme(plot.margin = margin(0, 0.2, 0.0, 0.2, 'cm'), 
          axis.ticks = element_blank(), 
          axis.text = element_blank(), 
          axis.title = element_blank())
  
  # generate 2nd figure
  plot_df <- melt(plot_df[, .(Name, 'Absorbed' = Dist_SS_Abosrbed_pDM, 'Landed' = Dist_SS_Landed_pDM)], id.vars = 'Name')
  plot_df[, Name := factor(Name, levels = c(f2_name, f1_name))]
  
  p52 <- ggplot(plot_df, aes(x = variable, y = value, color = Name, fill = Name, lty = variable)) + 
    geom_linerange(aes(xmin = variable, xmax = variable, ymin = 0, ymax = value, lty = variable), 
                   position = position_dodge(width = .7), size = 1.5, key_glyph = 'path') + 
    labs(title = '', 
         subtitle = 'Distance Strikes per Minute at Distance', 
         x = '', 
         y = '') + 
    scale_y_continuous(limits = c(0, 1.2 * plot_df[, max(value)])) + 
    geom_text(aes(x = variable, y = value, label = format(round2(value, 1), nsmall = 1)), size = 3, fontface = 'bold', hjust = -.6, 
              position = position_dodge(width = .7), show.legend = F) + 
    scale_color_manual(breaks = c(f1_name, f2_name), 
                       values = c(f1_color, f2_color), 
                       guide = NULL) + 
    scale_fill_manual(breaks = c(f1_name, f2_name), 
                      values = c(f1_color, f2_color), 
                      guide = NULL) + 
    scale_linetype_manual(breaks = c('Landed', 'Absorbed'), 
                          values = c(1, 3), 
                          guide = NULL) + 
    coord_flip() + 
    dark_theme_gray() + 
    theme(legend.position = 'right', 
          legend.title = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank()) + 
    custom_theme + 
    theme(plot.margin = margin(0, 0.2, 0.0, 0.2, 'cm'), 
          axis.title = element_blank())
  
  # combine figures
  p5 <- ggarrange(p51, p52, nrow = 2, ncol = 1, heights = c(1, 1.2))
  
  #-------------------------------
  # FIGURE 6: takedown statistics
  #-------------------------------
  
  # create table for figure
  plot_df <- df[, .(Name, TD_Ratio = paste0(Takedowns, ' : ', Takedowns_Conceded), 
                    TD_Offense = paste0(format(round2(Takedowns_p5DM, 1), nsmall = 1), ' : ', format(round2(Takedowns_Att_p5DM, 1), nsmall = 1)), 
                    TD_Defense = paste0(format(round2(Takedowns_Conceded_p5DM, 1), nsmall = 1), ' : ', format(round2(Takedowns_Faced_p5DM, 1), nsmall = 1)), 
                    Takedown_Accuracy, Takedown_Defense)]
  
  # collect lengths of strings
  f1_nchar <- plot_df[Name == f1_name, pmax(nchar(TD_Ratio), nchar(TD_Offense), nchar(TD_Defense))]
  f2_nchar <- plot_df[Name == f2_name, pmax(nchar(TD_Ratio), nchar(TD_Offense), nchar(TD_Defense))]
  
  # generate 1st figure
  p61 <- ggplot() + 
    # boxes
    geom_rect(aes(xmin = 27 - 0.3, xmax = 27 + f1_nchar - 1.9 - 0.7, ymin = 40 - 2.3, ymax = 50 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 27 - 0.3, xmax = 27 + f1_nchar - 1.9 - 0.7, ymin = 20 - 2.5, ymax = 30 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 27 - 0.3, xmax = 27 + f1_nchar - 1.9 - 0.7, ymin = -1 - 1, ymax = 10 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 38 - 0.3, xmax = 38 + f2_nchar - 1.9 - 0.7, ymin = 40 - 2.3, ymax = 50 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 38 - 0.3, xmax = 38 + f2_nchar - 1.9 - 0.7, ymin = 20 - 2.5, ymax = 30 + .7), fill = 'white') + 
    geom_rect(aes(xmin = 38 - 0.3, xmax = 38 + f2_nchar - 1.9 - 0.7, ymin = -1 - 1, ymax = 10 + .7), fill = 'white') + 
    # statistic labels
    annotate('text', x = 0, y = 45.5, label = 'Total Takedowns Landed:Conceded', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    annotate('text', x = 0, y = 25.5, label = 'Takedown Rates Landed:Attempted', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    annotate('text', x = 0, y = 5.5, label = 'Takedown Rates Conceded:Faced', size = 3, hjust = 0, color = 'white', fontface = 'bold') + 
    # fighter 1 stats
    annotate('text', x = 27, y = 45.5, label = plot_df[Name == f1_name, TD_Ratio], size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    annotate('text', x = 27, y = 25.5, label = plot_df[Name == f1_name, TD_Offense], size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    annotate('text', x = 27, y = 5.5, label = plot_df[Name == f1_name, TD_Defense], size = 3.5, hjust = 0, color = f1_color, fontface = 'bold') + 
    # fighter 2 stats
    annotate('text', x = 38, y = 45.5, label = plot_df[Name == f2_name, TD_Ratio], size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    annotate('text', x = 38, y = 25.5, label = plot_df[Name == f2_name, TD_Offense], size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    annotate('text', x = 38, y = 5.5, label = plot_df[Name == f2_name, TD_Defense], size = 3.5, hjust = 0, color = f2_color, fontface = 'bold') + 
    # other geoms
    scale_x_continuous(limits = c(0, 50), labels = NULL) + 
    scale_y_continuous(limits = c(-2, 54), labels = NULL) + 
    labs(x = '', y = '', title = 'Takedown Statistics', subtitle = 'Volume & Rates per 5 Minutes at Distance') + 
    dark_theme_gray() + 
    custom_theme + 
    theme(plot.margin = margin(0, 0.2, 0.0, 0.2, 'cm'), 
          axis.ticks = element_blank(), 
          axis.text = element_blank(), 
          axis.title = element_blank())
  
  # generate 2nd figure
  plot_df <- melt(plot_df[, .(Name, 'Defense' = Takedown_Defense, 'Accuracy' = Takedown_Accuracy)], id.vars = 'Name')
  plot_df[, Name := factor(Name, levels = c(f2_name, f1_name))]
  
  p62 <- ggplot(plot_df, aes(x = variable, y = value, color = Name, fill = Name, lty = variable)) + 
    geom_linerange(aes(xmin = variable, xmax = variable, ymin = 0, ymax = value, lty = variable), 
                   position = position_dodge(width = .7), size = 1.5, key_glyph = 'path') + 
    labs(title = '', 
         subtitle = 'Offensive & Defensive Efficiency', 
         x = '', 
         y = '') + 
    scale_y_continuous(limits = c(0, 1.12)) + 
    geom_text(aes(x = variable, y = value, label = paste0(100 * round2(value, 2), '%')), size = 3, fontface = 'bold', hjust = -.6, 
              position = position_dodge(width = .7), show.legend = F) + 
    scale_color_manual(breaks = c(f1_name, f2_name), 
                       values = c(f1_color, f2_color), 
                       guide = NULL) + 
    scale_fill_manual(breaks = c(f1_name, f2_name), 
                      values = c(f1_color, f2_color), 
                      guide = NULL) + 
    scale_linetype_manual(breaks = c('Accuracy', 'Defense'), 
                          values = c(1, 3), 
                          guide = NULL) + 
    coord_flip() + 
    dark_theme_gray() + 
    theme(legend.position = 'right', 
          legend.title = element_blank(), 
          axis.ticks.x = element_blank(), 
          axis.text.x = element_blank()) + 
    custom_theme + 
    theme(plot.margin = margin(0, 0.2, 0.0, 0.2, 'cm'), 
          axis.title = element_blank())
  
  # combine figures
  p6 <- ggarrange(p61, p62, nrow = 2, ncol = 1, heights = c(1, 1.2))
  
  #---------------------------
  # FIGURE 7 & 8: recent form
  #---------------------------
  
  # set table parameters
  cell_text_size <- 9
  colnames_text_size <- cell_text_size + 1
  padding_mm <- 7
  
  # Fighter 1 table
  
  # identify rows corresponding to wins and losses
  f1_win_rows <- which(f1_recent_df[, grepl('Win', Outcome)])
  f1_loss_rows <- which(f1_recent_df[, grepl('Loss', Outcome)])
  
  # create table object
  f1_recent_form_tbl <- ggtexttable(f1_recent_df, 
                                    rows = NULL, 
                                    theme = ttheme(padding = unit(c(padding_mm + .3, padding_mm), 'mm'), 
                                                   colnames.style = colnames_style(size = colnames_text_size, 
                                                                                   color = fig_font_color, 
                                                                                   fill = fig_bg_color, 
                                                                                   linewidth = 4), 
                                                   tbody.style = tbody_style(size = cell_text_size))) %>%
    table_cell_bg(row = 2:f1_recent_df[, .N + 1], 
                  column = 1:ncol(f1_recent_df), 
                  fill = fig_bg_color, 
                  color = fig_font_color, 
                  linewidth = 2) %>% 
    table_cell_font(row = 2:f1_recent_df[, .N + 1], 
                    column = 1:ncol(f1_recent_df), 
                    color = fig_font_color, 
                    size = cell_text_size, 
                    face = 'bold')
  
  # change text color for losses (if applicable)
  if(length(f1_loss_rows) > 0){
    f1_recent_form_tbl <- f1_recent_form_tbl %>% 
      table_cell_font(row = f1_loss_rows + 1, 
                      column = 1:ncol(f1_recent_df), 
                      color = f1_loss_color, 
                      size = cell_text_size, 
                      face = 'bold')
  }
  
  # change text color for wins (if applicable)
  if(length(f1_win_rows) > 0){
    f1_recent_form_tbl <- f1_recent_form_tbl %>% 
      table_cell_font(row = f1_win_rows + 1, 
                      column = 1:ncol(f1_recent_df), 
                      color = f1_color, 
                      size = cell_text_size, 
                      face = 'bold')
  }
  
  # Fighter 2 table
  
  # identify rows corresponding to wins and losses
  f2_win_rows <- which(f2_recent_df[, grepl('Win', Outcome)])
  f2_loss_rows <- which(f2_recent_df[, grepl('Loss', Outcome)])
  
  # create table object
  f2_recent_form_tbl <- ggtexttable(f2_recent_df, 
                                    rows = NULL, 
                                    theme = ttheme(padding = unit(c(padding_mm + .3, padding_mm), 'mm'), 
                                                   colnames.style = colnames_style(size = colnames_text_size, 
                                                                                   color = fig_font_color, 
                                                                                   fill = fig_bg_color, 
                                                                                   linewidth = 4), 
                                                   tbody.style = tbody_style(size = cell_text_size))) %>%
    table_cell_bg(row = 2:f2_recent_df[, .N + 1], 
                  column = 1:ncol(f2_recent_df), 
                  fill = fig_bg_color, 
                  color = fig_font_color, 
                  linewidth = 2) %>% 
    table_cell_font(row = 2:f2_recent_df[, .N + 1], 
                    column = 1:ncol(f2_recent_df), 
                    color = fig_font_color, 
                    size = cell_text_size, 
                    face = 'bold')
  
  # change text color for losses (if applicable)
  if(length(f2_loss_rows) > 0){
    f2_recent_form_tbl <- f2_recent_form_tbl %>% 
      table_cell_font(row = f2_loss_rows + 1, 
                      column = 1:ncol(f2_recent_df), 
                      color = f2_loss_color, 
                      size = cell_text_size, 
                      face = 'bold')
  }
  
  # change text color for wins (if applicable)
  if(length(f2_win_rows) > 0){
    f2_recent_form_tbl <- f2_recent_form_tbl %>% 
      table_cell_font(row = f2_win_rows + 1, 
                      column = 1:ncol(f2_recent_df), 
                      color = f2_color, 
                      size = cell_text_size, 
                      face = 'bold')
  }
  
  # add ggtexttable to ggplot graph
  # https://stackoverflow.com/questions/51331122/r-inserting-ggtexttable-inside-a-ggplot-graph
  
  p7 <- ggplot() + 
    labs(title = paste0(f1_name, ' - Last ', n_recent, ' UFC Fights')) + 
    dark_theme_gray() + 
    scale_x_continuous(limits = c(0, 10), labels = NULL)  + 
    scale_y_continuous(limits = c(0, 10), labels = NULL) + 
    annotation_custom(ggplotGrob(f1_recent_form_tbl)) + 
    custom_theme + 
    theme(axis.ticks = element_blank())
  
  p8 <- ggplot() + 
    labs(title = paste0(f2_name, ' - Last ', n_recent, ' UFC Fights')) + 
    dark_theme_gray() + 
    scale_x_continuous(limits = c(0, 10), labels = NULL)  + 
    scale_y_continuous(limits = c(0, 10), labels = NULL) + 
    annotation_custom(ggplotGrob(f2_recent_form_tbl)) + 
    custom_theme + 
    theme(axis.ticks = element_blank())
  
  #-----------------
  # COMBINE FIGURES
  #-----------------
  
  # other combining options - see patchwork
  #https://stackoverflow.com/questions/7993722/creating-arbitrary-panes-in-ggplot2/51220506#51220506
  
  # combine figures
  combined_fig <- ggarrange(p1, p2, p3, p4, p5, p6, p7, p8,
                            nrow = 4,
                            ncol = 2) +
    bgcolor(fig_bg_color)
  
  # create title with color
  custom_title <- 'UFC Matchup Comparison: '
  
  fig_title <- paste0(custom_title, 
                      f1_name, ' ', 
                      f1_record, 
                      ' vs ', 
                      f2_name, ' ', 
                      f2_record)
  
  x <- 0.5
  y <- -1.1
  title_font_size <- 16
  
  t1 <- textGrob(bquote(bold(.(custom_title)) * 
                          phantom(bold(.(f1_name))) * 
                          bold(' ') * 
                          phantom(bold(.(f1_record))) * 
                          bold(' vs ') * 
                          phantom(bold(.(f2_name))) * 
                          bold(' ') * 
                          phantom(bold(.(f2_record)))),
                 gp = gpar(col = 'black', fontsize = title_font_size), 
                 x = x, y = y)
  t2 <- textGrob(bquote(phantom(bold(.(custom_title))) * 
                          bold(.(f1_name)) * 
                          bold(' ') * 
                          bold(.(f1_record)) * 
                          phantom(bold(' vs ')) * 
                          phantom(bold(.(f2_name))) * 
                          bold(' ') * 
                          phantom(bold(.(f2_record)))),
                 gp = gpar(col = f1_color, fontsize = title_font_size), 
                 x = x, y = y)
  t3 <- textGrob(bquote(phantom(bold(.(custom_title))) * 
                          phantom(bold(.(f1_name))) * 
                          bold(' ') * 
                          phantom(bold(.(f1_record))) * 
                          phantom(bold(' vs ')) * 
                          bold(.(f2_name)) * 
                          bold(' ') * 
                          bold(.(f2_record))),
                 gp = gpar(col = f2_color, fontsize = title_font_size), 
                 x = x, y = y)
  
  # annotate combined figure with top and bottom titles
  combined_fig <- annotate_figure(combined_fig, 
                                  top = text_grob('', size = title_font_size, color = 'black', face = 'bold'), 
                                  bottom = text_grob(paste0('Created by: @NateLatshaw || Data source as of ', as_of_date, 
                                                            ': ufcstats.com || All records & stats reflect each fighter\'s UFC career only.'), 
                                                     size = 10, color = 'black', face = 'bold'))
  
  combined_fig <- annotate_figure(combined_fig, 
                                  top = grobTree(t1, t2, t3))
  
  return(combined_fig)
}

#---------------------------------------------------------------------------------------------
# ROUND FUNCTION
#---------------------------------------------------------------------------------------------

round2 = function(x, n) {
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^n
  z*posneg
}




