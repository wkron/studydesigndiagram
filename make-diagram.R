# Plotting function

gen_long_diag <- function(plt, indices, txt_size = 14, addpad = 0) {
  # Color-blind friendly palette
  plt_colors <- c("#E69F00", "#56B4E9", "#009E73",
                  "#F0E442", "#0072B2", "#D55E00", 
                  "#CC79A7", "#999999")
  
  # Covert colors column to actual colors
  plt$color <- plt_colors[plt$color]
  
  # Generate x coordinates for intervals/boxes
  plt$minx <- plt$start
  plt$minx[plt$minx == -Inf] <- min(plt$minx[plt$minx != -Inf])
  
  plt$maxx <- plt$end
  plt$maxx[plt$maxx == Inf] <- max(plt$maxx[plt$maxx != Inf], 365)
  #plt$maxx[plt$maxx == -1] <- 0
  
  lwdth <- 0.8
  
  # Specify box size for intervals of length zero (cover time zero line)
  # Specify box size for intervals of length 0 (Cover the line for time zero)
  plt$minx[plt$end - plt$start == 0] <- plt$start[plt$end - plt$start == 0] - lwdth
  plt$maxx[plt$end - plt$start == 0] <- plt$start[plt$end - plt$start == 0] + lwdth
  plt$maxx[plt$end == 0] <- lwdth
  plt$minx[plt$start == 0] <- -lwdth
  
  # Generate x coordinates for labels
  plt$lblx <- plt$maxx - abs(plt$maxx - plt$minx) / 2
  
  # Prepare interval labels
  plt$start_lbl[plt$start_lbl == ""] <- plt$start[plt$start_lbl == ""]
  plt$end_lbl[plt$end_lbl == ""] <- plt$end[plt$end_lbl == ""]
  
  plt$lbl2[plt$lbl2 != ""] <- paste0("\n", plt$lbl2[plt$lbl2 != ""])
  plt$lbl <- paste0(plt$lbl, plt$lbl2, "\n", "Days [", plt$start_lbl, ";", plt$end_lbl, "]")
  
  indices$indexlabel2[indices$indexlabel2 != ""] <- paste0("\n", indices$indexlabel2[indices$indexlabel2 != ""])
  indices$indexlabel <- paste0(indices$indexlabel, indices$indexlabel2, "\nDay ", indices$vlines)
  
  plt_big <- plt[plt$outside_box == 0,]
  plt_sml <- plt[plt$outside_box > 0,]
  plt_bot <- plt[!(plt$start == 0 | plt$end == 0),]
  plt_top <- plt[plt$start == 0 | plt$end == 0 | plt$end - plt$start == 0,]
  plt_top$minx[plt_top$start == 0] <- -1
  
  abs_nudge <- floor((max(plt$maxx) - min(plt$minx)) / 100)
  
  max_neg <- max(abs(plt[plt$start < 0,]$end - plt[plt$start < 0,]$start))
  max_pos <- max(abs(plt[plt$start >= 0,]$end - plt[plt$start >= 0,]$start))
  
  plt_sml$minx <- ifelse(plt_sml$outside_box == 1, plt_sml$start - max_neg, plt_sml$end + abs_nudge)
  plt_sml$maxx <- ifelse(plt_sml$outside_box == 1, plt_sml$start - abs_nudge, plt_sml$end + max_pos)
  
  
  ggplot(plt, aes(x = lblx, 
                  y = line - 0.45, 
                  xmin = minx, 
                  xmax = maxx, 
                  ymin = line - 0.9, 
                  ymax = line,
                  label = lbl)) +
    # PLot Vertical lines / Indexdates
    geom_rect(data = indices, 
              mapping = aes(xmin = vlines - 0.8,
                            xmax = vlines + 0.8,
                            ymin = 0,
                            ymax = max(plt$line)),
              fill = "grey60",
              inherit.aes = FALSE) +
    # Plot boxes / periods
    geom_rect(alpha = 0.9,
              fill = plt$color) +
    # Text inside of boxes / intervals
     geom_fit_text(data = plt_big, 
              color = "white",
              size = txt_size,
              fontface = "bold") + #
    # geom_text(data = plt_big, 
    #           color = "white",
    #           size = txt_size,
    #           fontface = "bold") + 
    # Text outside of boxes - left
    geom_fit_text(data = plt_sml[plt_sml$outside_box == 1,],
                  aes(xmin = minx,
                      xmax = maxx,
                      y = line - 0.5,
                      label = lbl),
                  inherit.aes = FALSE,
                  padding.x = unit(3, "mm"),
                  place = "right",
                  color = plt_sml[plt_sml$outside_box == 1,]$color,
                  size = txt_size,
                  fontface = "bold",
                  outside = TRUE) +
    # Text outside of boxes - right
    geom_fit_text(data = plt_sml[plt_sml$outside_box == 2,],
                  aes(xmin = minx,
                      xmax = maxx,
                      y = line - 0.5,
                      label = lbl),
                  inherit.aes = FALSE,
                  padding.x = unit(3, "mm"),
                  place = "left",
                  color = plt_sml[plt_sml$outside_box == 2,]$color,
                  size = txt_size,
                  fontface = "bold",
                  outside = TRUE) +
    # geom_text(data = plt_sml,
    #           aes(x = ifelse(abs(end - start) == 0 | end <= 0, minx - abs_nudge, maxx + abs_nudge), 
    #               y = line - 0.45),
    #           hjust = ifelse(plt_sml$end - plt_sml$start == 0| plt_sml$end <= 0, "right", "left"),
    #           color = plt_sml$color,
    #           size = txt_size,
    #           fontface = "bold") +
    # Labeling of indexdates / vertical lines
    geom_label(data = indices,
               mapping = aes(x = vlines, 
                             y = max(plt$line) + 0.1, 
                             label = indexlabel),
               vjust = "bottom",
               hjust = ifelse(max(plt$end) <= 0, "right", "center"),
               fontface = "bold",
               size = (txt_size/(14/5)) + 0.25,
               color = "grey60",
               label.r = unit(0, "lines"),
               label.size = 0,
               label.padding = unit(1, "lines"),
               inherit.aes = FALSE) +
    xlab("Time (days)") +
    scale_x_continuous(expand = expand_scale(add = addpad)) + 
    scale_y_continuous(expand = c(0,0), 
                       limits = c(0, max(plt$line) + ifelse(any(grepl("\\n", indices$indexlabel)), 1, 0.5))) +
    theme_classic() +
    theme(legend.position = "none",
          axis.line.x = element_line(arrow = arrow(length = unit(0.6, "npc"), type = "closed")),
          axis.ticks.x = element_blank(),
          axis.text.x = element_text(face = "bold", size = txt_size),
          axis.title.x = element_text(face = "bold", size = txt_size),
          axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y =  element_blank(),
          axis.title.y = element_blank())
}