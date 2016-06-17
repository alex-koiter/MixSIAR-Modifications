plot_data <- function (filename, plot_save_pdf, plot_save_png, mix, source, 
          discr) 
{
  if (mix$n.iso == 1) {
    p <- plot_data_one_iso(mix, source, discr, filename, plot_save_pdf, 
                      plot_save_png)
  }
  else {
    for (iso1 in 1:(mix$n.iso - 1)) {
      for (iso2 in (iso1 + 1):mix$n.iso) {
        p <- plot_data_two_iso(c(iso1, iso2), mix, source, 
                          discr, filename, plot_save_pdf, plot_save_png)
      }
    }
  }
  return(p)
}

plot_data_one_iso <- function (mix, source, discr, filename, plot_save_pdf, plot_save_png) {
  
  ## plot list

  
  if (!exists("x_label"))
    x_label <- mix$iso_names
  y_data <- 0.5
  y <- rep(y_data, mix$N)
  df <- data.frame(x = mix$data_iso, y = y)
  spacing <- 0.1
  if (!is.na(source$by_factor)) {
    source_linetype <-
      sort(rep(1:source$n.sources, source$S_factor_levels))
    source_color <- factor(as.numeric(source$S_factor1))
    index <- seq(
      from = 1, to = 1 + (source$n.sources - 1) *
        source$S_factor_levels, by = source$S_factor_levels
    )
    discr_mu_plot <- array(NA, dim = c(length(source$S_MU[,
                                                          1]), mix$n.iso))
    discr_sig2_plot <- array(NA, dim = c(length(source$S_MU[,
                                                            1]), mix$n.iso))
    for (i in 1:source$n.sources) {
      discr_mu_plot[index[i]:(index[i] + source$S_factor_levels -
                                1),] <-
        matrix(
          rep(discr$mu[i], source$S_factor_levels),
          nrow = source$S_factor_levels, ncol = mix$n.iso,
          byrow = T
        )
      discr_sig2_plot[index[i]:(index[i] + source$S_factor_levels -
                                  1),] <-
        matrix(
          rep(discr$sig2[i], source$S_factor_levels),
          nrow = source$S_factor_levels, ncol = mix$n.iso,
          byrow = T
        )
    }
    y_sources <- seq(
      y_data + 0.2, (source$S_factor_levels *
                       source$n.sources * spacing) - spacing + y_data +
        0.2, by = spacing
    )
    MU_plot <- source$S_MU[, mix$iso_names] + discr_mu_plot
    SIG_plot <-
      sqrt(source$S_SIG[, mix$iso_names] ^ 2 + discr_sig2_plot)
  }
  else {
    source_linetype <- 1:source$n.sources
    source_color <- factor(rep("black", source$n.sources))
    index <- 1:source$n.sources
    discr_mu_plot <- discr$mu
    discr_sig2_plot <- discr$sig2
    y_sources <- seq(y_data + 0.2, (source$n.sources * spacing) -
                       spacing + y_data + 0.2, by = spacing)
    source$S_factor_levels <- 0.5
    MU_plot <- source$S_MU + discr_mu_plot
    SIG_plot <- sqrt(source$S_SIG ^ 2 + discr_sig2_plot)
  }
  MU_plot <- as.vector(MU_plot)
  SIG_plot <- as.vector(SIG_plot)
  df_sources <- data.frame(
    x = MU_plot, xmin = MU_plot - SIG_plot,
    xmax = MU_plot + SIG_plot, y = y_sources, linetype = source_linetype,
    scolour = source_color
  )
  source.labels <-
    data.frame(
      x = MU_plot[index], y = y_sources[index] +
        spacing * source$S_factor_levels, label = source$source_names
    )

  if (mix$n.effects == 2) {
    shapes <- c(16, 17, 15, 3, 7, 8, 1, 6, 35, 36, 37, 4,
                18, 14, 11, 9, 13)
    shapes <- shapes[1:mix$FAC[[2]]$levels]
    if (!is.na(source$by_factor)) {
      g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x,
                                                   y = y)) + ggplot2::geom_point(
                                                     ggplot2::aes(
                                                       colour = factor(mix$FAC[[1]]$values),
                                                       shape = factor(mix$FAC[[2]]$values)
                                                     ), position = position_jitter(width = 0.2,
                                                                                   height = 0.1), show.legend = T
                                                   ) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)),
                                                                                      labels = mix$FAC[[1]]$labels) + ggplot2::scale_shape_manual(values = shapes,
                                                                                                                                                  labels = mix$FAC[[2]]$labels) + ggplot2::geom_point(
                                                                                                                                                    data = df_sources,
                                                                                                                                                    ggplot2::aes(
                                                                                                                                                      x = x, y = y, colour = scolour
                                                                                                                                                    ),
                                                                                                                                                    size = 2, show.legend = F
                                                                                                                                                  ) + ggplot2::geom_errorbarh(
                                                                                                                                                    data = df_sources,
                                                                                                                                                    ggplot2::aes(
                                                                                                                                                      xmin = xmin, xmax = xmax, colour = scolour
                                                                                                                                                    ),
                                                                                                                                                    size = 1, height = 0, linetype = source_linetype,
                                                                                                                                                    show.legend = F
                                                                                                                                                  ) + ggplot2::geom_text(data = source.labels,
                                                                                                                                                                         ggplot2::aes(x = x, y = y, label = label), show.legend = F) +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::ylab("") + ggplot2::xlab(x_label) +
        ggplot2::theme_bw() + ggplot2::theme(
          legend.position = c(0,
                              1), legend.justification = c(0, 1), legend.title = ggplot2::element_blank()
        )
      print(g)
      
    }
    else {
      g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x,
                                                   y = y)) + ggplot2::geom_point(
                                                     ggplot2::aes(
                                                       colour = factor(mix$FAC[[1]]$values),
                                                       shape = factor(mix$FAC[[2]]$values)
                                                     ), position = position_jitter(width = 0.2,
                                                                                   height = 0.1), show.legend = T
                                                   ) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)),
                                                                                      labels = mix$FAC[[1]]$labels) + ggplot2::scale_shape_manual(values = shapes,
                                                                                                                                                  labels = mix$FAC[[2]]$labels) + ggplot2::geom_point(
                                                                                                                                                    data = df_sources,
                                                                                                                                                    ggplot2::aes(x = x, y = y), size = 2, show.legend = F
                                                                                                                                                  ) +
        ggplot2::geom_errorbarh(
          data = df_sources, ggplot2::aes(xmin = xmin,
                                          xmax = xmax), size = 1, height = 0, linetype = source_linetype,
          show.legend = F
        ) + ggplot2::geom_text(data = source.labels,
                               ggplot2::aes(x = x, y = y, label = label), show.legend = F) +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::ylab("") + ggplot2::xlab(x_label) +
        ggplot2::theme_bw() + ggplot2::theme(
          legend.position = c(0,
                              1), legend.justification = c(0, 1), legend.title = ggplot2::element_blank()
        )
      print(g)
      
    }
  }
  if (mix$n.effects == 1) {
    if (!is.na(source$by_factor)) {
      g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x,
                                                   y = y)) + ggplot2::geom_point(
                                                     ggplot2::aes(colour = factor(mix$FAC[[1]]$values)),
                                                     position = position_jitter(width = 0.2, height = 0.1),
                                                     show.legend = T
                                                   ) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)),
                                                                                      labels = mix$FAC[[1]]$labels) + ggplot2::geom_point(
                                                                                        data = df_sources,
                                                                                        ggplot2::aes(
                                                                                          x = x, y = y, colour = scolour
                                                                                        ),
                                                                                        size = 2, show.legend = F
                                                                                      ) + ggplot2::geom_errorbarh(
                                                                                        data = df_sources,
                                                                                        ggplot2::aes(
                                                                                          xmin = xmin, xmax = xmax, colour = scolour
                                                                                        ),
                                                                                        size = 1, height = 0, linetype = source_linetype,
                                                                                        show.legend = F
                                                                                      ) + ggplot2::geom_text(data = source.labels,
                                                                                                             ggplot2::aes(x = x, y = y, label = label), show.legend = F) +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::ylab("") + ggplot2::xlab(x_label) +
        ggplot2::theme_bw() + ggplot2::theme(
          legend.position = c(0,
                              1), legend.justification = c(0, 1), legend.title = ggplot2::element_blank()
        )
      print(g)
      
    }
    else {
      g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x,
                                                   y = y)) + ggplot2::geom_point(
                                                     ggplot2::aes(colour = factor(mix$FAC[[1]]$values)),
                                                     position = position_jitter(width = 0.2, height = 0.1),
                                                     show.legend = T
                                                   ) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)),
                                                                                      labels = mix$FAC[[1]]$labels) + ggplot2::geom_point(
                                                                                        data = df_sources,
                                                                                        ggplot2::aes(x = x, y = y), size = 2, show.legend = F
                                                                                      ) +
        ggplot2::geom_errorbarh(
          data = df_sources, ggplot2::aes(xmin = xmin,
                                          xmax = xmax), size = 1, height = 0, linetype = source_linetype,
          show.legend = F
        ) + ggplot2::geom_text(data = source.labels,
                               ggplot2::aes(x = x, y = y, label = label), show.legend = F) +
        ggplot2::scale_y_continuous(breaks = NULL) +
        ggplot2::ylab("") + ggplot2::xlab(x_label) +
        ggplot2::theme_bw() + ggplot2::theme(
          legend.position = c(0,
                              1), legend.justification = c(0, 1), legend.title = ggplot2::element_blank()
        )
      print(g)
      
    }
  }
  if (mix$n.effects == 0) {
    g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_point(position = ggplot2::position_jitter(width = 0.2,
                                                              height = 0.1)) + ggplot2::geom_point(
                                                                data = df_sources,
                                                                ggplot2::aes(x = x, y = y), size = 2, show.legend = F
                                                              ) +
      ggplot2::geom_errorbarh(
        data = df_sources, ggplot2::aes(xmin = xmin,
                                        xmax = xmax), size = 1, height = 0, linetype = source_linetype,
        show.legend = F
      ) + ggplot2::geom_text(data = source.labels,
                             ggplot2::aes(x = x, y = y, label = label), show.legend = F) +
      ggplot2::scale_y_continuous(breaks = NULL) + ggplot2::ylab("") +
      ggplot2::xlab(x_label) + ggplot2::theme_bw() + ggplot2::theme(
        legend.position = c(0,
                            1), legend.justification = c(0, 1), legend.title = ggplot2::element_blank()
      )
    print(g)
    
  }
  if (plot_save_pdf == TRUE) {
    mypath <- file.path(paste(getwd(), "/", filename, ".pdf",
                              sep = ""))
    cairo_pdf(filename = mypath, width = 7, height = 7)
    print(g)
    dev.off()
  }
  if (plot_save_png == TRUE) {
    mypath <- file.path(paste(getwd(), "/", filename, ".png",
                              sep = ""))
    png(filename = mypath)
    print(g)
    dev.off()
  }
  return(g.all)
}

plot_data_two_iso <-
  function (isotopes, mix, source, discr, filename, plot_save_pdf,
            plot_save_png) {
    df <-
      data.frame(x = mix$data_iso[, isotopes[1]], y = mix$data_iso[,
                                                                   isotopes[2]])
    ## grab plots in list
  
    
    if (!exists("x_label"))
      x_label <- mix$iso_names[isotopes[1]]
    if (!exists("y_label"))
      y_label <- mix$iso_names[isotopes[2]]
    if (!is.na(source$by_factor)) {
      source_linetype <-
        sort(rep(1:source$n.sources, source$S_factor_levels))
      source_color <- factor(as.numeric(source$S_factor1))
      index <- seq(
        from = 1, to = 1 + (source$n.sources - 1) *
          source$S_factor_levels, by = source$S_factor_levels
      )
      discr_mu_plot <- array(NA, dim = c(length(source$S_MU[,
                                                            1]), mix$n.iso))
      discr_sig2_plot <- array(NA, dim = c(length(source$S_MU[,
                                                              1]), mix$n.iso))
      for (i in 1:source$n.sources) {
        discr_mu_plot[index[i]:(index[i] + source$S_factor_levels -
                                  1),] <-
          matrix(
            rep(discr$mu[i,], source$S_factor_levels),
            nrow = source$S_factor_levels, ncol = mix$n.iso,
            byrow = T
          )
        discr_sig2_plot[index[i]:(index[i] + source$S_factor_levels -
                                    1),] <-
          matrix(
            rep(discr$sig2[i,], source$S_factor_levels),
            nrow = source$S_factor_levels, ncol = mix$n.iso,
            byrow = T
          )
      }
    }
    else {
      source_linetype <- 1:source$n.sources
      source_color <- factor(rep("black", source$n.sources))
      index <- 1:source$n.sources
      discr_mu_plot <- discr$mu
      discr_sig2_plot <- discr$sig2
    }
    MU_plot <- array(NA, dim = c(length(source$S_MU[, 1]), 2))
    SIG_plot <- array(NA, dim = c(length(source$S_SIG[, 1]),
                                  2))
    for (iso in 1:2) {
      MU_plot[, iso] <- source$S_MU[, isotopes[iso]] + discr_mu_plot[,
                                                                     isotopes[iso]]
      SIG_plot[, iso] <- sqrt(source$S_SIG[, isotopes[iso]] ^ 2 +
                                discr_sig2_plot[, isotopes[iso]])
    }
    df_sources <- data.frame(
      x = MU_plot[, 1], y = MU_plot[,
                                    2], ymin = MU_plot[, 2] - SIG_plot[, 2], ymax = MU_plot[,
                                                                                            2] + SIG_plot[, 2], xmin = MU_plot[, 1] - SIG_plot[,
                                                                                                                                               1], xmax = MU_plot[, 1] + SIG_plot[, 1], linetype = source_linetype,
      scolour = source_color
    )
    source.labels <- data.frame(
      x = MU_plot[index, 1] - rep(1,
                                  source$n.sources), y = MU_plot[index, 2] + rep(0.75,
                                                                                 source$n.sources), label = source$source_names
    )
    if (mix$n.effects == 2) {
      shapes <- c(16, 17, 15, 3, 7, 8, 1, 6, 35, 36, 37, 4,
                  18, 14, 11, 9, 13)
      shapes <- shapes[1:mix$FAC[[2]]$levels]
      if (!is.na(source$by_factor)) {
        g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x,
                                                     y = y)) + ggplot2::geom_point(
                                                       ggplot2::aes(
                                                         colour = factor(mix$FAC[[1]]$values),
                                                         shape = factor(mix$FAC[[2]]$values)
                                                       ), size = 2.5,
                                                       show.legend = T
                                                     ) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)),
                                                                                        labels = mix$FAC[[1]]$labels) + ggplot2::scale_shape_manual(values = shapes,
                                                                                                                                                    labels = mix$FAC[[2]]$labels) + ggplot2::geom_pointrange(
                                                                                                                                                      data = df_sources,
                                                                                                                                                      ggplot2::aes(
                                                                                                                                                        ymin = ymin, ymax = ymax, colour = scolour
                                                                                                                                                      ),
                                                                                                                                                      size = 1, linetype = source_linetype, show.legend = F
                                                                                                                                                    ) +
          ggplot2::geom_errorbarh(
            data = df_sources, ggplot2::aes(
              xmin = xmin,
              xmax = xmax, colour = scolour
            ), size = 1, height = 0,
            linetype = source_linetype, show.legend = F
          ) +
          ggplot2::geom_text(data = source.labels, ggplot2::aes(x = x,
                                                                y = y, label = label), show.legend = F) + ggplot2::ylab(y_label) +
          ggplot2::xlab(x_label) + ggplot2::theme_bw() +
          ggplot2::theme(
            legend.position = c(0, 1), legend.justification = c(0,
                                                                1), legend.title = ggplot2::element_blank()
          )
        print(g)
        
      }
      else {
        g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x,
                                                     y = y)) + ggplot2::geom_point(
                                                       ggplot2::aes(
                                                         colour = factor(mix$FAC[[1]]$values),
                                                         shape = factor(mix$FAC[[2]]$values)
                                                       ), size = 2.5,
                                                       show.legend = T
                                                     ) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)),
                                                                                        labels = mix$FAC[[1]]$labels) + ggplot2::scale_shape_manual(values = shapes,
                                                                                                                                                    labels = mix$FAC[[2]]$labels) + ggplot2::geom_pointrange(
                                                                                                                                                      data = df_sources,
                                                                                                                                                      ggplot2::aes(ymin = ymin, ymax = ymax), size = 1,
                                                                                                                                                      linetype = source_linetype, show.legend = F
                                                                                                                                                    ) +
          ggplot2::geom_errorbarh(
            data = df_sources, ggplot2::aes(xmin = xmin,
                                            xmax = xmax), size = 1, height = 0, linetype = source_linetype,
            show.legend = F
          ) + ggplot2::geom_text(data = source.labels,
                                 ggplot2::aes(x = x, y = y, label = label), show.legend = F) +
          ggplot2::ylab(y_label) + ggplot2::xlab(x_label) +
          ggplot2::theme_bw() + ggplot2::theme(
            legend.position = c(0,
                                1), legend.justification = c(0, 1), legend.title = ggplot2::element_blank()
          )
        print(g)
        
      }
    }
    if (mix$n.effects == 1) {
      if (!is.na(source$by_factor)) {
        g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x,
                                                     y = y)) + ggplot2::geom_point(ggplot2::aes(colour = factor(mix$FAC[[1]]$values)),
                                                                                                     show.legend = T) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)),
                                                                                                                                                       labels = mix$FAC[[1]]$labels) + ggplot2::geom_pointrange(
                                                                                                                                                         data = df_sources,
                                                                                                                                                         ggplot2::aes(
                                                                                                                                                           ymin = ymin, ymax = ymax, colour = scolour
                                                                                                                                                         ),
                                                                                                                                                         size = 1, linetype = source_linetype, show.legend = F
                                                                                                                                                       ) +
          ggplot2::geom_errorbarh(
            data = df_sources, ggplot2::aes(
              xmin = xmin,
              xmax = xmax, colour = scolour
            ), size = 1, height = 0,
            linetype = source_linetype, show.legend = F
          ) +
          ggplot2::geom_text(data = source.labels, ggplot2::aes(x = x,
                                                                y = y, label = label), show.legend = F) + ggplot2::ylab(y_label) +
          ggplot2::xlab(x_label) + ggplot2::theme_bw() +
          ggplot2::theme(
            legend.position = c(0, 1), legend.justification = c(0,
                                                                1), legend.title = ggplot2::element_blank()
          )
        print(g)
        
      }
      else {
        g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x,
                                                     y = y)) + ggplot2::geom_point(ggplot2::aes(colour = factor(mix$FAC[[1]]$values)),
                                                                                                     show.legend = T) + ggplot2::scale_colour_discrete(breaks = levels(factor(mix$FAC[[1]]$values)),
                                                                                                                                                       labels = mix$FAC[[1]]$labels) + ggplot2::geom_pointrange(
                                                                                                                                                         data = df_sources,
                                                                                                                                                         ggplot2::aes(ymin = ymin, ymax = ymax), size = 1,
                                                                                                                                                         linetype = source_linetype, show.legend = F
                                                                                                                                                       ) +
          ggplot2::geom_errorbarh(
            data = df_sources, ggplot2::aes(xmin = xmin,
                                            xmax = xmax), size = 1, height = 0, linetype = source_linetype,
            show.legend = F
          ) + ggplot2::geom_text(data = source.labels,
                                 ggplot2::aes(x = x, y = y, label = label), show.legend = F) +
          ggplot2::ylab(y_label) + ggplot2::xlab(x_label) +
          ggplot2::theme_bw() + ggplot2::theme(
            legend.position = c(0,
                                1), legend.justification = c(0, 1), legend.title = ggplot2::element_blank()
          )
        print(g)
        
      }
    }
    if (mix$n.effects == 0) {
      g <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_point() + ggplot2::geom_pointrange(
          data = df_sources,
          ggplot2::aes(ymin = ymin, ymax = ymax), size = 1,
          linetype = source_linetype, show.legend = F
        ) + ggplot2::geom_errorbarh(
          data = df_sources,
          ggplot2::aes(xmin = xmin, xmax = xmax), size = 1,
          height = 0, linetype = source_linetype, show.legend = F
        ) +
        ggplot2::geom_text(data = source.labels, ggplot2::aes(x = x,
                                                              y = y, label = label), show.legend = F) + ggplot2::ylab(y_label) +
        ggplot2::xlab(x_label) + ggplot2::theme_bw()
      print(g)
      
    }
    if (plot_save_pdf == TRUE) {
      mypath <- file.path(paste(
        getwd(), "/", filename, "_",
        isotopes[1], "_", isotopes[2], ".pdf", sep = ""
      ))
      cairo_pdf(filename = mypath, width = 7, height = 7)
      print(g)
      dev.off()
    }
    if (plot_save_png == TRUE) {
      mypath <- file.path(paste(
        getwd(), "/", filename, "_",
        isotopes[1], "_", isotopes[2], ".png", sep = ""
      ))
      png(filename = mypath)
      print(g)
      dev.off()
    }

  }



plot_prior <- function (alpha.prior = 1, source, plot_save_pdf = TRUE, plot_save_png = FALSE, 
          filename = "prior_plot") 
{
  if (length(which(alpha.prior == 0)) != 0) {
    stop(paste("*** Error: You cannot set any alpha = 0.\n      Instead, set = 0.01.***", 
               sep = ""))
  }
  n.sources <- source$n.sources
  if (is.numeric(alpha.prior) == F) 
    alpha.prior = 1
  if (length(alpha.prior) == 1) 
    alpha = rep(alpha.prior, n.sources)
  if (length(alpha.prior) > 1 & length(alpha.prior) != n.sources) 
    alpha = rep(1, n.sources)
  if (length(alpha.prior) > 1 & length(alpha.prior) == n.sources) 
    alpha = alpha.prior
  alpha.unif <- rep(1, n.sources)
  alpha.jeff <- rep(1/n.sources, n.sources)
  p = compositions::rDirichlet.rcomp(10000, alpha)
  p.unif = compositions::rDirichlet.rcomp(10000, alpha.unif)
  alpha_lab <- paste0("(", paste0(round(alpha, 2), collapse = ","), 
                      ")", sep = "")
  alpha.unif_lab <- paste0("(", paste0(round(alpha.unif, 2), 
                                       collapse = ","), ")", sep = "")
  #dev.new()
  layout(matrix(c(seq(1:(2 * n.sources)), (2 * n.sources) + 
                    1, (2 * n.sources) + 1), ncol = 2, byrow = TRUE), heights = c(rep(3, 
                                                                                      n.sources), 2))
  par(mai = rep(0.3, 4))
  for (i in 1:n.sources) {
    hist(p[, i], breaks = seq(0, 1, length.out = 40), col = "red", 
         main = paste0("Source ", i), xlab = expression(p[i]), 
         xlim = c(0, 1))
    hist(p.unif[, i], breaks = seq(0, 1, length.out = 40), 
         col = "darkgrey", main = paste0("Source ", i), xlab = expression(p[i]), 
         xlim = c(0, 1))
  }
  par(mai = c(0, 0, 0, 0))
  #plot.new()
  legend(x = "center", ncol = 2, legend = c(paste0("Your prior: ", 
                                                   alpha_lab), paste0("\"Uninformative\" prior", alpha.unif_lab)), 
         fill = c("red", "darkgrey"), bty = "n", cex = 1.5)
  if (plot_save_pdf == TRUE) {
    mypath <- file.path(paste(getwd(), "/", filename, ".pdf", 
                              sep = ""))
    cairo_pdf(filename = mypath, width = 7, height = 7)
    layout(matrix(c(seq(1:(2 * n.sources)), (2 * n.sources) + 
                      1, (2 * n.sources) + 1), ncol = 2, byrow = TRUE), 
           heights = c(rep(3, n.sources), 2))
    par(mai = rep(0.3, 4))
    for (i in 1:n.sources) {
      hist(p[, i], breaks = seq(0, 1, length.out = 40), 
           col = "red", main = paste0("Source ", i), xlab = expression(p[i]), 
           xlim = c(0, 1))
      hist(p.unif[, i], breaks = seq(0, 1, length.out = 40), 
           col = "darkgrey", main = paste0("Source ", i), 
           xlab = expression(p[i]), xlim = c(0, 1))
    }
    par(mai = c(0, 0, 0, 0))
    plot.new()
    legend(x = "center", ncol = 2, legend = c(paste0("Your prior: ", 
                                                     alpha_lab), paste0("\"Uninformative\" prior", alpha.unif_lab)), 
           fill = c("red", "darkgrey"), bty = "n", cex = 1.5)
    dev.off()
  }
  if (plot_save_png == TRUE) {
    mypath <- file.path(paste(getwd(), "/", filename, ".png", 
                              sep = ""))
    png(filename = mypath)
    layout(matrix(c(seq(1:(2 * n.sources)), (2 * n.sources) + 
                      1, (2 * n.sources) + 1), ncol = 2, byrow = TRUE), 
           heights = c(rep(3, n.sources), 2))
    par(mai = rep(0.3, 4))
    for (i in 1:n.sources) {
      hist(p[, i], breaks = seq(0, 1, length.out = 40), 
           col = "red", main = paste0("Source ", i), xlab = expression(p[i]), 
           xlim = c(0, 1))
      hist(p.unif[, i], breaks = seq(0, 1, length.out = 40), 
           col = "darkgrey", main = paste0("Source ", i), 
           xlab = expression(p[i]), xlim = c(0, 1))
    }
    par(mai = c(0, 0, 0, 0))
    plot.new()
    legend(x = "center", ncol = 2, legend = c(paste0("Your prior: ", 
                                                     alpha_lab), paste0("\"Uninformative\" prior", alpha.unif_lab)), 
           fill = c("red", "darkgrey"), bty = "n", cex = 1.5)
    dev.off()
  }
}





output_JAGS <- function (jags.1, mix, source, output_options = list(summary_save = TRUE, 
                                                     summary_name = "summary_statistics", sup_post = FALSE, plot_post_save_pdf = TRUE, 
                                                     plot_post_name = "posterior_density", sup_pairs = FALSE, 
                                                     plot_pairs_save_pdf = TRUE, plot_pairs_name = "pairs_plot", 
                                                     sup_xy = TRUE, plot_xy_save_pdf = FALSE, plot_xy_name = "xy_plot", 
                                                     gelman = TRUE, heidel = FALSE, geweke = TRUE, diag_save = TRUE, 
                                                     diag_name = "diagnostics", indiv_effect = FALSE, plot_post_save_png = FALSE, 
                                                     plot_pairs_save_png = FALSE, plot_xy_save_png = FALSE)) 
{
  mcmc.chains <- jags.1$BUGSoutput$n.chains
  N <- mix$N
  n.re <- mix$n.re
  n.effects <- mix$n.effects
  if (n.re == 1) {
    random_effects <- ifelse(mix$FAC[[1]]$re, mix$FAC[[1]]$name, 
                             mix$FAC[[2]]$name)
  }
  if (n.re == 2) {
    random_effects <- mix$factors
  }
  n.sources <- source$n.sources
  source_names <- source$source_names
  R2jags::attach.jags(jags.1)
  jags1.mcmc <- coda::as.mcmc(jags.1)
  n.draws <- length(p.global[, 1])
  if (mix$fere) {
    fac2_lookup <- list()
    for (f1 in 1:mix$FAC[[1]]$levels) {
      fac2_lookup[[f1]] <- unique(mix$FAC[[2]]$values[which(mix$FAC[[1]]$values == 
                                                              f1)])
    }
    ilr.both <- array(NA, dim = c(n.draws, mix$FAC[[1]]$levels, 
                                  mix$FAC[[2]]$levels, n.sources - 1))
    p.both <- array(NA, dim = c(n.draws, mix$FAC[[1]]$levels, 
                                mix$FAC[[2]]$levels, n.sources))
    cross.both <- array(data = NA, dim = c(n.draws, mix$FAC[[1]]$levels, 
                                           mix$FAC[[2]]$levels, n.sources, n.sources - 1))
    e <- matrix(rep(0, n.sources * (n.sources - 1)), nrow = n.sources, 
                ncol = (n.sources - 1))
    for (i in 1:(n.sources - 1)) {
      e[, i] <- exp(c(rep(sqrt(1/(i * (i + 1))), i), -sqrt(i/(i + 
                                                                1)), rep(0, n.sources - i - 1)))
      e[, i] <- e[, i]/sum(e[, i])
    }
    for (i in 1:n.draws) {
      for (f1 in 1:mix$FAC[[1]]$levels) {
        for (f2 in fac2_lookup[[f1]]) {
          for (src in 1:(n.sources - 1)) {
            ilr.both[i, f1, f2, src] <- ilr.global[i, 
                                                   src] + ilr.fac1[i, f1, src] + ilr.fac2[i, 
                                                                                          f2, src]
            cross.both[i, f1, f2, , src] <- (e[, src]^ilr.both[i, 
                                                               f1, f2, src])/sum(e[, src]^ilr.both[i, 
                                                                                                   f1, f2, src])
          }
          for (src in 1:n.sources) {
            p.both[i, f1, f2, src] <- prod(cross.both[i, 
                                                      f1, f2, src, ])
          }
          p.both[i, f1, f2, ] <- p.both[i, f1, f2, ]/sum(p.both[i, 
                                                                f1, f2, ])
        }
      }
    }
  }
  if (!output_options[[9]]) {
    #dev.new()
    print(lattice::xyplot(coda::as.mcmc(p.global), strip = lattice::strip.custom(factor.levels = source_names)))
    if (output_options[[10]]) {
      mypath <- file.path(paste(getwd(), "/", output_options[[11]], 
                                "_diet_p.pdf", sep = ""))
      dev.copy2pdf(file = mypath)
    }
    if (output_options[[20]]) {
      mypath <- file.path(paste(getwd(), "/", output_options[[11]], 
                                "_diet_p.png", sep = ""))
      dev.copy(png, mypath)
    }
    if (output_options[[17]]) {
      #dev.new()
      traceplot_labels <- rep("", length(random_effects) + 
                                1)
      if (n.re > 0) {
        for (i in 1:length(random_effects)) {
          traceplot_labels[i] <- paste(random_effects[i], 
                                       " SD", sep = "")
        }
      }
      traceplot_labels[length(random_effects) + 1] <- "Individual SD"
      if (n.re == 2) 
        print(lattice::xyplot(coda::as.mcmc(cbind(fac1.sig, 
                                                  fac2.sig, ind.sig)), strip = lattice::strip.custom(factor.levels = traceplot_labels)))
      if (n.re == 1) {
        if (mix$FAC[[1]]$re) {
          print(lattice::xyplot(coda::as.mcmc(cbind(fac1.sig, 
                                                    ind.sig)), strip = lattice::strip.custom(factor.levels = traceplot_labels)))
        }
        else {
          print(lattice::xyplot(coda::as.mcmc(cbind(fac2.sig, 
                                                    ind.sig)), strip = lattice::strip.custom(factor.levels = traceplot_labels)))
        }
      }
      if (n.re == 0) 
        print(lattice::xyplot(coda::as.mcmc(ind.sig), 
                              strip = lattice::strip.custom(factor.levels = traceplot_labels)))
    }
    else {
      if (n.re > 0) {
        #dev.new()
        traceplot_labels <- rep("", length(random_effects))
        for (i in 1:length(random_effects)) {
          traceplot_labels[i] <- paste(random_effects[i], 
                                       " SD", sep = "")
        }
        if (n.re == 2) 
          print(lattice::xyplot(coda::as.mcmc(cbind(fac1.sig, 
                                                    fac2.sig)), strip = lattice::strip.custom(factor.levels = traceplot_labels)))
        if (n.re == 1) {
          if (mix$FAC[[1]]$re) {
            print(lattice::xyplot(coda::as.mcmc(cbind(fac1.sig)), 
                                  strip = lattice::strip.custom(factor.levels = traceplot_labels)))
          }
          else {
            print(lattice::xyplot(coda::as.mcmc(cbind(fac2.sig)), 
                                  strip = lattice::strip.custom(factor.levels = traceplot_labels)))
          }
        }
      }
    }
    if (output_options[[10]]) {
      mypath <- file.path(paste(getwd(), "/", output_options[[11]], 
                                "_SD.pdf", sep = ""))
      dev.copy2pdf(file = mypath)
    }
    if (output_options[[20]]) {
      mypath <- file.path(paste(getwd(), "/", output_options[[11]], 
                                "_SD.png", sep = ""))
      dev.copy(png, mypath)
    }
  }
  if (!output_options[[6]]) {
    #dev.new()
    panel.hist <- function(x, ...) {
      usr <- par("usr")
      on.exit(par(usr), add = TRUE)
      par(usr = c(usr[1:2], 0, 1.5))
      h <- hist(x, plot = FALSE)
      breaks <- h$breaks
      nB <- length(breaks)
      y <- h$counts
      y <- y/max(y)
      rect(breaks[-nB], 0, breaks[-1], y, col = "blue", 
           xlim = c(0, 1), ...)
    }
    panel.cor <- function(x, y, digits = 2, prefix = "", 
                          cex.cor) {
      usr <- par("usr")
      on.exit(par(usr), add = TRUE)
      par(usr = c(0, 1, 0, 1))
      r = (cor(x, y, use = "pairwise"))
      txt <- format(c(r, 0.123456789), digits = digits)[1]
      txt <- paste(prefix, txt, sep = "")
      if (missing(cex.cor)) 
        cex <- 0.8/strwidth(txt)
      text(0.5, 0.5, txt, cex = cex * abs(r))
    }
    panel.contour <- function(x, y) {
      n.lines <- 4
      my.cols <- rev(RColorBrewer::brewer.pal(n.lines, 
                                              "RdYlBu"))
      z <- MASS::kde2d(x, y)
      contour(z, drawlabels = FALSE, nlevels = n.lines, 
              col = my.cols, add = TRUE)
    }
    pairs(p.global, labels = source_names, diag.panel = panel.hist, 
          lower.panel = panel.cor, upper.panel = panel.contour)
    if (output_options[[7]]) {
      mypath <- file.path(paste(getwd(), "/", output_options[[8]], 
                                ".pdf", sep = ""))
      dev.copy2pdf(file = mypath)
    }
    if (output_options[[19]]) {
      mypath <- file.path(paste(getwd(), "/", output_options[[8]], 
                                ".png", sep = ""))
      dev.copy(png, mypath)
    }
  }
  if (!output_options[[3]]) {
    n.draws <- length(p.global[, 1])
    if (mix$n.fe == 0) {
      #dev.new()
      df <- data.frame(sources = rep(NA, n.draws * n.sources), 
                       x = rep(NA, n.draws * n.sources))
      for (i in 1:n.sources) {
        df$x[seq(1 + n.draws * (i - 1), i * n.draws)] <- as.matrix(p.global[, 
                                                                            i])
        df$sources[seq(1 + n.draws * (i - 1), i * n.draws)] <- rep(source_names[i], 
                                                                   n.draws)
      }
      my.title <- "Overall Population"
      print(ggplot2::ggplot(df, ggplot2::aes(x = x, fill = sources, 
                                             colour = sources)) + ggplot2::geom_density(alpha = 0.3, 
                                                                                        ggplot2::aes(y = ..scaled..)) + ggplot2::theme_bw() + 
              ggplot2::xlab("Proportion of Diet") + ggplot2::ylab("Scaled Posterior Density") + 
              ggplot2::xlim(0, 1) + ggplot2::labs(title = my.title) + 
              ggplot2::theme(legend.position = c(1, 1), legend.justification = c(1, 
                                                                                 1), legend.title = ggplot2::element_blank()))
      if (output_options[[4]]) {
        mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                  "_diet_p_global.pdf", sep = ""))
        dev.copy2pdf(file = mypath)
      }
      if (output_options[[18]]) {
        mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                  "_diet_p_global.png", sep = ""))
        dev.copy(png, mypath)
      }
    }
    if (n.effects >= 1 & mix$n.fe != 2) {
      for (f1 in 1:mix$FAC[[1]]$levels) {
        #dev.new()
        df <- data.frame(sources = rep(NA, n.draws * 
                                         n.sources), x = rep(NA, n.draws * n.sources))
        for (src in 1:n.sources) {
          df$x[seq(1 + n.draws * (src - 1), src * n.draws)] <- as.matrix(p.fac1[, 
                                                                                f1, src])
          df$sources[seq(1 + n.draws * (src - 1), src * 
                           n.draws)] <- rep(source_names[src], n.draws)
        }
        my.title <- mix$FAC[[1]]$labels[f1]
        print(ggplot2::ggplot(df, ggplot2::aes(x = x, 
                                               fill = sources, colour = sources)) + ggplot2::geom_density(alpha = 0.3, 
                                                                                                          ggplot2::aes(y = ..scaled..)) + ggplot2::xlim(0, 
                                                                                                                                                        1) + ggplot2::theme_bw() + ggplot2::xlab("Proportion of Diet") + 
                ggplot2::ylab("Scaled Posterior Density") + 
                ggplot2::labs(title = my.title) + ggplot2::theme(legend.position = c(1, 
                                                                                     1), legend.justification = c(1, 1), legend.title = ggplot2::element_blank()))
        if (output_options[[4]]) {
          mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                    "_diet_p_", mix$FAC[[1]]$labels[f1], ".pdf", 
                                    sep = ""))
          dev.copy2pdf(file = mypath)
        }
        if (output_options[[18]]) {
          mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                    "_diet_p_", mix$FAC[[1]]$labels[f1], ".png", 
                                    sep = ""))
          dev.copy(png, mypath)
        }
      }
      if (n.re == 2) {
        for (f2 in 1:mix$FAC[[2]]$levels) {
          #dev.new()
          df <- data.frame(sources = rep(NA, n.draws * 
                                           n.sources), x = rep(NA, n.draws * n.sources))
          for (src in 1:n.sources) {
            df$x[seq(1 + n.draws * (src - 1), src * n.draws)] <- as.matrix(p.fac2[, 
                                                                                  f2, src])
            df$sources[seq(1 + n.draws * (src - 1), src * 
                             n.draws)] <- rep(source_names[src], n.draws)
          }
          my.title <- mix$FAC[[2]]$labels[f2]
          print(ggplot2::ggplot(df, ggplot2::aes(x = x, 
                                                 fill = sources, colour = sources)) + ggplot2::geom_density(alpha = 0.3, 
                                                                                                            ggplot2::aes(y = ..scaled..)) + ggplot2::theme_bw() + 
                  ggplot2::xlim(0, 1) + ggplot2::xlab("Proportion of Diet") + 
                  ggplot2::ylab("Scaled Posterior Density") + 
                  ggplot2::labs(title = my.title) + ggplot2::theme(legend.position = c(1, 
                                                                                       1), legend.justification = c(1, 1), legend.title = ggplot2::element_blank()))
          if (output_options[[4]]) {
            mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                      "_diet_p_", mix$FAC[[2]]$labels[f2], ".pdf", 
                                      sep = ""))
            dev.copy2pdf(file = mypath)
          }
          if (output_options[[18]]) {
            mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                      "_diet_p_", mix$FAC[[2]]$labels[f2], ".png", 
                                      sep = ""))
            dev.copy(png, mypath)
          }
        }
      }
    }
    if (mix$fere) {
      for (f1 in 1:mix$FAC[[1]]$levels) {
        for (f2 in fac2_lookup[[f1]]) {
          #dev.new()
          df <- data.frame(sources = rep(NA, n.draws * 
                                           n.sources), x = rep(NA, n.draws * n.sources))
          for (src in 1:n.sources) {
            df$x[seq(1 + n.draws * (src - 1), src * n.draws)] <- as.matrix(p.both[, 
                                                                                  f1, f2, src])
            df$sources[seq(1 + n.draws * (src - 1), src * 
                             n.draws)] <- rep(source_names[src], n.draws)
          }
          my.title <- paste(mix$FAC[[1]]$labels[f1], 
                            mix$FAC[[2]]$labels[f2], sep = " ")
          print(ggplot2::ggplot(df, ggplot2::aes(x = x, 
                                                 fill = sources, colour = sources)) + ggplot2::geom_density(alpha = 0.3, 
                                                                                                            ggplot2::aes(y = ..scaled..)) + ggplot2::theme_bw() + 
                  ggplot2::xlim(0, 1) + ggplot2::xlab("Proportion of Diet") + 
                  ggplot2::ylab("Scaled Posterior Density") + 
                  ggplot2::labs(title = my.title) + ggplot2::theme(legend.position = c(1, 
                                                                                       1), legend.justification = c(1, 1), legend.title = ggplot2::element_blank()))
          if (output_options[[4]]) {
            mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                      "_diet_p_", mix$FAC[[2]]$labels[f2], ".pdf", 
                                      sep = ""))
            dev.copy2pdf(file = mypath)
          }
          if (output_options[[18]]) {
            mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                      "_diet_p_", mix$FAC[[2]]$labels[f2], ".png", 
                                      sep = ""))
            dev.copy(png, mypath)
          }
        }
      }
    }
    if (n.re > 0 || output_options[[17]]) {
      #dev.new()
      n.re_ind <- n.re + as.numeric(output_options[[17]])
      level <- c()
      x <- c()
      if (output_options[[17]]) {
        level <- c(level, rep("Individual SD", n.draws))
        x <- c(x, ind.sig)
      }
      if (n.re == 1) {
        if (mix$FAC[[1]]$re) {
          level <- c(level, rep(paste(mix$FAC[[1]]$name, 
                                      " SD", sep = ""), n.draws))
          x <- c(x, fac1.sig)
        }
        else {
          level <- c(level, rep(paste(mix$FAC[[2]]$name, 
                                      " SD", sep = ""), n.draws))
          x <- c(x, fac2.sig)
        }
      }
      if (n.re == 2) {
        level <- c(level, rep(paste(random_effects[1], 
                                    " SD", sep = ""), n.draws), rep(paste(random_effects[2], 
                                                                          " SD", sep = ""), n.draws))
        x <- c(x, fac1.sig, fac2.sig)
      }
      df2 <- data.frame(level = level, x = x)
      print(ggplot2::ggplot(df2, ggplot2::aes(x = x, fill = level, 
                                              colour = level)) + ggplot2::geom_density(alpha = 0.3) + 
              ggplot2::theme_bw() + ggplot2::xlab(expression(sigma)) + 
              ggplot2::ylab("Posterior Density") + ggplot2::theme(legend.position = c(1, 
                                                                                      1), legend.justification = c(1, 1), legend.title = ggplot2::element_blank()))
      if (output_options[[4]]) {
        mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                  "_SD.pdf", sep = ""))
        dev.copy2pdf(file = mypath)
      }
      if (output_options[[18]]) {
        mypath <- file.path(paste(getwd(), "/", output_options[[5]], 
                                  "_SD.png", sep = ""))
        dev.copy(png, mypath)
      }
    }
  }
  sig_labels <- NULL
  ind_labels <- NULL
  fac1_labels <- NULL
  fac2_labels <- NULL
  sig_stats <- NULL
  getQuant <- function(x) quantile(x, probs = c(0.025, 0.05, 
                                                0.25, 0.5, 0.75, 0.95, 0.975))
  getMeanSD <- function(x) cbind(round(apply(x, 2, mean), 3), 
                                 round(apply(x, 2, sd), 3))
  stats <- NULL
  sig_stats <- NULL
  sig_labels <- NULL
  if (mix$n.fe == 0) {
    global_quants <- t(round(apply(p.global, 2, getQuant), 
                             3))
    global_means <- getMeanSD(p.global)
    stats <- cbind(global_means, global_quants)
    global_labels <- rep(NA, n.sources)
    for (src in 1:n.sources) {
      global_labels[src] <- paste("p.global.", source_names[src], 
                                  sep = "")
    }
    rownames(stats) <- global_labels
  }
  if (n.effects > 0 & mix$n.fe != 2) {
    fac1_quants <- as.matrix(reshape::cast(reshape2::melt(round(apply(p.fac1, 
                                                                      c(2, 3), getQuant), 3)), Var3 + Var2 ~ Var1)[, -c(1, 
                                                                                                                        2)])
    fac1_quants <- t(apply(fac1_quants, 1, sort))
    fac1_means <- cbind(reshape2::melt(round(apply(p.fac1, 
                                                   c(2, 3), mean), 3))$value, reshape2::melt(round(apply(p.fac1, 
                                                                                                         c(2, 3), sd), 3))$value)
    fac1_stats <- cbind(fac1_means, fac1_quants)
    fac1_labels <- rep(NA, mix$FAC[[1]]$levels * n.sources)
    for (src in 1:n.sources) {
      for (f1 in 1:mix$FAC[[1]]$levels) {
        fac1_labels[mix$FAC[[1]]$levels * (src - 1) + 
                      f1] <- paste("p.", mix$FAC[[1]]$labels[f1], 
                                   ".", source_names[src], sep = "")
      }
    }
    rownames(fac1_stats) <- fac1_labels
    stats <- rbind(stats, fac1_stats)
    if (mix$FAC[[1]]$re) {
      sig_stats <- cbind(getMeanSD(fac1.sig), t(round(apply(fac1.sig, 
                                                            2, getQuant), 3)))
      sig_labels <- paste(mix$FAC[[1]]$name, ".SD", sep = "")
    }
  }
  if (n.re == 2) {
    fac2_quants <- as.matrix(reshape::cast(reshape2::melt(round(apply(p.fac2, 
                                                                      c(2, 3), getQuant), 3)), Var3 + Var2 ~ Var1)[, -c(1, 
                                                                                                                        2)])
    fac2_quants <- t(apply(fac2_quants, 1, sort))
    fac2_means <- cbind(reshape2::melt(round(apply(p.fac2, 
                                                   c(2, 3), mean), 3))$value, reshape2::melt(round(apply(p.fac2, 
                                                                                                         c(2, 3), sd), 3))$value)
    fac2_stats <- cbind(fac2_means, fac2_quants)
    fac2_labels <- rep(NA, mix$FAC[[2]]$levels * n.sources)
    for (src in 1:n.sources) {
      for (f2 in 1:mix$FAC[[2]]$levels) {
        fac2_labels[mix$FAC[[2]]$levels * (src - 1) + 
                      f2] <- paste("p.", mix$FAC[[2]]$labels[f2], 
                                   ".", source_names[src], sep = "")
      }
    }
    rownames(fac2_stats) <- fac2_labels
    stats <- rbind(stats, fac2_stats)
    if (mix$FAC[[2]]$re) {
      sig_stats <- rbind(sig_stats, cbind(getMeanSD(fac2.sig), 
                                          t(round(apply(fac2.sig, 2, getQuant), 3))))
      sig_labels <- c(sig_labels, paste(mix$FAC[[2]]$name, 
                                        ".SD", sep = ""))
    }
  }
  if (mix$fere) {
    fac2_quants <- matrix(NA, nrow = n.sources * length(unlist(fac2_lookup)), 
                          ncol = 7)
    fac2_means <- matrix(NA, nrow = n.sources * length(unlist(fac2_lookup)), 
                         ncol = 2)
    fac2_labels <- rep(NA, n.sources * length(unlist(fac2_lookup)))
    i <- 1
    for (f1 in 1:mix$FAC[[1]]$levels) {
      for (f2 in fac2_lookup[[f1]]) {
        for (src in 1:n.sources) {
          fac2_quants[i, ] <- getQuant(p.both[, f1, f2, 
                                              src])
          fac2_means[i, ] <- c(mean(p.both[, f1, f2, 
                                           src]), sd(p.both[, f1, f2, src]))
          fac2_labels[i] <- paste("p", mix$FAC[[1]]$labels[f1], 
                                  mix$FAC[[2]]$labels[f2], source_names[src], 
                                  sep = ".")
          i <- i + 1
        }
      }
    }
    fac2_stats <- round(cbind(fac2_means, fac2_quants), 3)
    rownames(fac2_stats) <- fac2_labels
    stats <- rbind(stats, fac2_stats)
    if (mix$FAC[[2]]$re) {
      sig_stats <- rbind(sig_stats, cbind(getMeanSD(fac2.sig), 
                                          t(round(apply(fac2.sig, 2, getQuant), 3))))
      sig_labels <- c(sig_labels, paste(mix$FAC[[2]]$name, 
                                        ".SD", sep = ""))
    }
  }
  if (output_options[[17]]) {
    ind_quants <- as.matrix(reshape::cast(reshape2::melt(round(apply(p.ind, 
                                                                     c(2, 3), getQuant), 3)), X3 + X2 ~ X1)[, -c(1, 2)])
    ind_quants <- t(apply(ind_quants, 1, sort))
    ind_means <- cbind(reshape2::melt(round(apply(p.ind, 
                                                  c(2, 3), mean), 3))$value, reshape2::melt(round(apply(p.ind, 
                                                                                                        c(2, 3), sd), 3))$value)
    ind_stats <- cbind(ind_means, ind_quants)
    ind_labels <- rep(NA, N * n.sources)
    for (src in 1:n.sources) {
      for (j in 1:N) {
        ind_labels[N * (src - 1) + j] <- paste("p.Ind ", 
                                               j, ".", source_names[src], sep = "")
      }
    }
    sig_stats <- rbind(sig_stats, cbind(getMeanSD(ind.sig), 
                                        t(round(apply(ind.sig, 2, getQuant), 3))))
    sig_labels <- c(sig_labels, "Individual.SD")
    rownames(ind_stats) <- ind_labels
    stats <- rbind(stats, ind_stats)
  }
  rownames(sig_stats) <- sig_labels
  stats <- rbind(sig_stats, stats)
  colnames(stats) <- c("Mean", "SD", "2.5%", "5%", "25%", "50%", 
                       "75%", "95%", "97.5%")
  n.var <- coda::nvar(jags1.mcmc)
  if (output_options[[12]]) {
    if (mcmc.chains == 1) {
      gelman <- "*** Error: Gelman diagnostic requires more than one chain ***"
    }
    if (mcmc.chains > 1) {
      gelman <- matrix(NA, nrow = n.var, ncol = 2)
      for (v in 1:coda::nvar(jags1.mcmc)) {
        gelman[v, ] <- coda::gelman.diag(jags1.mcmc[, 
                                                    v])$psrf
      }
      colnames(gelman) <- c("Point est.", "Upper C.I.")
      rownames(gelman) <- coda::varnames(jags1.mcmc)
      gelman.all <- gelman[which(!is.nan(gelman[, 1])), 
                           ]
      gelman_short <- gelman[order(gelman[, 1], decreasing = T), 
                             ]
      if (n.var > 10) 
        gelman_short <- gelman_short[1:10, ]
      gelman_fail <- c(length(which(gelman[, 1] > 1.01)), 
                       length(which(gelman[, 1] > 1.05)), length(which(gelman[, 
                                                                              1] > 1.1)))
    }
  }
  if (output_options[[13]]) {
    heidel <- coda::heidel.diag(jags1.mcmc)
    w <- which(!is.na(heidel[[1]][, "pvalue"]))
    heidel.all <- data.frame(matrix(NA, nrow = length(w), 
                                    ncol = 3 * mcmc.chains))
    colstring <- rep(NA, mcmc.chains * 3)
    for (i in 1:mcmc.chains) {
      heidel.tmp <- as.data.frame(heidel[[i]][w, c("stest", 
                                                   "pvalue", "htest")])
      heidel.all[, (3 * i - 2):(3 * i)] <- heidel.tmp
      colstring[(3 * i - 2):(3 * i)] <- c(paste("stest.", 
                                                i, sep = ""), paste("pval.", i, sep = ""), paste("hwtest.", 
                                                                                                 i, sep = ""))
    }
    rownames(heidel.all) <- coda::varnames(jags1.mcmc)
    colnames(heidel.all) <- colstring
    heidel.all <- round(heidel.all, 3)
    heidel.all <- replace(heidel.all, heidel.all == 0, "fail")
    heidel.all <- replace(heidel.all, heidel.all == 1, "pass")
    heidel.all <- replace(heidel.all, is.na(heidel.all), 
                          "fail")
    stest_fail <- rep(NA, mcmc.chains)
    hwtest_fail <- rep(NA, mcmc.chains)
    for (i in 1:mcmc.chains) {
      stest_fail[i] <- sum(heidel.all[, 3 * i - 2] == "fail")
      hwtest_fail[i] <- sum(heidel.all[, 3 * i] == "fail")
    }
    heidel_fail <- rbind(stest_fail, hwtest_fail)
    rownames(heidel_fail) <- c("Stationarity", "Half-width")
    colnames(heidel_fail) <- paste("Chain", 1:mcmc.chains)
  }
  if (output_options[[14]]) {
    geweke <- coda::geweke.diag(jags1.mcmc)
    geweke.all <- data.frame(matrix(NA, nrow = n.var, ncol = mcmc.chains))
    colstring <- rep(NA, mcmc.chains)
    for (i in 1:mcmc.chains) {
      geweke.tmp <- as.data.frame(geweke[[i]]$z)
      geweke.all[, i] <- geweke.tmp
      colstring[i] <- c(paste("chain", i, sep = ""))
    }
    rownames(geweke.all) <- coda::varnames(jags1.mcmc)
    colnames(geweke.all) <- colstring
    geweke.all <- round(geweke.all, 3)
    w <- which(!is.nan(geweke[[1]]$z))
    geweke.all <- geweke.all[w, ]
    geweke_fail <- matrix(NA, nrow = 1, ncol = mcmc.chains)
    for (i in 1:mcmc.chains) {
      geweke_fail[1, i] <- sum(abs(geweke.all[, i]) > 1.96)
    }
    colnames(geweke_fail) <- paste("Chain", 1:mcmc.chains)
    rownames(geweke_fail) <- "Geweke"
  }
  if (output_options[[12]]) {
    cat("\n################################################################################\n# Gelman-Rubin Diagnostic\n################################################################################\n\nGenerally the Gelman diagnostic should be < 1.05\n\n", 
        paste("Out of ", n.var, " variables: ", gelman_fail[1], 
              " > 1.01", sep = ""), "\n                      ", 
        paste(gelman_fail[2], " > 1.05", sep = ""), "\n                      ", 
        paste(gelman_fail[3], " > 1.1", sep = ""), "\n\nThe worst variables are:\n", 
        sep = "")
    print(gelman_short)
    if (output_options[[15]]) {
      mypath <- file.path(paste(getwd(), "/", output_options[[16]], 
                                ".txt", sep = ""))
      out <- capture.output(gelman)
      out2 <- capture.output(gelman_short)
      cat("\n################################################################################\n# Gelman-Rubin Diagnostic\n################################################################################\n\nGenerally the Gelman diagnostic should be < 1.05\n\n", 
          paste("Out of ", n.var, " variables: ", gelman_fail[1], 
                " > 1.01", sep = ""), "\n                      ", 
          paste(gelman_fail[2], " > 1.05", sep = ""), "\n                      ", 
          paste(gelman_fail[3], " > 1.1", sep = ""), "\n\nThe worst variables are:\n", 
          out2, "\n\nAnd here are the Gelman diagnostics for all variables:\n", 
          out, sep = "\n", file = mypath, append = FALSE)
    }
  }
  if (output_options[[13]]) {
    cat("\n################################################################################\n# Heidelberger and Welch Diagnostic\n################################################################################\n\nA few failures is normal and acceptable...\nNumber of failures in each chain (out of ", 
        n.var, " variables):\n\n", sep = "")
    print(heidel_fail)
    if (output_options[[15]]) {
      mypath <- file.path(paste(getwd(), "/", output_options[[16]], 
                                ".txt", sep = ""))
      out <- capture.output(heidel.all)
      out2 <- capture.output(heidel_fail)
      cat("\n################################################################################\n# Heidelberger and Welch Diagnostic\n################################################################################\n\nA few failures is normal and acceptable...\nNumber of failures in each chain (out of ", 
          n.var, " variables):\n\n", out2, "\n\nAnd here are the Heidelberger-Welch diagnostics for all variables:\n", 
          out, sep = "\n", file = mypath, append = output_options[[12]])
    }
  }
  if (output_options[[14]]) {
    cat("\n################################################################################\n# Geweke Diagnostic\n################################################################################\n\nThe Geweke diagnostic is a standard z-score, so we'd expect 5% to be outside +/-1.96\nNumber of variables outside +/-1.96 in each chain (out of ", 
        n.var, "):\n\n", sep = "")
    print(geweke_fail)
    if (output_options[[15]]) {
      mypath <- file.path(paste(getwd(), "/", output_options[[16]], 
                                ".txt", sep = ""))
      out <- capture.output(geweke.all)
      out2 <- capture.output(geweke_fail)
      cat("\n################################################################################\n# Geweke Diagnostic\n################################################################################\n\nThe Geweke diagnostic is a standard z-score, so we'd expect 5% to be outside +/-1.96\nNumber of variables outside +/-1.96 in each chain (out of ", 
          n.var, "):\n\n", out2, "\n\nAnd here are the Geweke diagnostics for all variables:\n", 
          out, sep = "\n", file = mypath, append = output_options[[12]] || 
            output_options[[13]])
    }
  }
  DIC <- jags.1$BUGSoutput$DIC
  cat("\n################################################################################\n# Summary Statistics\n################################################################################\n\nDIC = ", 
      DIC, sep = "")
  out1 <- capture.output(stats)
  cat("\n", out1, sep = "\n")
  if (output_options[[1]]) {
    mypath <- file.path(paste(getwd(), "/", output_options[[2]], 
                              ".txt", sep = ""))
    cat("\n#################################################################\n# Summary Statistics\n#################################################################\n\nDIC = ", 
        DIC, sep = "", file = mypath, append = FALSE)
    cat("\n", out1, sep = "\n", file = mypath, append = TRUE)
  }
  if (mix$n.ce > 0) {
    plot_continuous_var(jags.1, mix, source, output_options)
  }
  diag_filename <- paste(getwd(), "/", output_options[[16]], 
                         ".pdf", sep = "")
  ggmcmc::ggmcmc(ggmcmc::ggs(jags1.mcmc), file = diag_filename, 
                 plot = c("Rhat", "geweke", "density", "traceplot", "running", 
                          "autocorrelation", "crosscorrelation"))
}
 