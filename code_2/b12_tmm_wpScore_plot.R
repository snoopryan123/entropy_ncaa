
source("b12_tmm_main.R")

###############################################################
### Plot WP with Hamming Score for Grid 1 (constant q) ###
###############################################################

plot_df_tmmWpHam1 = read_csv(paste0(output_folder,"plot_grid_wpScore_v1.csv"))
plot_df_tmmWpHam1 = plot_df_tmmWpHam1 %>%
  group_by(p,q,r,n,k,scoring_method) %>%
  mutate(wp = mean(wp)) %>%
  ungroup() %>%
  filter(fold == 1) 

my_palette_nk1 = c(
  rev(brewer.pal(name="PuRd",n=9)[4:9]),
  brewer.pal(name="Blues",n=9)[4:9]
)


for (p_ in unique(plot_df_tmmWpHam1$p)) {
  for (k_ in unique(plot_df_tmmWpHam1$k)) {
    plot_wp1_kp =
      plot_df_tmmWpHam1 %>%
      filter(p == p_ & k == k_) %>%
      mutate(n_ = paste0("n = ", n)) %>%
      ggplot(aes(y=q, x=r)) +
      facet_wrap(~n_) +
      theme(panel.spacing = unit(2, "lines")) +
      labs(title=paste0("k = ", k_, ", p = ", p_)) +
      geom_tile(aes(fill=wp)) +
      geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
      geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
      geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
      # scale_fill_gradientn(name="win\nprobability", colours = terrain.colors(7))
      scale_fill_gradientn(name="win\nprobability", colours = rev(terrain.colors(7)))
    plot_wp1_kp
    ggsave(
      paste0(output_folder, "plot_wpTmmHammingScore_v1_k",format(k_, scientific=T),"p",p_,".png"), 
      plot_wp1_kp,
      # width=12,height=10
      width=10,height=8
    )
  }
}
  
###########################################################
### Plot WP with Hamming Score for Grid 2 (qE, qL) ###
###########################################################

plot_df_tmmWpHam2 = read_csv(paste0(output_folder,"plot_grid_wpScore_v2.csv"))
plot_df_tmmWpHam2 = plot_df_tmmWpHam2 %>%
  group_by(p,qE,qL,rE,rL,q_cutoff,r_cutoff,n,k,scoring_method) %>%
  mutate(wp = mean(wp)) %>%
  ungroup() %>%
  filter(fold == 1) 

for (p_ in unique(plot_df_tmmWpHam2$p)) {
  for (k_ in unique(plot_df_tmmWpHam2$k)) {
    for (clipped_ in c(TRUE,FALSE)) {
      # p_ = 0.75
      # k_ = 100
      # clipped_ = TRUE
      n_ = k_
      
      df_plot_wp_nkp2 =
        plot_df_tmmWpHam2 %>%
        filter(q_cutoff == 3.5 & r_cutoff == 3.5) %>%
        filter(p == p_ & k == k_ & n == n_) %>%
        mutate(clipped = clipped_) %>%
        filter(ifelse(clipped, 
                      0.6 <= rL & rL <= 0.8 & 0.6 <= qL & qL <= 0.8,
                      TRUE)) %>%
        mutate(
          facet_ = paste0("rL = ", rL, ", qL = ", qL),
          facet_ = factor(facet_),
          facet_ = fct_reorder(facet_, -qL)
        ) %>%
        mutate(
          n_ = paste0("n=", n, ",  "),
          qE_str = paste0(
            paste0("q",(1:6)[1:6 < q_cutoff[1]], collapse="="), "=qE,  "
          ),
          qL_str = paste0(
            paste0("q",(1:6)[1:6 > q_cutoff[1]], collapse="="), "=qL, "
          ),
          rE_str = paste0(
            paste0("r",(1:6)[1:6 < r_cutoff[1]], collapse="="), "=rE,  "
          ),
          rL_str = paste0(
            paste0("r",(1:6)[1:6 > r_cutoff[1]], collapse="="), "=rL"
          ),
          qr_ = paste0(qE_str, qL_str, rE_str, rL_str),
          # title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, "\n", qr_)
        ) 
      
      title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, ", ", df_plot_wp_nkp2$scoring_method[1], " score",
                      "\n", df_plot_wp_nkp2$qr_[1]) 
      
      plot_wp_nkp2 = df_plot_wp_nkp2 %>%
        ggplot(aes(x=rE, y=qE)) +
        facet_wrap(~facet_) +
        theme(panel.spacing = unit(2, "lines")) +
        labs(title = title_) +
        geom_tile(aes(fill=wp)) +
        geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
        theme(
          plot.title = element_text(size = if (clipped_) 37 else 70),
          strip.text.x = element_text(size = if (clipped_) 26 else 30),
          axis.text.x = element_text(size = if (clipped_) 35 else 40),
          axis.text.y = element_text(size = if (clipped_) 35 else 40),
          axis.title.x = element_text(size = if (clipped_) 40 else 80),
          axis.title.y = element_text(size = if (clipped_) 40 else 80),
          legend.title = element_text(size = if (clipped_) 40 else 70), 
          legend.text  = element_text(size = if (clipped_) 35 else 50),
          legend.key.size = unit(if (clipped_) 2 else 3, "lines")
        ) +
        scale_fill_gradientn(name="win\nprob.", colours = rev(terrain.colors(7)))
      # plot_wp_nkp2
      ggsave(
        paste0(output_folder, 
               "plot_wpTmmHammingScore_v2_k",format(k_, scientific=T),"_n",
               format(n_, scientific=T),"_p",p_, if (clipped_) "_clipped",".png"), 
        plot_wp_nkp2,
        width = if (!clipped_) 28 else 14,
        height = if (!clipped_) 26 else 13
      )
    }
  }
}

###########################################################
### Plot WP with ESPN Score for Grid 3 (qE, qL) ###
###########################################################

plot_df_tmmWpESPN3 = read_csv(paste0(output_folder,"plot_grid_wpScore_v3.csv"))
plot_df_tmmWpESPN3 = plot_df_tmmWpESPN3 %>%
  group_by(p,qE,qL,rE,rL,q_cutoff,r_cutoff,n,k,scoring_method) %>%
  mutate(wp = mean(wp)) %>%
  ungroup() %>%
  filter(fold == 1) 

for (p_ in unique(plot_df_tmmWpESPN3$p)) {
  for (k_ in unique(plot_df_tmmWpESPN3$k)) {
    for (clipped_ in c(TRUE,FALSE)) {
      # p_ = 0.75
      # k_ = 100
      # clipped_ = TRUE
      n_ = k_
      
      df_plot_wp_nkp3 =
        plot_df_tmmWpESPN3 %>%
        filter(q_cutoff == 3.5 & r_cutoff == 3.5) %>%
        filter(p == p_ & k == k_ & n == n_) %>%
        mutate(clipped = clipped_) %>%
        filter(ifelse(clipped, 
                      0.6 <= rL & rL <= 0.8 & 0.6 <= qL & qL <= 0.8,
                      TRUE)) %>%
        mutate(
          facet_ = paste0("rL = ", rL, ", qL = ", qL),
          facet_ = factor(facet_),
          facet_ = fct_reorder(facet_, -qL)
        ) %>%
        mutate(
          n_ = paste0("n=", n, ",  "),
          qE_str = paste0(
            paste0("q",(1:6)[1:6 < q_cutoff[1]], collapse="="), "=qE,  "
          ),
          qL_str = paste0(
            paste0("q",(1:6)[1:6 > q_cutoff[1]], collapse="="), "=qL, "
          ),
          rE_str = paste0(
            paste0("r",(1:6)[1:6 < r_cutoff[1]], collapse="="), "=rE,  "
          ),
          rL_str = paste0(
            paste0("r",(1:6)[1:6 > r_cutoff[1]], collapse="="), "=rL"
          ),
          qr_ = paste0(qE_str, qL_str, rE_str, rL_str),
          # title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, "\n", qr_)
        ) 
      
      title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, ", ", df_plot_wp_nkp3$scoring_method[1], " score",
                      "\n", df_plot_wp_nkp3$qr_[1]) 
      
      plot_wp_nkp3 = df_plot_wp_nkp3 %>%
        ggplot(aes(x=rE, y=qE)) +
        facet_wrap(~facet_) +
        theme(panel.spacing = unit(2, "lines")) +
        labs(title = title_) +
        geom_tile(aes(fill=wp)) +
        geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
        theme(
          plot.title = element_text(size = if (clipped_) 37 else 70),
          strip.text.x = element_text(size = if (clipped_) 26 else 30),
          axis.text.x = element_text(size = if (clipped_) 35 else 40),
          axis.text.y = element_text(size = if (clipped_) 35 else 40),
          axis.title.x = element_text(size = if (clipped_) 40 else 80),
          axis.title.y = element_text(size = if (clipped_) 40 else 80),
          legend.title = element_text(size = if (clipped_) 40 else 70), 
          legend.text  = element_text(size = if (clipped_) 35 else 50),
          legend.key.size = unit(if (clipped_) 2 else 3, "lines")
        ) +
        scale_fill_gradientn(name="win\nprob.", colours = rev(terrain.colors(7)))
      # plot_wp_nkp3
      ggsave(
        paste0(output_folder, 
               "plot_wpTmmESPNScore_v3_k",format(k_, scientific=T),"_n",
               format(n_, scientific=T),"_p",p_, if (clipped_) "_clipped",".png"), 
        plot_wp_nkp3,
        width = if (!clipped_) 28 else 14,
        height = if (!clipped_) 26 else 13
      )
    }
  }
}
  
###########################################################
### Plot WP with Hamming Score for Grid 4 (qE, qL) ###
###########################################################

plot_df_tmmWpHam4 = read_csv(paste0(output_folder,"plot_grid_wpScore_v4.csv"))
plot_df_tmmWpHam4 = plot_df_tmmWpHam4 %>%
  group_by(p,qE,qL,rE,rL,q_cutoff,r_cutoff,n,k,scoring_method) %>%
  mutate(wp = mean(wp)) %>%
  ungroup() %>%
  filter(fold == 1) 
  
for (p_ in unique(plot_df_tmmWpHam4$p)) {
  for (k_ in unique(plot_df_tmmWpHam4$k)) {
    for (clipped_ in c(TRUE,FALSE)) {
      # p_ = 0.75
      # k_ = 100
      # clipped_ = TRUE
      n_ = k_
      
      df_plot_wp_nkp4 =
        plot_df_tmmWpHam4 %>%
        filter(q_cutoff == 1.5 & r_cutoff == 1.5) %>%
        filter(p == p_ & k == k_ & n == n_) %>%
        mutate(clipped = clipped_) %>%
        filter(ifelse(clipped, 
                      0.6 <= rL & rL <= 0.8 & 0.6 <= qL & qL <= 0.8,
                      # rL %in% c(0.6, 0.8, 1) & qL %in% c(0.6, 0.8, 1),
                      TRUE)) %>%
        mutate(
          facet_ = paste0("rL = ", rL, ", qL = ", qL),
          facet_ = factor(facet_),
          facet_ = fct_reorder(facet_, -qL)
        ) %>%
        mutate(
          n_ = paste0("n=", n, ",  "),
          qE_str = paste0(
            paste0("q",(1:6)[1:6 < q_cutoff[1]], collapse="="), "=qE,  "
          ),
          qL_str = paste0(
            paste0("q",(1:6)[1:6 > q_cutoff[1]], collapse="="), "=qL, "
          ),
          rE_str = paste0(
            paste0("r",(1:6)[1:6 < r_cutoff[1]], collapse="="), "=rE,  "
          ),
          rL_str = paste0(
            paste0("r",(1:6)[1:6 > r_cutoff[1]], collapse="="), "=rL"
          ),
          qr_ = paste0(qE_str, qL_str, rE_str, rL_str),
          # title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, "\n", qr_)
        ) 
      
      title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, ", ", df_plot_wp_nkp4$scoring_method[1], " score",
                      "\n", df_plot_wp_nkp4$qr_[1]) 
      
      plot_wp_nkp4 = df_plot_wp_nkp4 %>%
        ggplot(aes(x=rE, y=qE)) +
        facet_wrap(~facet_) +
        theme(panel.spacing = unit(4, "lines")) +
        labs(title = title_) +
        geom_tile(aes(fill=wp)) +
        geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
        theme(
          plot.title = element_text(size = if (clipped_) 37 else 70),
          strip.text.x = element_text(size = if (clipped_) 26 else 30),
          axis.text.x = element_text(size = if (clipped_) 35 else 40),
          axis.text.y = element_text(size = if (clipped_) 35 else 40),
          axis.title.x = element_text(size = if (clipped_) 40 else 80),
          axis.title.y = element_text(size = if (clipped_) 40 else 80),
          legend.title = element_text(size = if (clipped_) 40 else 70), 
          legend.text  = element_text(size = if (clipped_) 35 else 50),
          legend.key.size = unit(if (clipped_) 2 else 3, "lines")
        ) +
        scale_fill_gradientn(name="win\nprob.", colours = rev(terrain.colors(7)))
      # plot_wp_nkp4
      ggsave(
        paste0(output_folder, 
               "plot_wpTmmHammingScore_v4_k",format(k_, scientific=T),"_n",
               format(n_, scientific=T),"_p",p_, if (clipped_) "_clipped",".png"), 
        plot_wp_nkp4,
        width = if (!clipped_) 28 else 14,
        height = if (!clipped_) 26 else 13
      )
    }
  }
}
  
###########################################################
### Plot WP with ESPN Score for Grid 5 (qE, qL) ###
###########################################################

plot_df_tmmWpESPN5 = read_csv(paste0(output_folder,"plot_grid_wpScore_v5.csv"))
plot_df_tmmWpESPN5 = plot_df_tmmWpESPN5 %>%
  group_by(p,qE,qL,rE,rL,q_cutoff,r_cutoff,n,k,scoring_method) %>%
  mutate(wp = mean(wp)) %>%
  ungroup() %>%
  filter(fold == 1) 

for (p_ in unique(plot_df_tmmWpESPN5$p)) {
  for (k_ in unique(plot_df_tmmWpESPN5$k)) {
    for (clipped_ in c(TRUE,FALSE)) {
      # p_ = 0.75
      # k_ = 100
      # clipped_ = TRUE
      n_ = k_

      df_plot_wp_nkp5 =
        plot_df_tmmWpESPN5 %>%
        filter(q_cutoff == 1.5 & r_cutoff == 1.5) %>%
        filter(p == p_ & k == k_ & n == n_) %>%
        mutate(clipped = clipped_) %>%
        filter(ifelse(clipped, 
                      0.6 <= rL & rL <= 0.8 & 0.6 <= qL & qL <= 0.8,
                      TRUE)) %>%
        mutate(
          facet_ = paste0("rL = ", rL, ", qL = ", qL),
          facet_ = factor(facet_),
          facet_ = fct_reorder(facet_, -qL)
        ) %>%
        mutate(
          n_ = paste0("n=", n, ",  "),
          qE_str = paste0(
            paste0("q",(1:6)[1:6 < q_cutoff[1]], collapse="="), "=qE,  "
          ),
          qL_str = paste0(
            paste0("q",(1:6)[1:6 > q_cutoff[1]], collapse="="), "=qL, "
          ),
          rE_str = paste0(
            paste0("r",(1:6)[1:6 < r_cutoff[1]], collapse="="), "=rE,  "
          ),
          rL_str = paste0(
            paste0("r",(1:6)[1:6 > r_cutoff[1]], collapse="="), "=rL"
          ),
          qr_ = paste0(qE_str, qL_str, rE_str, rL_str),
          # title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, "\n", qr_)
        ) 
      
      title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, ", ", df_plot_wp_nkp5$scoring_method[1], " score",
                      "\n", df_plot_wp_nkp5$qr_[1]) 
      
      plot_wp_nkp5 = df_plot_wp_nkp5 %>%
        ggplot(aes(x=rE, y=qE)) +
        facet_wrap(~facet_) +
        theme(panel.spacing = unit(4, "lines")) +
        labs(title = title_) +
        geom_tile(aes(fill=wp)) +
        geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
        theme(
          plot.title = element_text(size = if (clipped_) 37 else 70),
          strip.text.x = element_text(size = if (clipped_) 26 else 30),
          axis.text.x = element_text(size = if (clipped_) 35 else 40),
          axis.text.y = element_text(size = if (clipped_) 35 else 40),
          axis.title.x = element_text(size = if (clipped_) 40 else 80),
          axis.title.y = element_text(size = if (clipped_) 40 else 80),
          legend.title = element_text(size = if (clipped_) 40 else 70), 
          legend.text  = element_text(size = if (clipped_) 35 else 50),
          legend.key.size = unit(if (clipped_) 2 else 3, "lines")
        ) +
        scale_fill_gradientn(name="win\nprob.", colours = rev(terrain.colors(7)))
      # plot_wp_nkp5
      ggsave(
        paste0(output_folder, 
               "plot_wpTmmESPNScore_v5_k",format(k_, scientific=T),"_n",
               format(n_, scientific=T),"_p",p_, if (clipped_) "_clipped",".png"), 
        plot_wp_nkp5,
        width = if (!clipped_) 28 else 14,
        height = if (!clipped_) 26 else 13
      )
      
    }
  }
}
