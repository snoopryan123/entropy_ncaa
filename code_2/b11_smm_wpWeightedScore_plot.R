

source("b11_smm_main.R")

plot_df2 = read_csv(paste0(output_folder,"plot_grid_wpMaxScore_v2.csv")) 
plot_df3 = read_csv(paste0(output_folder,"plot_grid_wpMaxScore_v3.csv")) 

# ### remove q,r = 0.5, 1
# plot_df2 = plot_df2 %>% filter(qE != 0.5 & qL != 0.5 & rE != 0.5 & rL != 0.5 &
#                                qE != 1 & qL != 1 & rE != 1 & rL != 1)
# plot_df3 = plot_df3 %>% filter(qE != 0.5 & qL != 0.5 & rE != 0.5 & rL != 0.5 &
#                                  qE != 1 & qL != 1 & rE != 1 & rL != 1)

# plot_df2 = plot_df2 %>%
#   group_by(q_cutoff,r_cutoff,n) %>%
#   mutate(max_wp = ifelse(wp == max(wp), wp, NA)) %>%
#   ungroup()

# my_palette_nk1 = c(
#   rev(brewer.pal(name="PuRd",n=9)[4:9]),
#   brewer.pal(name="Blues",n=9)[4:9]
# )

#############
### PLOTS ###
#############

#### WP, p, n, q, k, r

for (p_ in unique(plot_df2$p)) {
  for (k_ in unique(plot_df2$k)) {
    for (clipped_ in c(TRUE,FALSE)) {
      # p_ = 0.75
      # k_ = 100
      # clipped_ = TRUE
      n_ = k_
      # for (n_ in unique(plot_df2$n)) {
      
      # plot_wp_nkp =
      #   plot_df2 %>%
      #   filter(q_cutoff == 3.5 & r_cutoff == 3.5) %>%
      #   filter(p == p_ & k == k_ & n == n_) %>%
      #   mutate(
      #     facet_ = paste0("rL = ", rL, ", qL = ", qL),
      #     facet_ = factor(facet_),
      #     facet_ = fct_reorder(facet_, -qL)
      #   ) %>%
      #   mutate(
      #     n_ = paste0("n=", n, ",  "),
      #     qE_str = paste0(
      #       paste0("q",(1:6)[1:6 < q_cutoff[1]], collapse="="), "=qE,  "
      #     ),
      #     qL_str = paste0(
      #       paste0("q",(1:6)[1:6 > q_cutoff[1]], collapse="="), "=qL, "
      #     ),
      #     rE_str = paste0(
      #       paste0("r",(1:6)[1:6 < r_cutoff[1]], collapse="="), "=rE,  "
      #     ),
      #     rL_str = paste0(
      #       paste0("r",(1:6)[1:6 > r_cutoff[1]], collapse="="), "=rL"
      #     ),
      #     qr_ = paste0(qE_str, qL_str, rE_str, rL_str),
      #     # title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, "\n", qr_)
      #   ) 
      # 
      # title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, "\n", plot_wp_nkp$qr_[1]) 
      # 
      # plot_wp_nkp = plot_wp_nkp %>%
      #   ggplot(aes(x=rE, y=qE)) +
      #   facet_wrap(~facet_) +
      #   theme(panel.spacing = unit(2, "lines")) +
      #   labs(title = title_) +
      #   geom_tile(aes(fill=wp)) +
      #   geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
      #   geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
      #   geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
      #   theme(text = element_text(size=40)) +
      #   theme(strip.text.x = element_text(size = 22)) +
      #   # scale_fill_gradientn(name="win\nprobability", colours = terrain.colors(7))
      #   scale_fill_gradientn(name="win\nprobability", colours = rev(terrain.colors(7)))
      # # plot_wp_nkp
      # ggsave(
      #   paste0(output_folder, "plot_wpMaxWeightedScore_k",format(k_, scientific=T),"_n",format(n_, scientific=T),"_p",p_,".png"), 
      #   plot_wp_nkp,
      #   # width=12,height=10
      #   width=28,height=26
      # )
      
      ############################################################################
      
      plot_wp_nkp2 =
        plot_df2 %>%
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
      
      title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, ", ", plot_wp_nkp2$score_method[1]," score",
                      "\n", plot_wp_nkp2$qr_[1]) 
      
      plot_wp_nkp2 = plot_wp_nkp2 %>%
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
          strip.text.x = element_text(size = if (clipped_) 28 else 30),
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
               "plot_wpMaxWeightedScore2_k",format(k_, scientific=T),"_n",
               format(n_, scientific=T),"_p",p_, if (clipped_) "_clipped",".png"), 
        plot_wp_nkp2,
        width = if (!clipped_) 28 else 14,
        height = if (!clipped_) 26 else 13
        # width=12,height=10
        # width=28,height=26
        # width=20,height=16
      )
      
      ############################################################################
      
      plot_wp_nkp3 =
        plot_df3 %>%
        filter(q_cutoff == 1.5 & r_cutoff == 1.5) %>%
        filter(p == p_ & k == k_ & n == n_) %>%
        # mutate(facet_ = paste0("rE = ", rE, ", qE = ", qE)) %>%
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
      
      title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, ", ", plot_wp_nkp3$score_method[1]," score",
                      "\n", plot_wp_nkp3$qr_[1]) 
      
      plot_wp_nkp3 = plot_wp_nkp3 %>%
        ggplot(aes(x=rE, y=qE)) +
        facet_wrap(~facet_) +
        theme(panel.spacing = unit(3, "lines")) +
        labs(title = title_) +
        geom_tile(aes(fill=wp)) +
        geom_vline(aes(xintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_hline(aes(yintercept = p_), color="gray60", linetype="dashed", linewidth=0.5) +
        geom_abline(aes(slope = 1, intercept=0), color="gray60", linetype="dashed", linewidth=0.5) +
        theme(
          plot.title = element_text(size = if (clipped_) 37 else 70),
          strip.text.x = element_text(size = if (clipped_) 28 else 30),
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
               "plot_wpMaxWeightedScore3_k",format(k_, scientific=T),"_n",
               format(n_, scientific=T),"_p",p_, if (clipped_) "_clipped",".png"), 
        plot_wp_nkp3,
        width = if (!clipped_) 28 else 14,
        height = if (!clipped_) 26 else 13
        # width=12,height=10
        # width=28,height=26
        # width=20,height=16
      )
    
    }
  }
}

