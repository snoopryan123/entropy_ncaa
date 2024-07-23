

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
      mutate(
        facet_ = paste0("rL = ", rL, ", qL = ", qL),
        facet_ = factor(facet_),
        facet_ = fct_reorder(facet_, -qL)
      ) %>%
      mutate(
        # n_ = paste0("n=", n, ",  "),
        n_ = paste0("n=", format_comma(n), ",  "),
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
    
    title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, ", ESPN score",
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
      theme(text = element_text(size=40)) +
      # theme(text = element_text(size=35)) +
      theme(strip.text.x = element_text(size = 22)) +
      # scale_fill_gradientn(name="win\nprobability", colours = terrain.colors(7))
      scale_fill_gradientn(name="win\nprobability", colours = rev(terrain.colors(7)))
    # plot_wp_nkp
    ggsave(
      paste0(output_folder, "plot_wpMaxWeightedScore2_k",format(k_, scientific=T),"_n",format(n_, scientific=T),"_p",p_,".png"), 
      plot_wp_nkp2,
      # width=12,height=10
      width=28,height=26
      # width=18,height=16
    )
    
    ############################################################################
    
    plot_wp_nkp3 =
      plot_df3 %>%
      filter(q_cutoff == 1.5 & r_cutoff == 1.5) %>%
      filter(p == p_ & k == k_ & n == n_) %>%
      # mutate(facet_ = paste0("rE = ", rE, ", qE = ", qE)) %>%
      mutate(
        facet_ = paste0("rL = ", rL, ", qL = ", qL),
        facet_ = factor(facet_),
        facet_ = fct_reorder(facet_, -qL)
      ) %>%
      mutate(
        # n_ = paste0("n=", n, ",  "),
        n_ = paste0("n=", format_comma(n), ",  "),
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
    
    title_ = paste0("p = ", p_, ", n = ", n_, ", k = ", k_, ", ESPN score",
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
      theme(text = element_text(size=40)) +
      theme(strip.text.x = element_text(size = 22)) +
      # scale_fill_gradientn(name="win\nprobability", colours = terrain.colors(7))
      scale_fill_gradientn(name="win\nprobability", colours = rev(terrain.colors(7)))
    # plot_wp_nkp
    ggsave(
      paste0(output_folder, "plot_wpMaxWeightedScore3_k",format(k_, scientific=T),"_n",format(n_, scientific=T),"_p",p_,".png"), 
      plot_wp_nkp3,
      # width=13,height=10
      width=28,height=26
      # width=18,height=16
    )
  
    # }
  }
}

