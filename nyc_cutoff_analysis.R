
library(tidyverse)
library(viridis)
library(boot)


#Read reddit reports and NYC standards
df_raw <- read_csv("nyc_2024_cutoff_reports.csv",
                   col_types = "cccic") %>%
  mutate(gender = str_sub(age_gender_cat,1,1))
df_raw %>% glimpse()


df_standards <- read_csv("nyc_standards.csv", col_types = "cccc")
df_standards %>% glimpse()


#NYRR 10k conversion factors, following
#https://www.nyrr.org/run/guidelines-and-procedures/race-procedures/calculated-pace-and-corral-updates
hm_mult <- 0.45
m_mult <- 0.22


#Vectorized time splitter (hh:mm:ss to decimal minutes)
time_to_min <- function(t) {
  # Split each string in the vector
  parts <- strsplit(t, ":")
  
  # Convert the parts to numeric values
  hh <- sapply(parts, function(x) as.numeric(x[1]))
  mm <- sapply(parts, function(x) as.numeric(x[2]))
  ss <- sapply(parts, function(x) as.numeric(x[3]))
  
  # Calculate total time in decimal minutes for each row
  total_minutes <- hh * 60 + mm + ss / 60
  
  return(total_minutes)
}

#clean
df <- df_raw %>%
  #Convert to NYRR equivalent 10k
  mutate(event_time_min = time_to_min(reported_time),
         equivalent_10k = ifelse(qualifying_event == "HM",
                                 event_time_min*hm_mult,
                                 event_time_min*m_mult)) %>%
  #Combine with age/gender category standards; convert to 10k equivalent
  left_join(df_standards, by="age_gender_cat") %>%
  mutate(m_standard_10k = m_mult*time_to_min(m_standard),
         hm_standard_10k = hm_mult*time_to_min(hm_standard)) %>%
  #Calculate time under marathon standard (as 10k eq.) + time under event entered (M or HM)
  mutate(time_under_10k_m = m_standard_10k - equivalent_10k,
         time_under_10k_event = ifelse(qualifying_event == "HM",
                                       hm_standard_10k - equivalent_10k,
                                       m_standard_10k - equivalent_10k))

df %>% glimpse()


#Subset only precise times
df_precise <- df %>% filter(precise == "yes")

df_precise %>% 
  ggplot(aes(x=time_under_10k_m, y=entry_successful)) +
  geom_point()

# Fit logistic regression
mod_precise <- glm(entry_successful ~ time_under_10k_m, data = df_precise, family="binomial")
summary(mod_precise) #Probs near 0/1 are expected

# p(success) = 0.5 where the linear predictor is zero
# b_0 + b_1 * x = 0
# -b_0/b_1 = x at p == 0.5

cutoff_est_precise <- -1*coef(mod_precise)[1]/coef(mod_precise)[2]
print(cutoff_est_precise) #4.067 minutes
print(sprintf("Precise time marathon cutoff estimate: %.2f minutes", 
              cutoff_est_precise/m_mult))


# -- Try with all entries

mod_all <- glm(entry_successful ~ time_under_10k_m, data = df, family="binomial")
summary(mod_all) 

cutoff_est_all <- -1*coef(mod_all)[1]/coef(mod_all)[2]
print(cutoff_est_all) #3.959 minutes, very close
print(sprintf("All submitted times marathon cutoff estimate: %.2f minutes", 
              cutoff_est_all/m_mult))



mod_test <- glm(entry_successful ~ time_under_10k_m + gender, data = df, family="binomial")
summary(mod_test) 


# --- Plotting ---- 

#Predictions
ng <- 201
df_grid <- data.frame(time_under_10k_m = seq(0,10, length.out = ng))
df_grid$phat_precise <- predict.glm(mod_precise, df_grid, type="response")
df_grid$phat_all <- predict.glm(mod_all, df_grid, type="response")


# Plot controls
sz <- 1.5
sz2 <- 2.5
alf <- 0.5
lwd <- 1.0
lwd2 <- 1.5
all_alf <- 0.3

lwd_cut <- 0.5
fnt_ant <- 3.5

df_approx <- df %>% filter(precise == "no")

logit_plt <- df_precise %>%
  #Dummy point to get level to show up
  add_row(time_under_10k_m=NA, entry_successful=NA, precise="no", gender="M") %>%
  mutate(precise = factor(precise, levels=c("no", "yes"))) %>%
  ggplot(aes(x=time_under_10k_m, y=entry_successful)) +
  #Precise times
  geom_point(aes(color=precise), pch=16,
             size = sz2, position = position_jitter(width=0, height=0.01, seed=42)) + 
  #Estimated times
  geom_point(size = sz,  pch=16,
             position = position_jitter(width=0, height=0.01, seed=42),
             alpha = all_alf, data = df_approx, color = "#e41a1c") + 
  geom_line(aes(y=phat_precise, color=NULL), data = df_grid, color = "#377eb8",
            linewidth = lwd2, alpha = alf) + 
  geom_line(aes(y=phat_all, color=NULL), data = df_grid, color = "#e41a1c",
            linewidth = lwd, alpha = all_alf) + 
  annotate(geom="text", x=4.2,y=0.22, label = "4:04 under 10k equivalent", 
           size=fnt_ant, color="#377eb8", hjust=0) + 
  #Cutoff estimate
  geom_vline(xintercept = cutoff_est_precise, linetype = "solid", 
             linewidth = lwd_cut, color = "#377eb8") + 
  # geom_hline(yintercept = 0.5, linetype = "dotted", linewidth = lwd_cut, color = "#377eb8") + 
  scale_y_continuous(limits = c(-0.02,1.02), name = "Entry successful?", 
                     breaks = c(0,1),
                     labels = c("No", "Yes")) + 
  coord_cartesian(xlim=c(0,7.5)) +
  facet_wrap(~gender) + 
  ggtitle("NYC Marathon 2024 cutoff: -4:04 10k equivalent time") +
  labs(x = "Time under 10k-equivalent marathon standard (min)", caption = "/u/running_writings") + 
  #Scale yes/no
  scale_color_manual(values = c("yes" = "#377eb8", "no" = "#e41a1c"), drop=FALSE,
                     name = "Precise race time: ") + 
  theme_bw() + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust=0.5))

logit_plt


ggsave("Logistic regression plot.png", plot=logit_plt,
       width = 1600, height = 1200, units = "px")


# -- Compute actual cutoff in M/HM time ---
cutoff_est_precise


df_res <- df %>%
  mutate(cutoff_m_standard_10k = m_standard_10k - cutoff_est_precise,
         cutoff_m = cutoff_m_standard_10k/m_mult,
         cutoff_hm = cutoff_m_standard_10k/hm_mult) %>%
  mutate(m_time_under = time_to_min(m_standard) - cutoff_m,
         hm_time_under = time_to_min(hm_standard) - cutoff_hm) 



dec_to_time <- function(decimal_minutes){
  total_seconds <- round(decimal_minutes * 60)
  hours <- total_seconds %/% 3600
  minutes <- (total_seconds %% 3600) %/% 60
  seconds <- total_seconds %% 60
  # Format the string as hh:mm:ss
  time_str <- sprintf("%01d:%02d:%02d", hours, minutes, seconds)
  return(time_str)
}

dec_to_min_sec <- function(decimal_minutes){
  total_seconds <- round(decimal_minutes * 60)
  hours <- total_seconds %/% 3600
  minutes <- (total_seconds %% 3600) %/% 60
  seconds <- total_seconds %% 60
  # Format the string as hh:mm:ss
  time_str <- ifelse(decimal_minutes >= 0, 
                     sprintf("+%d:%02d", minutes, seconds), 
                     sprintf("-%d:%02d", minutes, seconds))
  
  return(time_str)
}


df_qual <- df_standards %>%
  mutate(gender = str_sub(age_gender_cat,1,1)) %>%
  mutate(m_10k_eq = time_to_min(m_standard)*m_mult,
         hm_10k_eq = time_to_min(hm_standard)*hm_mult,
         bq_10k_eq = time_to_min(boston_standard)*m_mult) %>%
  #Calculate time under needed for each
  mutate(m_10k_q_time = m_10k_eq - cutoff_est_precise,
         hm_10k_q_time = hm_10k_eq - cutoff_est_precise,
         m_q_time = m_10k_q_time/m_mult,
         hm_q_time = hm_10k_q_time/hm_mult,
         m_q_time_under = time_to_min(m_standard) - m_q_time,
         hm_q_time_under = time_to_min(hm_standard) - hm_q_time) %>%
  #to string
  mutate(m_q_time_string = dec_to_time(m_q_time),
         hm_q_time_string = dec_to_time(hm_q_time),
         boston_q_time = time_to_min(boston_standard),
         m_q_under_boston = m_q_time - boston_q_time,
         under_boston_string = dec_to_min_sec(m_q_under_boston)) 



sz_time <- 2.5
bar_w <- 0.75
fnt <- 9 #axes


#Special white labels for above BQ times
f_df <- df_qual %>%
  mutate(gender = factor(gender, levels = c("M", "F"))) %>%
  filter(age_gender_cat %in% c("F70-74","F75-79","F80+")) %>%
  mutate()


bq_col <- "#333333"


custom_labels <- c(
  "M" = "Male",
  "F" = "Female / Nonbinary"
)


bq_plot <- df_qual %>%
  mutate(gender = factor(gender, levels = c("M", "F"))) %>%
  ggplot(aes(x=m_q_under_boston, y=age_gender_cat)) + 
  geom_col(aes(fill = m_q_under_boston), width = bar_w) + 
  #Qtime text
  geom_text(aes(label=m_q_time_string, x=2), hjust=0, size=sz_time) +
  geom_text(aes(label=m_q_time_string, x=2), hjust=0,
            data = f_df, color="white", size=sz_time) +
  #BQ ref, scales
  geom_vline(xintercept=0, linetype="solid", color=bq_col) + 
  scale_y_discrete(limits=rev, name=NULL) + 
  ggtitle("Qualifying for NYC 2024 was much harder than a BQ") +
  labs(caption="/u/running_writings") + 
  scale_x_continuous(limits = c(-40,12), oob=scales::squish,
                     expand = c(0,0),
                     breaks = seq(-40,0, by=10),
                     labels = c("-40:00","-30:00","-20:00","-10:00","BQ"),
                     name = "Minutes below BQ") + 
  scale_fill_viridis(option = "mako", limits = c(-40,15), direction = -1,
                     oob=scales::squish) + 
  #Facet themes
  facet_wrap(~gender, scales="free_y", labeller = as_labeller(custom_labels)) + 
  theme_bw() + 
  theme(legend.position = "none", 
        plot.caption = element_text(size=7),
        axis.text = element_text(size = fnt),
        axis.title.x = element_text(size=fnt),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing = unit(1, "lines"))

bq_plot


ggsave("NYC marathon qualifying time vs BQ.png", plot=bq_plot,
       width = 1600, height = 1000, units = "px")



# --- 95% uncertainty via bootstraping

#Extract x at p=0.5 for each replicate
extract_prob <- function(data, indices){
  d <- data[indices,]
  m <- glm(entry_successful ~ time_under_10k_m, data = d, family="binomial")
  cut_est <- -1*coef(m)[1]/coef(m)[2]
  return(cut_est)
}

n_boot <- 10000 #n reps
set.seed(1989) #a good year
boot_precise <- boot(data = df_precise, statistic = extract_prob, R = n_boot)
ci_precise <- boot.ci(boot_precise, type = "perc", conf=0.95)

# -- again but with all data --
boot_all <- boot(data = df, statistic = extract_prob, R = n_boot)
ci_all <- boot.ci(boot_all, type = "perc", conf=0.90)


ci_precise$percent[4:5]/m_mult
ci_all$percent[4:5]/m_mult






custom_labels <- c(
  "M" = "Male",
  "F" = "Female / Nonbinary"
)


gender_plt <- df_precise %>%
  #Dummy point to get level to show up
  add_row(time_under_10k_m=NA, entry_successful=NA, precise="no", gender="M") %>%
  mutate(precise = factor(precise, levels=c("no", "yes"))) %>%
  ggplot(aes(x=time_under_10k_m, y=entry_successful)) +
  #Precise times
  geom_point(aes(color=precise), pch=16,
             size = sz2, position = position_jitter(width=0, height=0.01, seed=42)) + 
  #Estimated times
  geom_point(size = sz,  pch=16,
             position = position_jitter(width=0, height=0.01, seed=42),
             alpha = all_alf, data = df_approx, color = "#e41a1c") + 
  geom_line(aes(y=phat_precise, color=NULL), data = df_grid, color = "#377eb8",
            linewidth = lwd2, alpha = alf) + 
  geom_line(aes(y=phat_all, color=NULL), data = df_grid, color = "#e41a1c",
            linewidth = lwd, alpha = all_alf) + 
  annotate(geom="text", x=4.2,y=0.22, label = "4:04 under 10k equivalent", 
           size=fnt_ant, color="#377eb8", hjust=0) + 
  #Cutoff estimate
  geom_vline(xintercept = cutoff_est_precise, linetype = "solid", 
             linewidth = lwd_cut, color = "#377eb8") + 
  # geom_hline(yintercept = 0.5, linetype = "dotted", linewidth = lwd_cut, color = "#377eb8") + 
  scale_y_continuous(limits = c(-0.02,1.02), name = "Entry successful?", 
                     breaks = c(0,1),
                     labels = c("No", "Yes")) + 
  coord_cartesian(xlim=c(0,7.5)) +
  facet_wrap(~gender, labeller = as_labeller(custom_labels)) + 
  ggtitle("NYC Marathon 2024 cutoff: -4:04 10k equivalent time") +
  labs(x = "Time under 10k-equivalent marathon standard (min)", caption = "/u/running_writings") + 
  #Scale yes/no
  scale_color_manual(values = c("yes" = "#377eb8", "no" = "#e41a1c"), drop=FALSE,
                     name = "Precise race time: ") + 
  theme_bw() + 
  theme(legend.position = "bottom",
        plot.title = element_text(hjust=0.5))

gender_plt


ggsave("Gender regression plot.png", plot=gender_plt,
       width = 1600, height = 1200, units = "px")