#' This script plots the sero-prevalence estimates (weighted, unweighted,
#' split according to the vaccintation status, etc.)

here_koco_data = function (...) here::here("Data", ...)
here_koco_prev = function (...) here::here("Seroprevalence", ...)
here_prev_figures = function (...) here_koco_prev("Figures", ...)
here_prev_results = function (...) here_koco_prev("Results", ...)
here_prev_scripts = function (...) here_koco_prev("Scripts", ...)


# Number of participants for the different rounds
n_ind <- c(5313, 4456, 4382, 3971, 3838)

# Dates
Date <- seq(as.Date("2020-02-01"), as.Date("2021-11-01"), by = "1 month")

# Labels
Labels <- rep("", length(Date))
Labels[Date %in% as.Date(c("2020-02-01", "2020-05-01", "2020-12-01", "2021-03-01", "2021-08-01", "2021-11-01"))] <- 
  paste(c("Feb 2020", "May 2020", "Dec 2020", "Mar 2021", "Aug 2021", "Nov 2021"), "\n",
        c("", "Round 1", "Round 2", "Round 3", "Round 4", "Round 5"), "\n",
        c("", paste0("(n=", n_ind, ")")), sep = "")

# Adjust size legend
large_points <- function(data, params, size) {
  # Multiply by some number
  data$size <- data$size * 2
  draw_key_point(data = data, params = params, size = size)
}


##################################
# Plot cumulative Seroprevalence #
##################################

###
# Load results
###

# Unweighted
sp_uw <- readRDS(here_prev_results("sp_uw.RDS"))

# Weighted
sp_w_r1 <- read.csv(here_prev_results("r1_sp_w.csv"))
sp_w_r2 <- read.csv(here_prev_results("r2_cum_sp_w.csv"))
sp_w_r3 <- read.csv(here_prev_results("r3_cum_sp_w.csv"))
sp_w_r3_no_cal <- read.csv(here_prev_results("r3_cum_sp_w_no_cal.csv"))
sp_w_r4 <- read.csv(here_prev_results("r4_cum_sp_w.csv"))
sp_w_r4_no_cal <- read.csv(here_prev_results("r4_cum_sp_w_no_cal.csv"))
sp_w_r5 <- read.csv(here_prev_results("r5_cum_sp_w.csv"))
sp_w_r5_no_cal <- read.csv(here_prev_results("r5_cum_sp_w_no_cal.csv"))

                                  

###
# Create table for the plot
###


res <- rbind(rep(0, 3),
             sp_uw[sp_uw$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")],
             rep(0, 3),
             sp_w_r1[sp_w_r1$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r2[sp_w_r2$Calculation == "All" & sp_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r3_no_cal[sp_w_r3_no_cal$calculation == "N_pos" & sp_w_r3_no_cal$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r4_no_cal[sp_w_r4_no_cal$calculation == "N_pos" & sp_w_r4_no_cal$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r5_no_cal[sp_w_r5_no_cal$calculation == "N_pos" & sp_w_r5_no_cal$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             rep(0, 3),
             sp_w_r1[sp_w_r1$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r2[sp_w_r2$Calculation == "All" & sp_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r3[sp_w_r3$calculation == "N_pos" & sp_w_r3$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r4[sp_w_r4$calculation == "N_pos" & sp_w_r4$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r5[sp_w_r5$calculation == "N_pos" & sp_w_r5$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             rep(0, 3),
             c(6216/1323142, NA, NA),
             c(41365/1324834, NA, NA),
             c(54607/1324232, NA, NA),
             c(71671/1325336, NA, NA),
             c(86322/1325949, NA, NA)
)


# Add dates
res$date = rep(as.Date(c("2020-02-01", "2020-05-01", "2020-12-01", "2021-03-01", "2021-08-01","2021-11-01")), 4)

# Add legend
res$Prevalence <- factor(rep(c("Unweighted", "Weighted - No Cal. Vacc.", "Weighted - Cal. Vacc.", "Official"), each = 6),
                         levels = c( "Unweighted", "Weighted - No Cal. Vacc.", "Weighted - Cal. Vacc.", "Official"))


# Underreporting factor
res[res$Prevalence == "Weighted - Cal. Vacc.", c("estimate", "lb_ci", "ub_ci")]/
  res$estimate[res$Prevalence == "Official"]

###
# Plot
###


# Colors
col <- RColorBrewer::brewer.pal(4, "Set2")

g_prev_inf <- ggplot(res, aes(x = date, y = estimate, group = Prevalence)) +
  geom_vline(xintercept =  as.Date(c("2020-02-01", "2020-05-01", 
                                     "2020-12-01", 
                                     "2021-03-01", 
                                     "2021-08-01", "2021-11-01")),
             colour = "grey92") +
  geom_hline(yintercept =  seq(0,0.17,0.02),
             colour = "grey92") +
  geom_line(aes(col = Prevalence, linetype = Prevalence), size = 1) + 
  geom_point(aes(col = Prevalence, shape = Prevalence), size = 2, key_glyph = large_points) +
  scale_color_manual(values = col) + 
  theme_classic() + 
  scale_y_continuous(labels=scales::label_percent(accuracy = 1L), limits = c(0, 0.17), n.breaks = 10) +
  scale_x_date(labels = Labels,
               breaks = Date) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid", "dotdash")) +
  geom_ribbon(aes(ymin = lb_ci, ymax = ub_ci),
              alpha = 0.15, fill = rep(col, each = 6)) +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11), 
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=9),
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        # legend.title = element_text(size=15),
        panel.grid.minor.x = element_blank(),
        legend.position="bottom") +
  guides(col = guide_legend(override.aes = list(size=0.8))) +
  ylab("Prevalence infection (%)") 


#######################
# Plots Seroincidence #
#######################

###
# Load results
###

# Unweighted
sp_uw <- readRDS(here_prev_results("sp_uw.RDS"))
si_uw <- readRDS(here_prev_results("si_uw.RDS"))

# Weighted
sp_w_r1 <- read.csv(here_prev_results("r1_sp_w.csv"))
si_w_r2 <- read.csv(here_prev_results("r2_cum_sp_w.csv"))
si_w_r3 <- read.csv(here_prev_results("r3_cum_sp_w.csv"))
si_w_r3_no_cal <- read.csv(here_prev_results("r3_cum_sp_w_no_cal.csv"))
si_w_r4 <- read.csv(here_prev_results("r4_cum_sp_w.csv"))
si_w_r4_no_cal <- read.csv(here_prev_results("r4_cum_sp_w_no_cal.csv"))
si_w_r5 <- read.csv(here_prev_results("r5_cum_sp_w.csv"))
si_w_r5_no_cal <- read.csv(here_prev_results("r5_cum_sp_w_no_cal.csv"))

###
# Create table for the plot
###

res <- rbind(rep(0, 3),
             sp_uw[sp_uw$round == "R1" & sp_uw$Adjust == "adjusted",
                   c("estimate", "lb_ci", "ub_ci")],
             si_uw[si_uw$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")],
             rep(0, 3),
             sp_w_r1[sp_w_r1$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r2[si_w_r2$Calculation == "Negative_Positive" & si_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r3_no_cal[si_w_r3_no_cal$calculation == "Negative_Positive" & si_w_r3_no_cal$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r4_no_cal[si_w_r4_no_cal$calculation == "Negative_Positive" & si_w_r4_no_cal$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r5_no_cal[si_w_r5_no_cal$calculation == "Negative_Positive" & si_w_r5_no_cal$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             rep(0, 3),
             sp_w_r1[sp_w_r1$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r2[si_w_r2$Calculation == "Negative_Positive" & si_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r3[si_w_r3$calculation == "Negative_Positive" & si_w_r3$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r4[si_w_r4$calculation == "Negative_Positive" & si_w_r4$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r5[si_w_r5$calculation == "Negative_Positive" & si_w_r5$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100
)



# Add dates
res$date = rep(as.Date(c("2020-02-01", "2020-05-01", "2020-12-01", "2021-03-01", "2021-08-01", "2021-11-01")), 3)

# Add legend
res$Incidence <- factor(rep(c("Unweighted", "Weighted - No Cal. Vacc.", "Weighted - Cal. Vacc."), each = 6),
                        levels = c("Unweighted", "Weighted - No Cal. Vacc.", "Weighted - Cal. Vacc."))


###
# Plot
###

# Colors
col <- RColorBrewer::brewer.pal(3, "Set2")

g_inc_inf <- ggplot(res, aes(x = date, y = estimate, group = Incidence)) +
  geom_vline(xintercept =  as.Date(c("2020-02-01", "2020-05-01", 
                                     "2020-12-01", 
                                     "2021-03-01", 
                                     "2021-08-01",
                                     "2021-11-01")),
             colour = "grey92") +
  geom_hline(yintercept =  seq(0,0.04,0.005),
             colour = "grey92") +
  geom_line(aes(col = Incidence, linetype = Incidence), size = 1) + 
  geom_point(aes(col = Incidence, shape = Incidence), size = 2, key_glyph = large_points) +
  scale_color_manual(values = col) + 
  theme_classic() +
  scale_y_continuous(labels=scales::label_percent(accuracy = 1L), limits = c(0, 0.04), n.breaks = 4) +
  scale_x_date(labels = Labels,
               breaks = Date) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  geom_ribbon(aes(ymin = lb_ci, ymax = ub_ci),
              alpha = 0.15, fill = rep(col, each = 6)) +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11), 
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=9),
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom") +
  guides(col = guide_legend(override.aes = list(size=0.8))) +
  ylab("Incidence infection (%)") 

########################################################
# Plot cumulative Seroprevalence by vaccination status #
########################################################

###
# Load results
###

# Weighted
sp_w_r1 <- read.csv(here_prev_results("r1_sp_w.csv"))
sp_w_r2 <- read.csv(here_prev_results("r2_cum_sp_w.csv"))
sp_w_r3 <- read.csv(here_prev_results("r3_cum_sp_w.csv"))
sp_w_r4 <- read.csv(here_prev_results("r4_cum_sp_w.csv"))
sp_w_r5 <- read.csv(here_prev_results("r5_cum_sp_w.csv"))



###
# Create table for the plot
###


res <- rbind(rep(0, 3),
             sp_w_r1[sp_w_r1$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100, 
             sp_w_r2[sp_w_r2$Calculation == "All" & sp_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             c(sp_w_r3$estimate[sp_w_r3$calculation == "N_pos" & sp_w_r3$adjust == "adjusted"]/100, NA, NA),
             c(sp_w_r4$estimate[sp_w_r4$calculation == "N_pos" & sp_w_r4$adjust == "adjusted"]/100, NA, NA),
             c(sp_w_r5$estimate[sp_w_r5$calculation == "N_pos" & sp_w_r5$adjust == "adjusted"]/100, NA, NA),
             sp_w_r2[sp_w_r2$Calculation == "All" & sp_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r2[sp_w_r2$Calculation == "All" & sp_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r3[sp_w_r3$calculation %in% c("Non_vax", "Vax") & sp_w_r3$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r4[sp_w_r4$calculation %in% c("Non_vax", "Vax") & sp_w_r4$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r5[sp_w_r5$calculation %in% c("Non_vax", "Vax") & sp_w_r5$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100
)


# Add dates
res$date = c(as.Date(c("2020-02-01", "2020-05-01", "2020-12-01", "2021-03-01", "2021-08-01","2021-11-01")),
             rep(as.Date(c("2020-12-01", "2021-03-01", "2021-08-01","2021-11-01")), each = 2)
)


# Add legend
res$Calculation <- factor(c(rep("Together", 6), rep(c("Non-vaccinated", "Vaccinated"), 4)),
                          levels = c("Non-vaccinated", "Vaccinated", "Together"))


###
# Plot
###

# Colors
col <- RColorBrewer::brewer.pal(3, "Set2")

small_rect <- function(data, params, size) {
  # Multiply by some number
  data$size <- data$size / 1.1
  draw_key_rect(data = data, params = params, size = size)
}

g_prev_vax <- ggplot(res, aes(x = date, y = estimate, group = Calculation)) +
  geom_vline(xintercept =  as.Date(c("2020-02-01", "2020-05-01", 
                                     "2020-12-01", 
                                     "2021-03-01", 
                                     "2021-08-01", "2021-11-01")),
             colour = "grey92") +
  geom_hline(yintercept =  seq(0,0.29,0.05),
             colour = "grey92") +
  geom_line(aes(linetype = Calculation), col = col[3], size = 1) +
  geom_point(aes(shape = Calculation), col = col[3], size = 2, key_glyph = large_points) +
  theme_classic() + #ggtitle("Cumulative prevalence in Munich (age > 13 yrs)") +
  # scale_shape_manual(values=c(3, 16, 17)) +
  scale_y_continuous(labels=scales::label_percent(accuracy = 1L), limits = c(0, 0.29), n.breaks = 10) +
  scale_x_date(labels = Labels,
               breaks = Date) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  geom_ribbon(aes(ymin = lb_ci, ymax = ub_ci, linetype = Calculation),
              alpha = 0.15, fill = col[3], col = "#8DA0CB70") +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11), 
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=9),
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position="bottom") +
  guides(shape = guide_legend(override.aes = list(size=0.8))) +
  ylab("Prevalence infection (%)") 



########################################
# Plots Seroincidence and BTI and INS  #
########################################

###
# Load results
###

# Weighted
sp_w_r1 <- read.csv(here_prev_results("r1_sp_w.csv"))
si_w_r2 <- read.csv(here_prev_results("r2_cum_sp_w.csv"))
si_w_r3 <- read.csv(here_prev_results("r3_cum_sp_w.csv"))
si_w_r4 <- read.csv(here_prev_results("r4_cum_sp_w.csv"))
si_w_r5 <- read.csv(here_prev_results("r5_cum_sp_w.csv"))



###
# Create table for the plot
###

res <- rbind(rep(0, 3),
             sp_w_r1[sp_w_r1$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r2[si_w_r2$Calculation == "Negative_Positive" & si_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r3[si_w_r3$calculation == "Negative_Positive" & si_w_r3$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             c(si_w_r4$estimate[si_w_r4$calculation == "Negative_Positive"]/100, NA, NA),
             c(si_w_r5$estimate[si_w_r5$calculation == "Negative_Positive"]/100, NA, NA),
             si_w_r3[si_w_r3$calculation == "Negative_Positive" & si_w_r3$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r3[si_w_r3$calculation == "Negative_Positive" & si_w_r3$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r4[si_w_r4$calculation %in% c("Negative_Positive_Non_vax", "Negative_Positive_Vax") & si_w_r4$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             si_w_r5[si_w_r5$calculation %in% c("Negative_Positive_Non_vax", "Negative_Positive_Vax") & si_w_r5$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100
)

# Set negative lower bound CI to 0

res$lb_ci[res$lb_ci < 0] <- 0

# Add dates
res$date = c(as.Date(c("2020-02-01", "2020-05-01", "2020-12-01", "2021-03-01", "2021-08-01","2021-11-01")),
             rep(as.Date(c("2021-03-01", "2021-08-01","2021-11-01")), each = 2)
)


# Add legend
res$Calculation <- factor(c(rep("Together", 6), rep(c("Infection of naïve subject", "Breakthrough infection"), 3)),
                          levels = c("Infection of naïve subject", "Breakthrough infection", "Together"))


###
# Plot
###

# Colors
col <- RColorBrewer::brewer.pal(3, "Set2")

g_bti <- ggplot(res, aes(x = date, y = estimate, group = Calculation)) +
  geom_vline(xintercept =  as.Date(c("2020-02-01", "2020-05-01", 
                                     "2020-12-01", 
                                     "2021-03-01", 
                                     "2021-08-01",
                                     "2021-11-01")),
             colour = "grey92") +
  geom_hline(yintercept =  seq(0,0.06,0.01),
             colour = "grey92") +
  geom_line(aes(linetype = Calculation), col = col[3], size = 1) + 
  geom_point(aes(shape = Calculation), col = col[3], size = 2, key_glyph = large_points) +
  scale_color_manual(values = col) + 
  theme_classic() + 
  scale_y_continuous(labels=scales::label_percent(accuracy = 1L), limits = c(0, 0.06), n.breaks = 4) +
  scale_x_date(labels = Labels,
               breaks = Date) +
  scale_linetype_manual(values=c("dashed", "dotted", "solid")) +
  geom_ribbon(aes(ymin = lb_ci, ymax = ub_ci, linetype = Calculation),
              alpha = 0.15, fill = col[3], col = "#8DA0CB70") +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11), 
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=9),
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "bottom") +
  guides(shape = guide_legend(override.aes = list(size=0.8))) +
  ylab("Incidence infection (%)") 


########################
# Plots on vaccination #
########################

# % of the population vaccinated

dat_vax <- read.csv(here_koco_data("R5/Vaccination.csv"))

colnames(dat_vax)[1] <- "date"

dat_vax <- dat_vax[dat_vax$date %in% c("30.3.", "30.4.", "31.5.", "30.6.", "30.7.", "31.8.",
                                       "30.9.", "31.10.", "30.11.") &
                     as.numeric(rownames(dat_vax)) < 183, ] # To keep only months in 2021


dat_vax$pop <- seq(1324232, 1325949, length.out = 9)

dat_vax$Value <- dat_vax$Erstimpfung/dat_vax$pop

incidence <- rep(NA, 9)
for(i in 2:9){
  incidence[i] <- dat_vax$Value[i] - dat_vax$Value[i-1]
}

dat_vax2 <- dat_vax

dat_vax2$Value <- incidence

dat_vax <- rbind(dat_vax, dat_vax2)

# Add dates
dat_vax$date = rep(seq(as.Date("2021-03-01"), as.Date("2021-11-01"), by = "1 month"), 2)


# Labels
Labels <- format(dat_vax$date, "%b %Y")
Labels[dat_vax$date %in% as.Date(c("2021-03-01", "2021-08-01", "2021-11-01"))] <- 
  paste(Labels[dat_vax$date %in% as.Date(c("2021-03-01", "2021-08-01", "2021-11-01"))], "\n", 
        c("Round 3", "Round 4", "Round 5"))
  
Labels <- Labels[1:(length(Labels)/2)]

dat_vax$Calculation <- factor(rep(c("Prevalence", "Incidence"), each = 9), levels = c("Prevalence", "Incidence"))


# Transform data for the plot
dat_vax$Value[dat_vax$Calculation == "Incidence"] <- dat_vax$Value[dat_vax$Calculation == "Incidence"] * 4

###
# Plot
###

g_vax <- ggplot(dat_vax, aes(x = date, y = Value, col = Calculation, shape = Calculation)) +
  geom_vline(xintercept =  dat_vax$date,
    colour = "grey92") +
  geom_hline(yintercept =  seq(0,0.8,0.1),
             colour = "grey92") +
  geom_line(aes(linetype = Calculation), col = col[2], size = 1) + 
  geom_point(aes(shape = Calculation), col = col[2], size = 2, key_glyph = large_points) +
  theme_classic() + 
  scale_y_continuous(name = "Prevalence vaccination (%)  ", labels = scales::percent_format(accuracy = 1), # trans = "reverse",
                                          sec.axis = sec_axis(~./4, name = "Incidence vaccination (%)", labels = scales::percent_format(accuracy = 1))) +
  scale_x_date(labels = Labels,
               breaks = dat_vax$date[dat_vax$Calculation == "Incidence"]) +
  theme(title = element_text(size = 15),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11), 
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=9, angle = 45, vjust = 1, hjust = 1),
        legend.text = element_text(size=10),
        legend.title = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position="bottom") +
  guides(shape = guide_legend(override.aes = list(size=0.8)))
  

########################
# Relative frequencies #
########################


###
# Load results
###

# Weighted
sp_w_r1 <- read.csv(here_prev_results("r1_sp_w.csv"))
sp_w_r2 <- read.csv(here_prev_results("r2_cum_sp_w.csv"))
sp_w_r3 <- read.csv(here_prev_results("r3_cum_sp_w.csv"))
sp_w_r4 <- read.csv(here_prev_results("r4_cum_sp_w.csv"))
sp_w_r5 <- read.csv(here_prev_results("r5_cum_sp_w.csv"))

# % of the population vaccinated

# R3
vax_r3 <- 149536 / 1324232
# R4 
vax_r4 <- 898430 / 1325336
# R5
vax_r5 <- 1012084 / 1325949


###
# Create table for the plot
###

res <- rbind(sp_w_r1[sp_w_r1$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100, 
             1 - sp_w_r1[sp_w_r1$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r2[sp_w_r2$Calculation == "All" & sp_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             1 - sp_w_r2[sp_w_r2$Calculation == "All" & sp_w_r2$Adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r3[sp_w_r3$calculation %in% c("Non_vax", "Vax") & sp_w_r3$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             1 - sp_w_r3[sp_w_r3$calculation %in% c("Non_vax", "Vax") & sp_w_r3$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r4[sp_w_r4$calculation %in% c("Non_vax", "Vax") & sp_w_r4$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             1 - sp_w_r4[sp_w_r4$calculation %in% c("Non_vax", "Vax") & sp_w_r4$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
             sp_w_r5[sp_w_r5$calculation %in% c("Non_vax", "Vax") & sp_w_r5$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100,
            1 - sp_w_r5[sp_w_r5$calculation %in% c("Non_vax", "Vax") & sp_w_r5$adjust == "adjusted", c("estimate", "lb_ci", "ub_ci")]/100
)


# Add dates
res$date = c(rep(as.Date(c("2020-05-01", "2020-12-01")), each = 2), rep(as.Date(c("2021-03-01", "2021-08-01","2021-11-01")), each = 4))

# Add legend
res$Infection <- factor(c(rep(c("Infected", "Not_infected"), times = 2), 
                          rep(c("Infected", "Not_infected"), each = 2, times = 3)))

res$Vaccination <- factor(c(rep("Not_vaccinated", 4), rep(c("Not_vaccinated", "Vaccinated"), times = 6)),
                          levels = c("Vaccinated", "Not_vaccinated"))

# Add rounds
res$Round <- c(rep(c("Round 1", "Round 2"), each = 2), rep(c("Round 3", "Round 4","Round 5"), each = 4))



# Adjust % to the size of the population
res[res$Vaccination == "Vaccinated", c("estimate", "lb_ci", "ub_ci")] <- 
  res[res$Vaccination == "Vaccinated", c("estimate", "lb_ci", "ub_ci")] *
  rep(c(vax_r3, vax_r4, vax_r5), each = 2)

res[res$Vaccination == "Not_vaccinated" & !(res$Round %in% c("Round 1", "Round 2")), c("estimate", "lb_ci", "ub_ci")] <- 
  res[res$Vaccination == "Not_vaccinated" & !(res$Round %in% c("Round 1", "Round 2")), c("estimate", "lb_ci", "ub_ci")] *
  rep(c(1-vax_r3, 1-vax_r4, 1-vax_r5),  each = 2)

res$group <- factor(paste(res$Vaccination, res$Infection, sep = "_"),
                    levels = c("Not_vaccinated_Infected", "Vaccinated_Infected",
                               "Not_vaccinated_Not_infected", "Vaccinated_Not_infected"))

res <- res[order(res$Round, res$group), ]


###
# Plot
###


Labels <- c("Mar 2021\nRound 3\n(n=4382)", "Aug 2021\nRound 4\n(n=3971)", "Nov 2021\nRound 5\n(n=3838)")


g <- ggplot(res[!(res$Round %in% c("Round 1", "Round 2")), ], aes(x = Round, y = estimate, fill = group, pattern = group)) +
  geom_bar(width = 0.8, position = position_dodge(width = 0.9), stat="identity") +
  scale_x_discrete(labels = Labels) +
  theme_minimal() +
                       scale_fill_manual(values = c("#8DA0CB", "#FC8D62", "#8DA0CB70", "#FC8D6270"), 
                                         labels = c("Not vaccinated & infected", "Vaccinated & infected",
                                                    "Not vaccinated & not infected", "Vaccinated & not infected"),
                                         guide = guide_legend(nrow = 2)) + 
  geom_errorbar(aes(ymin=lb_ci, ymax=ub_ci), width=.2,
                position=position_dodge(.9)) +
  theme(title = element_text(size = 15),
        axis.ticks = element_line(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 11),
        axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=9),
        legend.text = element_text(size=10),
        legend.title = element_blank(), 
        legend.key = element_blank(),
        legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  ylab("Relative frequency (%)") +
  guides(col = guide_legend(override.aes = list(size=0.8))) +
  geom_vline(xintercept = c(1.5, 2.5))

# Add gap and %
source(here_prev_scripts("my_gggap.R"))
g_prev_inf_vax <- my_gg.gap(plot=g,
          segments=c(0.3,0.55),
          ylim=c(0,0.85),
          tick_width = c(0.05, 0.1),
          rel_heights = c(0.25, 0, 0.1))


#########################
# Plots for publication #
#########################


p_ab <- plot_grid(g_prev_inf + theme(legend.position="none"), g_inc_inf + theme(legend.position="none"), labels = LETTERS[1:2])

legend_ab <- get_legend(g_prev_inf)

g_ab <- plot_grid(p_ab, legend_ab, ncol = 1, rel_heights = c(1.1, .15))

p_cd <- plot_grid(g_prev_vax + theme(legend.position="none"), g_bti + theme(legend.position="none"), labels = LETTERS[3:4])

legend_cd <- plot_grid(get_legend(g_prev_vax), get_legend(g_bti))

g_cd <- plot_grid(p_cd, legend_cd, ncol = 1, rel_heights = c(1.1, .15))


ggsave2(plot = g_prev_inf_vax, filename = here_prev_figures("plot_F.pdf"), width = 6, height = 3.5)


p_f <- cowplot::ggdraw() + cowplot::draw_image(magick::image_read_pdf(here_prev_figures("plot_F.pdf")), 
                                               clip = "on", scale = 1.25)

p_ef <- plot_grid(g_vax + theme(legend.position="none"), p_f,
                  labels = LETTERS[5:6])


l_ef <- plot_grid(get_legend(g_vax), get_legend(g))

g_ef <- plot_grid(p_ef, l_ef, ncol = 1, rel_heights = c(1.1, .15))


g_fin <- plot_grid(g_ab, g_cd, g_ef, ncol = 1)


ggsave(here_prev_figures("Prevalence_incidence_final.png"), plot = g_fin, width = 30, height = 25, units = "cm")

