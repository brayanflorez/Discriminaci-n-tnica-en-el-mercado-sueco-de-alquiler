###############################################################################
# PUNTO 3
# Autor: David Florez y Daniel 
# 
#   - Poder total
#   - Distribuciones de t (H0 vs H1)
#   - MDE (Minimum Detectable Effect)
#   - Sesgo y cobertura
#   - Configuración única de parámetros base
###############################################################################

library(tidyverse)
library(ggplot2)

setwd("C:/Users/braya/OneDrive - Universidad de los Andes/Escritorio/U/8vo semestre/Economía Urbana/Taller 2")

###############################################################################
# 1. PARÁMETROS BASE (ÚNICA DEFINICIÓN)
###############################################################################

p_Erik     <- 0.46
p_Maria    <- 0.564
p_Mohammed <- 0.212

# Efecto real utilizado en varias secciones
tau_true <- p_Mohammed - p_Erik


###############################################################################
# 2. FUNCIÓN GENERAL DE GENERAR PROBABILIDADES SEGÚN PERFIL + RUIDO
###############################################################################

generate_prob <- function(Z, eps){
  p <- case_when(
    Z == "Erik"      ~ p_Erik     + eps,
    Z == "Maria"     ~ p_Maria    + eps,
    Z == "Mohammed"  ~ p_Mohammed + eps
  )
  pmin(pmax(p, 0.001), 0.999)
}


###############################################################################
# 3. PODER – EFECTO PRINCIPAL P(Mohammed) vs P(Erik)
###############################################################################

sim_one_experiment <- function(N, sd_eps){
  Z  <- sample(c("Erik","Maria","Mohammed"), N, replace = TRUE)
  eps <- rnorm(N, 0, sd_eps)
  
  Y <- rbinom(N, 1, generate_prob(Z, eps))
  
  fit <- lm(Y ~ Z)
  summary(fit)$coef["ZMohammed","Pr(>|t|)"]
}

power_for <- function(N, sd_eps, sims = 800){
  mean(replicate(sims, sim_one_experiment(N, sd_eps)) < 0.05)
}

# GRID
Ns      <- seq(100, 1000, by = 100)
sd_vec  <- c(0, 0.02, 0.05, 0.1, 0.2, 0.3, 0.5)

power_results <- expand_grid(N = Ns, sd_eps = sd_vec) %>%
  mutate(power = map2_dbl(N, sd_eps, ~ power_for(.x, .y)))


# GRÁFICO
g_poder <- ggplot(power_results, aes(N, power)) +
  geom_line(linewidth = 1, color = "#003f5c") +
  geom_point(size = 1.8, color = "#003f5c") +
  facet_wrap(~ sd_eps, ncol = 3, labeller = labeller(sd_eps = function(x) paste("SD error:", x))) +
  labs(x = "Tamaño muestral", y = "Poder") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"))

ggsave("Results/grafico_poder.pdf", g_poder, width = 8, height = 6, device = cairo_pdf)


###############################################################################
# 4. DISTRIBUCIÓN EMPÍRICA DEL t-STAT BAJO H0 Y H1
###############################################################################

sim_tstat <- function(N, sd_eps, pMoh_override = NULL){
  
  Z <- sample(c("Erik","Mohammed"), N, replace = TRUE)
  eps <- rnorm(N, 0, sd_eps)
  
  pMoh <- if(is.null(pMoh_override)) p_Mohammed else pMoh_override
  
  p <- case_when(
    Z == "Erik"      ~ p_Erik     + eps,
    Z == "Mohammed"  ~ pMoh       + eps
  )
  
  p <- pmin(pmax(p, 0.001), 0.999)
  
  Y <- rbinom(N, 1, p)
  fit <- lm(Y ~ Z)
  
  summary(fit)$coef["ZMohammed","t value"]
}

# Parámetros
N  <- 500
sd_eps <- 0.1
S <- 3000

# H0: no diferencia
t_H0 <- replicate(S, sim_tstat(N, sd_eps, pMoh_override = p_Erik))

# H1: efecto real
t_H1 <- replicate(S, sim_tstat(N, sd_eps))

crit <- qt(0.025, df = N - 2)

df_plot <- tibble(
  t = c(t_H0, t_H1),
  group = rep(c("H0","H1"), each = S)
)

g_emp <- ggplot(df_plot, aes(t, fill = group, color = group)) +
  
  geom_density(alpha = 0.25, linewidth = 1.1) +
  
  geom_area(
    data = df_plot %>% filter(group=="H0", t < crit),
    aes(y = ..density..),
    stat = "density",
    fill = "#d73027",
    alpha = 0.35
  ) +
  geom_area(
    data = df_plot %>% filter(group=="H1", t >= crit),
    aes(y = ..density..),
    stat = "density",
    fill = "#fdae61",
    alpha = 0.35
  ) +
  
  geom_vline(xintercept = crit, linetype = "dashed") +
  scale_fill_manual(values = c("H0"="#4575b4","H1"="#1a9850")) +
  scale_color_manual(values = c("H0"="#4575b4","H1"="#1a9850")) +
  labs(x = "Estadístico t", y = "Densidad") +
  theme_minimal(base_size = 14) +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = "none")

ggsave("Results/grafico_distribuciones_t.pdf", g_emp, width = 9, height = 5.5, device = cairo_pdf)


###############################################################################
# 5. MDE – MINIMUM DETECTABLE EFFECT
###############################################################################

power_for_delta <- function(N, delta, sd_eps, sims = 600){
  
  pMoh_new <- max(p_Erik - delta, 0.001)
  
  pvals <- replicate(sims, {
    Z <- sample(c("Erik","Maria","Mohammed"), N, replace = TRUE)
    eps <- rnorm(N, 0, sd_eps)
    
    p <- case_when(
      Z == "Erik"      ~ p_Erik     + eps,
      Z == "Maria"     ~ p_Maria    + eps,
      Z == "Mohammed"  ~ pMoh_new   + eps
    )
    
    Y <- rbinom(N, 1, pmin(pmax(p,0.001),0.999))
    fit <- lm(Y ~ Z)
    summary(fit)$coef["ZMohammed","Pr(>|t|)"]
  })
  
  mean(pvals < 0.05)
}

find_MDE <- function(N, sd_eps = 0.1){
  deltas <- seq(0.01, 0.30, by = 0.01)
  powers <- map_dbl(deltas, ~ power_for_delta(N, .x, sd_eps))
  
  tibble(delta = deltas, power = powers) %>%
    filter(power >= 0.8) %>%
    slice_head(n=1) %>%
    mutate(N = N)
}

MDE_results <- map_dfr(Ns, find_MDE)

g_MDE <- ggplot(MDE_results, aes(N, delta)) +
  geom_line(color="#003f5c", linewidth=1) +
  geom_point(size=2, color="#003f5c") +
  labs(x="Tamaño muestral", y="MDE (mínima diferencia detectable)") +
  theme_minimal(base_size=14) +
  theme(text = element_text(family = "Times New Roman"))

ggsave("Results/grafico_MDE.pdf", g_MDE, width=8, height=4, device=cairo_pdf)


###############################################################################
# 6. SESGO Y COBERTURA
###############################################################################


sim_one_bias_cov <- function(N, sd_eps){
  
  Z <- sample(c("Erik","Maria","Mohammed"), N, replace = TRUE)
  eps <- rnorm(N, 0, sd_eps)
  
  p <- case_when(
    Z == "Erik"      ~ p_Erik     + eps,
    Z == "Maria"     ~ p_Maria    + eps,
    Z == "Mohammed"  ~ p_Mohammed + eps
  )
  
  p <- pmin(pmax(p, 0.001), 0.999)
  
  Y <- rbinom(N, 1, p)
  
  fit <- lm(Y ~ Z)
  est <- coef(fit)["ZMohammed"]
  se  <- summary(fit)$coef["ZMohammed","Std. Error"]
  
  CI_low  <- est - 1.96 * se
  CI_high <- est + 1.96 * se
  
  tibble(
    est = est,
    bias = est - tau_true,
    covers = (tau_true >= CI_low & tau_true <= CI_high)
  )
}

bias_cov_for <- function(N, sd_eps, sims = 1000){
  sims_out <- map_dfr(1:sims, ~ sim_one_bias_cov(N, sd_eps))
  
  sims_out %>%
    summarise(
      mean_est = mean(est),
      mean_bias = mean(bias),
      sd_est = sd(est),
      RMSE = sqrt(mean(bias^2)),
      coverage = mean(covers)
    ) %>%
    mutate(N = N, sd_eps = sd_eps)
}

Ns <- seq(100, 1000, by = 100)
sd_vec <- c(0, 0.05, 0.1, 0.2, 0.3)

grid <- expand_grid(N = Ns, sd_eps = sd_vec)

bias_cov_results <- map2_dfr(
  grid$N, grid$sd_eps,
  ~ bias_cov_for(.x, .y, sims = 800)
)


# Paleta sobria tipo paper (Okabe–Ito)
colores <- c(
  "0"    = "#000000",  # negro
  "0.05" = "#3B6280",  # azul oscuro
  "0.1"  = "#4C8A67",  # verde grisáceo
  "0.2"  = "#A0522D",  # terracota
  "0.3"  = "#7E7394"   # púrpura apagado
)


g_bias <- ggplot(bias_cov_results, 
                 aes(x = N, y = mean_bias, color = factor(sd_eps))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = colores) +
  labs(
    x = "Tamaño muestral",
    y = "Sesgo promedio del estimador",
    color = "SD error"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "right"
  )
g_bias


g_cov <- ggplot(bias_cov_results, 
                aes(x = N, y = coverage, color = factor(sd_eps))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  scale_color_manual(values = colores) +
  labs(
    x = "Tamaño muestral",
    y = "Cobertura del 95%",
    color = "SD error"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman"),
    legend.position = "right"
  )
g_cov


###############################################################################
# 7. Efectos heterogeneos
###############################################################################

### Probabilidades y coeficientes del paper
beta0       <- -0.213
beta_Moh    <- -0.075
beta_Imm    <-  0.101
# gamma se pasa como argumento

### Para capturar correctamente el coeficiente de interacción
extract_interaction_pvalue <- function(fit){
  s  <- summary(fit)$coef
  rn <- rownames(s)
  idx <- grep("Mohammed.*L|L.*Mohammed", rn)
  if (length(idx) == 0) return(NA)
  s[idx, "Pr(>|t|)"]
}

### Simular una réplica
sim_hetero_once <- function(N, sd_eps, p_L, gamma){
  
  Z <- sample(c("Erik","Mohammed"), N, replace = TRUE)
  L <- rbinom(N, 1, p_L)
  
  eps <- rnorm(N, 0, sd_eps)
  
  p <- beta0 +
    beta_Moh * (Z == "Mohammed") +
    beta_Imm * L +
    gamma * (Z == "Mohammed" & L == 1) +
    eps
  
  p <- pmin(pmax(p, 0.001), 0.999)
  Y <- rbinom(N, 1, p)
  
  fit <- lm(Y ~ Z * L)
  extract_interaction_pvalue(fit)
}

### Poder
power_hetero <- function(N, sd_eps, p_L, gamma, sims = 500){
  pvals <- replicate(sims, sim_hetero_once(N, sd_eps, p_L, gamma))
  mean(pvals < 0.05, na.rm = TRUE)
}


scenarios <- tribble(
  ~scenario, ~p_L, ~gamma,
  "A. 10% inmigrantes, gamma=-0.041", 0.10, -0.041,
  "B. 40% inmigrantes, gamma=-0.041", 0.40, -0.041,
  "C. 10% inmigrantes, gamma=-0.10",  0.10, -0.10,
  "D. 40% inmigrantes, gamma=-0.10",  0.40, -0.10
)

Ns     <- seq(100, 1600, by = 200)
d_vec  <- c(0, 0.05, 0.1, 0.2, 0.3)

# expandimos: escenarios × N × sd_eps
param_grid <- scenarios %>%
  crossing(N = Ns, sd_eps = d_vec)

results <- param_grid %>%
  mutate(
    power = pmap_dbl(
      list(N, sd_eps, p_L, gamma),
      ~ power_hetero(..1, ..2, ..3, ..4, sims = 400)
    )
  )

g_hetero_all <- ggplot(results, aes(N, power, color = factor(sd_eps))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0.80, linetype = "dashed") +
  scale_color_manual(values = colores) +
  facet_wrap(~ scenario, ncol = 2) +
  labs(
    x = "Tamaño muestral",
    y = "Poder",
    color = "SD error",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Times New Roman")
  )

g_hetero_all

ggsave(
  "Results/grafico_hetero.pdf",
  device = cairo_pdf,
  width = 8,
  height = 6
)




















