#####################################################
###########Muestreo aleatorio SISTEMÁTICO ###########
#####################################################

library(dplyr)
library(tidyverse)
library(reshape)
library(ggplot2)
library(TeachingSampling)
library(Rmpfr)

####Base de datos ORIGINAL####
wine <- read_csv2("winequality-white.csv")


####prueba piloto sistemática####
set.seed(123)

p_size_sys <- function(df, error) {
    N_pop <- nrow(df)
    pil_size <- round(error*N_pop, 0)
    Kp <- round(N_pop/pil_size, 0)
    print(data.frame(N_pop, pil_size, Kp))
}

####aplicación formula anterior####
p_size_sys(wine, 0.05)

####formular para estimadores de piloto sistemático######
estm_pilot_sys <- function(df, N_pop, kp, N_confianza) {
    require(TeachingSampling)
    s_system <- S.SY(N_pop,kp) ### se requiere paquete TeachingSampling para función S.SY
    df_samp <- df[s_system,]
    ### ajuste de la base a datos númericos###
    s_num <- apply(df_samp, 2, as.numeric)
    s_num <- round(s_num, 3) ### evaluar si es necesario redondear
    #Se aplican primero estimadores#
        Media <- as.vector(apply(s_num, 2, mean))
        Error <- as.vector(Media*0.05)
        Varianza <- as.vector(apply(s_num, 2, var))
    #Se construye df con los estimadores
    df_st <- data.frame(Media, Error, Varianza)
        rownames(df_st) <- colnames(s_num)
    Z <- qnorm((1-N_confianza)/2, mean = 0, sd = 1) # valor Z
    #Se crean dos nuevas columnas con los tamaños por variable con y sin N
    Size_out_N <- round((Z^2*df_st$Varianza)/df_st$Error^2, 0)
    Size_with_N <- round(Size_out_N /(1+(Size_out_N/N_pop)),0)
    df_st <- cbind(df_st, Size_out_N, Size_with_N)
    print(df_st) #impresion del data.frame
}

estm_pilot_sys(wine, 4898, 10, 0.95)


tab_est <- estm_pilot_sys(wine, 4898, 10, 0.95)


#### Función errores ####
error_df_var <- function(data_est) {
    mar_1 <- seq(0,data_est[1, 2]+0.8,by=0.08)
    mar_2 <- seq(0,data_est[2, 2]+0.01, by=0.0006)
    mar_3 <- seq(0,data_est[3, 2], by=0.0004)
    mar_4 <- seq(0,data_est[4, 2]+0.1, by=0.01)
    mar_5 <- seq(0,data_est[5, 2]+0.001, by=0.00008)
    mar_6 <- seq(0,data_est[6, 2], by=0.04)
    mar_7 <- seq(0,data_est[7, 2]+0.9, by=0.2)
    mar_8 <- seq(0,data_est[8, 2]+0.01, by=0.0015)
    mar_9 <- seq(0,data_est[9, 2], by=0.3)
    mar_10 <- seq(0,data_est[10, 2], by=0.0006)
    mar_11 <- seq(0,data_est[11, 2], by= 1000000000)
    mar_12 <- seq(0,data_est[12, 2], by=0.0065)
    f_list <- list(mar_1, mar_2, mar_3, mar_4, mar_5, mar_6, 
                   mar_7, mar_8, mar_9, mar_10, mar_11, mar_12)
    n.obs <- sapply(f_list, length)
    seq.max <- seq_len(max(n.obs))
    meta <- data.frame(sapply(f_list, "[", i = seq.max))
    colnames(meta) <- rownames(data_est)
    print(meta)
}

error_df_var(tab_est)

e_list <- error_df_var(tab_est)

#######################################################
######Función para población infinita e infinita#######
###HACER PARA CADA VARIABLE, SOLO SE DEBE CAMBIARL EL NÚMERO DE LA VARIABLE#####
################################################################################

pob_inf.finite <- function(N, data_error, data_est, num_v, conf) {
    require(reshape)
    require(ggplot2)
    nam_v <- names(data_error[num_v])
    Z <- qnorm((1-conf)/2, mean = 0, sd = 1)
    Error_v <- na.omit(data_error[, num_v])
    var_d <- data_est[num_v, 3]
    P_infinite <- ((Z^2)*var_d)/Error_v^2
    P_finite <- P_infinite/(1+(P_infinite/N))
    tabl_e_s <- data.frame(Error_v, P_infinite, P_finite)
    ###melting para la gráfica####
        dat.melt <- melt(tabl_e_s, id.vars = "Error_v")
    ###inserta una elemento tipo gráfica de los errores###
    graph <-ggplot(dat.melt, aes(Error_v, value, colour = variable)) + 
            geom_point(size = 3) + 
            labs(title = 
                     paste('Muestreo por variable', '\"',nam_v, '\"'),
                 y = 'Tamaño de muestra', 
                 x = 'Errores',
                 caption = "Método = muestro sistemático") +
            geom_line(linetype = "dashed") 
    graph1 <- graph + 
        theme(plot.caption = element_text(hjust = 1, face = "italic"), 
              legend.title = element_blank(), 
              legend.position="bottom", legend.box = "horizontal")
    print(graph1)
    print(tabl_e_s)
}


#### Gráfica sola sistemático ####
Gr.sys_inf.finite <- function(N, data_error, data_est, num_v, conf) {
    require(reshape)
    require(ggplot2)
    nam_v <- names(data_error[num_v])
    Z <- qnorm((1-conf)/2, mean = 0, sd = 1)
    Error_v <- na.omit(data_error[, num_v])
    var_d <- data_est[num_v, 3]
    P_infinite <- ((Z^2)*var_d)/Error_v^2
    P_finite <- P_infinite/(1+(P_infinite/N))
    tabl_e_s <- data.frame(Error_v, P_infinite, P_finite)
    ###melting para la gráfica####
    dat.melt <- melt(tabl_e_s, id.vars = "Error_v")
    ###inserta una elemento tipo gráfica de los errores###
    graph <-ggplot(dat.melt, aes(Error_v, value, colour = variable)) + 
        geom_point(size = 3) + 
        labs(title = 
                 paste('Muestreo por variable', '\"',nam_v, '\"'),
             y = 'Tamaño de muestra', 
             x = 'Errores',
             caption = "Método = muestro sistemático") +
        geom_line(linetype = "dashed") 
    graph1 <- graph + 
        theme(plot.caption = element_text(hjust = 1, face = "italic"), 
              legend.title = element_blank(), 
              legend.position="bottom", legend.box = "horizontal")
    print(graph1)
    }


###prueba de la función gráfica y tabla###
pob_inf.finite(4898, e_list, tab_est, 5, 0.95)


###ESCOGER TAMAÑO DE LA POBLACIÓN PARA CADA VARIABLE###

sample_var = 448 ###número que se quiera escoger (observador)


#Ahora, determinamos los Indices de los datos con 
#los que trabajaremos la muestra de la primera variable
# Formula para muestreo FINAL
samp_final <- function(data_original, muestra_sel) {
    require(TeachingSampling)
    N_pop <- nrow(data_original)
    k_total <- round(N_pop/muestra_sel)
    sample_1 <- S.SY(N_pop, k_total)
    data_fin <- data_original[sample_1,]
    data_fin <- (apply(data_fin, 2, as.numeric))
    print(data.frame(data_fin))
    
}

### Prueba muestreo final###
sampling_v10 <- samp_final(wine, 448)


##### parte final ####
est_final_sys <- function (data_sampling, n_var, N, muestra_sel) {
    k_total <- round(N/muestra_sel)
    med_s <- mean(data_sampling[,n_var])
    sd_s <- sd(data_sampling[,n_var])
    Inf_int_s <- med_s - 2 * (sd_s/sqrt(muestra_sel))
    Sup_int_s <- med_s + 2 * (sd_s/sqrt(muestra_sel))
    Coef_var <- (sd_s/med_s)
    var_var <- (sd_s^2/muestra_sel)*((N-muestra_sel)/N)
    med_total <- med_s*N
    v_t <- var_var*N^2
    Inf_int_var <- med_s*N-2*sqrt(v_t)
    Sup_int_var <- med_s*N+2*sqrt(v_t)
    col_noms <- c('n', 'kp', 'Int. Inferior', 'Media muestra', 'Int. Superior', 
                  'Desviación std.','Varianza', 'Coeficiente var.',
                  'Int. Inferior total', 'Media Total', 
                  'Int. Superior Total', 'Varianza Total')
    results <- data.frame(muestra_sel, k_total, Inf_int_s,med_s,
               Sup_int_s, sd_s, var_var, Coef_var,
                Inf_int_var, med_total, 
               Sup_int_var, v_t)
    rownames(results) <- names(data_sampling[n_var])
    colnames(results) <- col_noms
    print(results)
}

est_final_sys(sampling_v10, 5, 4898, 448)

x_1 <- est_final_sys(sampling_v10, 1, 4898, 320)
x_2 <- est_final_sys(sampling_v10, 2, 4898, 320)
x_3 <- est_final_sys(sampling_v10, 3, 4898, 320)
x_4 <- est_final_sys(sampling_v10, 4, 4898, 320)
x_5 <- est_final_sys(sampling_v10, 5, 4898, 320)
x_6 <- est_final_sys(sampling_v10, 6, 4898, 320)
x_7 <- est_final_sys(sampling_v10, 7, 4898, 320)
x_8 <- est_final_sys(sampling_v10, 8, 4898, 320)
x_9 <- est_final_sys(sampling_v10, 9, 4898, 320)
x_10 <- est_final_sys(sampling_v10, 10, 4898, 320)
x_11 <- est_final_sys(sampling_v10, 11, 4898, 320)
x_12 <- est_final_sys(sampling_v10, 12, 4898, 320)

df_est_final <- rbind(x_1, x_2, x_3, 
                      x_4, x_5, x_6, 
                      x_7, x_8, x_9, 
                      x_10, x_11, x_12)



######función para ver (ÚNICAMENTE) con MPFR#######
mpfr_zing <- function(df_revisar) {
    require(Rmpfr)
    matrix_float <- as.matrix(df_revisar)
    mpfr_x <- mpfr(matrix_float, precBits = 32)
    print(mpfr_x)
}
###################################################
mpfr_zing (df_est_final)

###################################################
##################### FIN #########################
###################################################


Col_acept_sys <- c('Error aceptado', 'Tamaño de muestra (n)')
Error_acp_sys <- c(0.72, 0.003, 0.0032, 0.17, 0.00048, 1.16, 6, 0.0015, 2.7, 0.003, 2e+10, 0.026)
n_sample_sys <- c(2177,  2444, 2457, 2502, 2012, 2501,  2470, 26, 2570, 2591, 4838, 2136)
df_ns_sys <- data.frame(Error_acp_sys, n_sample_sys)
colnames(df_ns_sys) <- Col_acept_sys
rownames(df_ns_sys) <- names(wine)


