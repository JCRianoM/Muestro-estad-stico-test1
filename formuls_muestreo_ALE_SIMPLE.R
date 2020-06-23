################################################
###########Muestreo aleatorio SIMPLE ###########
################################################

library(dplyr)
library(tidyverse)
library(reshape)
library(ggplot2)
library(TeachingSampling)
library(Rmpfr)

#####Carga de base de datos########
wine <- read_csv2("winequality-white.csv")

####prueba piloto simple####
set.seed(123)

p_size_simple <- function(df_s, error) {
    df_s <- apply(df_s, 2, as.numeric)
    N_pop <- nrow(df_s)
    pil_size <- round(error*N_pop, 0)
        piloto_size_N <- data.frame(N_pop, pil_size)
    print(piloto_size_N)
    
}

###prueba de la formula piloto simple###
p_size_simple(wine, 0.05)


### formula para tabla de estimadores aleatorio simple###
estm_pilot_simple <- function(df_data, error) {
    N_pop <- nrow(df_data) 
    pil_size <- round(error*N_pop, 0)
    ID_pl <- sample(1:N_pop, size=pil_size,replace=FALSE)
    pilot_num <- df_data[ID_pl, ]
    piloto_df <- apply(pilot_num, 2, as.numeric)
    piloto_df <- round(piloto_df, 3) 
    #aplicación de estimadores
        Media_var <- as.vector(apply(piloto_df, 2, mean))
        E.Error <- as.vector(Media_var*0.05)
        D.standard <- as.vector(apply(piloto_df, 2, sd))
        sd_med_var <-  as.vector(D.standard/Media_var)
    #Se construye df con los estimadores
    df_st <- data.frame(Media_var, D.standard, sd_med_var, E.Error)
    rownames(df_st) <- colnames(piloto_df)
    piloto_size_N <- data.frame(N_pop, pil_size)
    print(piloto_size_N)
    print(df_st) #impresion del data.frame
}

est_aleat_simple <- estm_pilot_simple(wine, 0.05)

###################################################
######función para ver (ÚNICAMENTE) con MPFR#######
###################################################
mpfr_zing <- function(df_revisar) {
    require(Rmpfr)
    matrix_float <- as.matrix(df_revisar)
    mpfr_x <- mpfr(matrix_float, precBits = 32)
    print(mpfr_x)
}
###################################################
########## Revisando valores flotantes ############
###################################################
mpfr_zing(est_aleat_simple)
###################################################
###################################################

error_lst_simple <- function(data_est) {
    mar_1 <- seq(0,data_est[1, 4]+0.01,by=0.25)
    mar_2 <- seq(0,data_est[2, 4]+0.01, by=0.001)
    mar_3 <- seq(0,data_est[3, 4], by=0.003)
    mar_4 <- seq(0,data_est[4, 4]+0.01, by=0.03)
    mar_5 <- seq(0,data_est[5, 4]+0.01, by=0.0005)
    mar_6 <- seq(0,data_est[6, 4]+0.001, by=0.08)
    mar_7 <- seq(0,data_est[7, 4], by=0.3)
    mar_8 <- seq(0,data_est[8, 4]+0.01, by=0.005)
    mar_9 <- seq(0,data_est[9, 4], by=0.6)
    mar_10 <- seq(0,data_est[10, 4], by=0.003)
    mar_11 <- seq(0,data_est[11, 4], by=0.2)
    mar_12 <- seq(0,data_est[12, 4], by=0.01)
    f_list <- list(mar_1, mar_2, mar_3, mar_4, mar_5, mar_6, 
                   mar_7, mar_8, mar_9, mar_10, mar_11, mar_12)
    n.obs <- sapply(f_list, length)
    seq.max <- seq_len(max(n.obs))
    meta <- data.frame(sapply(f_list, "[", i = seq.max))
    colnames(meta) <- rownames(data_est)
    print(meta)
}

e_list_simple <- error_lst_simple(est_aleat_simple)


#######################################################
######Función para población infinita e infinita#######
###HACER PARA CADA VARIABLE, SOLO SE DEBE CAMBIARL EL NÚMERO DE LA VARIABLE#####
################################################################################

simple_inf.finite <- function(N, data_error, data_var, num_v, conf) {
    require(reshape)
    require(ggplot2)
    nam_v <- names(data_error[num_v])
    Z <- qnorm((1-conf)/2, mean = 0, sd = 1)
    Error_v_s <- na.omit(data_error[, num_v])
    sd_d <- data_var[num_v, 3]
    P_infinite_s <- ((Z^2)*sd_d^2)/Error_v_s^2
    P_finite_s <- P_infinite_s/(1+(P_infinite_s/N))
    tabl_e_s <- data.frame(Error_v_s, P_infinite_s, P_finite_s)
    ###melting para la gráfica####
    dat.melt <- melt(tabl_e_s, id.vars = "Error_v_s")
    ###inserta una elemento tipo gráfica de los errores###
    graph <-ggplot(dat.melt, aes(Error_v_s, value, colour = variable)) + 
        geom_point(size = 3) + 
        labs(title = 
                 paste('Muestreo por variable', '\"',nam_v, '\"'),
             y = 'Tamaño de muestra', 
             x = 'Errores', 
             caption = "Método = muestro aleatorio simple") +
        geom_line(linetype = "dashed")
    graph1 <- graph + 
        theme(plot.caption = element_text(hjust = 1, face = "italic"), 
              legend.title = element_blank(), 
              legend.position="bottom", legend.box = "horizontal")
    print(graph1)
    print(tabl_e_s)
}


gtes <- simple_inf.finite(4898, e_list_simple, est_aleat_simple, 12, 0.95)

###### función gráfica sola ######
graph_inf.finite <- function(N, data_error, data_var, num_v, conf) {
    require(reshape)
    require(ggplot2)
    nam_v <- names(data_error[num_v])
    Z <- qnorm((1-conf)/2, mean = 0, sd = 1)
    Error_v_s <- na.omit(data_error[, num_v])
    sd_d <- data_var[num_v, 3]
    P_infinite_s <- ((Z^2)*sd_d^2)/Error_v_s^2
    P_finite_s <- P_infinite_s/(1+(P_infinite_s/N))
    tabl_e_s <- data.frame(Error_v_s, P_infinite_s, P_finite_s)
    ###melting para la gráfica####
    dat.melt <- melt(tabl_e_s, id.vars = "Error_v_s")
    ###inserta una elemento tipo gráfica de los errores###
    graph <-ggplot(dat.melt, aes(x = Error_v_s, y = value, colour = variable)) + 
        geom_point(size = 2) + 
        labs(title = 
                 paste('Muestreo por variable', '\"',nam_v, '\"'),
             y = 'Tamaño de muestra', 
             x = 'Errores', 
             caption = "Método = muestro aleatorio simple") +
        geom_line(linetype = "dashed") 
    graph1 <- graph + 
        theme(plot.caption = element_text(hjust = 1, face = "italic"), 
              legend.title = element_blank(), 
              legend.position="bottom", legend.box = "horizontal")
    print(graph1)
}

graph_inf.finite(4898, e_list_simple, est_aleat_simple, 1, 0.95)

###ESCOGER TAMAÑO DE LA POBLACIÓN PARA CADA VARIABLE###

sample_var = 1289 ###número que se quiera escoger (observador)



# Formula para muestreo FINAL por método aleatorio simple
sam_final_simple <- function(data_original, muestra_sel) {
    require(TeachingSampling)
    N_pop <- nrow(data_original)
    sample_s <- sample(1:N_pop, size=muestra_sel,replace=FALSE)
    muestra_f <- data_original[sample_s, ]
    print(paste('N_población = ', N_pop, ';',
                'Tamaño de muestra seleccionado del piloto =', muestra_sel, ';',
                'Dimensiones: [', 'Columnas =',ncol(muestra_f), 
                'filas = ', nrow(muestra_f), ']' ))
    muestra_f <- (apply(muestra_f, 2, as.numeric))
    print(data.frame(muestra_f))
    
}
### Prueba muestreo final aleatorio simple###
simple_v1 <- sam_final_simple(wine, 6)


##### parte final ####
est_final_simple <- function (data_sampling, n_var, N, muestra_sel) {
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
    col_noms <- c('Int. Inferior', 'Media muestra', 'Int. Superior', 
                  'Desviación std.','Varianza', 'Coeficiente var.',
                  'Int. Inferior total', 'Media Total', 
                  'Int. Superior Total', 'Varianza Total')
    results <- data.frame(Inf_int_s,med_s,
                          Sup_int_s, sd_s, var_var, Coef_var,
                          Inf_int_var, med_total, 
                          Sup_int_var, v_t)
    rownames(results) <- names(data_sampling[n_var])
    colnames(results) <- col_noms
    print(results)
}


e.var_final_simple <- function(df_sampling, n_var, muestra_sel) {
    N<- 4898
    df_sampling <- apply(df_sampling, 2, as.numeric)
    df_sampling <- round(df_sampling, 3) 
    #aplicación de estimadores
    med_s <- mean(df_sampling[,n_var])
    sd_s <- sd(df_sampling[,n_var])
    Inf_int_s <- med_s - 2 * (sd_s/sqrt(muestra_sel))
    Sup_int_s <- med_s + 2 * (sd_s/sqrt(muestra_sel))
    Coef_var <- (sd_s/med_s)
    var_var <- (sd_s^2/muestra_sel)*((N-muestra_sel)/N)
    med_total <- med_s*N
    v_t <- var_var*N^2
    Inf_int_var <- med_s*N-2*sqrt(v_t)
    Sup_int_var <- med_s*N+2*sqrt(v_t)
    #Se construye df con los estimadores
    col_noms <- c('Int. Inferior', 'Media muestra', 'Int. Superior', 
                  'Desviación std.','Varianza', 'Coeficiente var.',
                  'Int. Inferior total', 'Media Total', 
                  'Int. Superior Total', 'Varianza Total')
    results <- data.frame(Inf_int_s,med_s,
                          Sup_int_s, sd_s, var_var, Coef_var,
                          Inf_int_var, med_total, 
                          Sup_int_var, v_t)
    rownames(results) <- names(df_sampling)
    colnames(results) <- col_noms
    print(results)
}

e.var_final_simple(simple_v1, 1, 6)


x_1simple <- est_final_simple(simple_v1, 1, 4898, 6)
x_2simple <- est_final_simple(simple_v2, 2, 4898, 1289)
x_3simple <- est_final_simple(simple_v2, 3, 4898, 1000)
x_4simple <- est_final_simple(simple_v2, 4, 4898, 1000)
x_5simple <- est_final_simple(simple_v2, 5, 4898, 1000)
x_6simple <- est_final_simple(simple_v2, 6, 4898, 1000)
x_7simple <- est_final_simple(simple_v2, 7, 4898, 1000)
x_8simple <- est_final_simple(simple_v2, 8, 4898, 1000)
x_9simple <- est_final_simple(simple_v2, 9, 4898, 1000)
x_10simple <- est_final_simple(simple_v2, 10, 4898, 1000)
x_11simple <- est_final_simple(simple_v2, 11, 4898, 1000)
x_12simple <- est_final_simple(simple_v2, 12, 4898, 1000)


df_SIMPLE_final <- as.data.frame(rbind(x_1simple, x_2simple, x_3simple, 
                      x_4simple, x_5simple, x_6simple, 
                      x_7simple, x_8simple, x_9simple, 
                      x_10simple, x_11simple, x_12simple))

df_SIMPLE_final

mpfr_zing(df_SIMPLE_final)

###################################################
##################### FIN #########################
###################################################


### selección de n = para cada variable ####
Col_acept <- c('Error aceptado', 'Tamaño de muestra (n)')
Error_acp <- c(0.25, 0.015, 0.012, 0.03, 0.012, 0.08, 0.3, 0.005, 0.6, 0.006, 0.2, 0.01)
n_sample <- c(6,  1290, 1888, 1765, 2944, 342,  3, 1, 1, 2749, 152, 668)
df_ns_simples <- data.frame(Error_acp, n_sample)
colnames(df_ns_simples) <- Col_acept
rownames(df_ns_simples) <- names(wine)

