
#################################################
########### muestreo sistemático ################
########### estimadores finales #################
#################################################
library (TeachingSampling)
library (readr)

####Base de datos ORIGINAL#### (como la tengan en el archivo de origen)
wine <- read_csv2("winequality-white.csv") ## ac� la llame "wine"
############################################################

### esta versi�n solo �tiliza dos argumentos (n y df). n = es el tamaño de muestra seleccionado, 
### df = a la base de datos original. 
est_total_sys2<- function(n, df){
    require(TeachingSampling) # requiere este paquete y tener esta librería cargada
    N <- nrow(df)
    kp <- round(N/n)
    sample_1 <- S.SY(N, kp) # se define muestreon a través de función S.SY
    df_f <- df[sample_1,] # se realiza muestreo sobre la base de datos original. 
    df_f <- apply(df_f, 2, as.numeric) # se hacen numericas todas las columnas
    df_f <- as.data.frame(df_f) # se pasa a data frame en caso que no lo sea
    table <- matrix(NA, nrow = 12, ncol = (dim(df_f)[2])) 
    rownames(table) = c('n', 'kp', 'Int. Inferior', 'Media muestra', 'Int. Superior', 
                        'Desviacion std.','Varianza', 'Coeficiente var.',
                        'Int. Inferior total', 'Media Total', 
                        'Int. Superior Total', 'Varianza Total')
    colnames(table) <- names(df)
    for (k in 1:dim(df_f)[2]) {
        med_s <- mean(df_f[,k]) # se obtiene la media para todas las variables
        sd_s <- sd(df_f[,k]) # se obtiene la desviación para todas las variables
        Inf_int_s <- med_s - 2 * (sd_s/sqrt(n)) # se obtiene el intevalo inf de media para todas las variables
        Sup_int_s <- med_s + 2 * (sd_s/sqrt(n)) # se obtiene el intevalo sup de media para todas las variables
        Coef_var <- (sd_s/med_s) # se obtiene coef de variación para todas las variables
        var_var <- (sd_s^2/n)*((N-n)/N) # se obtiene la varianza para todas las variables
        med_total <- med_s*N # se obtiene la media total para todas las variables
        v_t <- var_var*N^2 # se obtiene la varianza total para todas las variables
        Inf_int_var <- med_s*N-2*sqrt(v_t) # se obtiene el intevalo inf media total para todas las variables
        Sup_int_var <- med_s*N+2*sqrt(v_t) # se obtiene el intevalo sup media total para todas las variables
        col_noms <- c('n', 'kp', 'Int.Inferior', 'Media muestra', 'Int.Superior', 
                      'Desviacion std.','Varianza', 'Coeficiente var.',
                      'Int.Inferior total', 'Media Total', 
                      'Int. Superior Total', 'Varianza Total')
        table[, k] <- c(n, kp, Inf_int_s, med_s, Sup_int_s, sd_s, var_var,
                        Coef_var, Inf_int_var, med_total, Sup_int_var,v_t)
    }
    print(data.frame(t(table)))
}

###### test ########

est_total_sys2(254, wine)


#################################################
########### muestreo Ale. SIMPLE ################
########### estimadores finales #################
#################################################

### esta versión solo útiliza dos argumentos (n y df). n = es el tamaño de muestra seleccionado, 
### df = a la base de datos original. 

est_total_simple2<- function(n, df){
    require(TeachingSampling) # requiere este paquete y tener esta librería cargada
    N <- nrow(df)
    sample_1 <- sample(1:N, size=n,replace=FALSE) # se define muestreo a través de función sample
    df_f <- df[sample_1,] # se realiza muestreo sobre la base de datos original. 
    df_f <- apply(df_f, 2, as.numeric) # se hacen numericas todas las columnas
    df_f <- as.data.frame(df_f) # se pasa a data frame en caso que no lo sea
    table <- matrix(NA, nrow = 11, ncol = (dim(df_f)[2])) 
    rownames(table) = c('n', 'Int. Inferior', 'Media muestra', 'Int. Superior', 
                        'Desviacion std.','Varianza', 'Coeficiente var.',
                        'Int. Inferior total', 'Media Total', 
                        'Int. Superior Total', 'Varianza Total')
    colnames(table) <- names(df)
    for (k in 1:dim(df_f)[2]) {
        med_s <- mean(df_f[,k]) # se obtiene la media para todas las variables
        sd_s <- sd(df_f[,k]) # se obtiene la desviación para todas las variables
        Inf_int_s <- med_s - 2 * (sd_s/sqrt(n)) # se obtiene el intevalo inf de media para todas las variables
        Sup_int_s <- med_s + 2 * (sd_s/sqrt(n)) # se obtiene el intevalo sup de media para todas las variables
        Coef_var <- (sd_s/med_s) # se obtiene coef de variación para todas las variables
        var_var <- (sd_s^2/n)*((N-n)/N) # se obtiene la varianza para todas las variables
        med_total <- med_s*N # se obtiene la media total para todas las variables
        v_t <- var_var*N^2 # se obtiene la varianza total para todas las variables
        Inf_int_var <- med_s*N-2*sqrt(v_t) # se obtiene el intevalo inf media total para todas las variables
        Sup_int_var <- med_s*N+2*sqrt(v_t) # se obtiene el intevalo sup media total para todas las variables
        col_noms <- c('Int.Inferior', 'Media muestra', 'Int.Superior', 
                      'Desviacion std.','Varianza', 'Coeficiente var.',
                      'Int.Inferior total', 'Media Total', 
                      'Int. Superior Total', 'Varianza Total')
        table[, k] <- c(n, Inf_int_s, med_s, Sup_int_s, sd_s, var_var,
                        Coef_var, Inf_int_var, med_total, Sup_int_var,v_t)
    }
    print(data.frame(t(table)))
}

###### test SIMPLE ########
est_total_simple2(254, wine)
