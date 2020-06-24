################################################
###########muestreo aleatorio simple############
###########estimadores finales #################
################################################

est_total_simple<- function(N, n, df){
    df <- apply(df, 2, as.numeric)
    df <- as.data.frame(df)
    table <- matrix(NA, nrow = 11, ncol = (dim(df)[2]))
    rownames(table) = c('Int. Inferior', 'Media muestra', 'Int. Superior', 
                        'DesviaciÃ³n std.','Varianza', 'Coeficiente var.',
                        'Int. Inferior total', 'Media Total', 
                        'Int. Superior Total', 'Varianza Total')
    colnames(table) <- names(df)
    for (k in 1:dim(df)[2]) {
        med_s <- mean(df[,k])
        sd_s <- sd(df[,k])
        Inf_int_s <- med_s - 2 * (sd_s/sqrt(n))
        Sup_int_s <- med_s + 2 * (sd_s/sqrt(n))
        Coef_var <- (sd_s/med_s)
        var_var <- (sd_s^2/n)*((N-n)/N)
        med_total <- med_s*N
        v_t <- var_var*N^2
        Inf_int_var <- med_s*N-2*sqrt(v_t)
        Sup_int_var <- med_s*N+2*sqrt(v_t)
        col_noms <- c('n', 'Int.Inferior', 'Media muestra', 'Int.Superior', 
                      'Desviación std.','Varianza', 'Coeficiente var.',
                      'Int.Inferior total', 'Media Total', 
                      'Int.Superior Total', 'Varianza Total')
        table[, k] <- c(n, Inf_int_s, med_s, Sup_int_s, sd_s, var_var,
                        Coef_var, Inf_int_var, med_total, Sup_int_var,v_t)
    }
    print(data.frame(t(table)))
}

########prueba ale. simple #########
est_total_simple(4898, 254, wine)

#la base de datos que se obtiene de haber hecho el muetreo final (df)
#254 el n elegido de la piloto para alguna de las variables
#4898 el tamaño total de la población

################################################
###########muestreo sistemático#################
###########estimadores finales #################
################################################

est_total_sys<- function(N, n, df){
    kp <- round(N/n)
    df <- apply(df, 2, as.numeric)
    df <- as.data.frame(df)
    table <- matrix(NA, nrow = 12, ncol = (dim(df)[2]))
    rownames(table) = c('n', 'kp', 'Int. Inferior', 'Media muestra', 'Int. Superior', 
                        'DesviaciÃ³n std.','Varianza', 'Coeficiente var.',
                        'Int. Inferior total', 'Media Total', 
                        'Int. Superior Total', 'Varianza Total')
    colnames(table) <- names(df)
    for (k in 1:dim(df)[2]) {
        med_s <- mean(df[,k])
        sd_s <- sd(df[,k])
        Inf_int_s <- med_s - 2 * (sd_s/sqrt(n))
        Sup_int_s <- med_s + 2 * (sd_s/sqrt(n))
        Coef_var <- (sd_s/med_s)
        var_var <- (sd_s^2/n)*((N-n)/N)
        med_total <- med_s*N
        v_t <- var_var*N^2
        Inf_int_var <- med_s*N-2*sqrt(v_t)
        Sup_int_var <- med_s*N+2*sqrt(v_t)
        col_noms <- c('Int.Inferior', 'Media muestra', 'Int.Superior', 
                      'Desviación std.','Varianza', 'Coeficiente var.',
                      'Int.Inferior total', 'Media Total', 
                      'Int.Superior Total', 'Varianza Total')
        table[, k] <- c(n, kp, Inf_int_s, med_s, Sup_int_s, sd_s, var_var,
                        Coef_var, Inf_int_var, med_total, Sup_int_var,v_t)
    }
    print(data.frame(t(table)))
}


########prueba sys#########
est_total_simple(4898, 254, wine)

##la base de datos que se obtiene de haber hecho el muetreo final (df)
#254 el n elegido de la piloto para alguna de las variables (n)
#4898 el tamaño total de la población (N)

