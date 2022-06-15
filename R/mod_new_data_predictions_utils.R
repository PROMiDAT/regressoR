code.carga <- function (nombre.filas = T, ruta = NULL, separador = ";", sep.decimal = ",", 
          encabezado = T, incluir.NA = F) 
{
  res <- paste0("##### doccarga #####\n", "datos <- fread('", 
                ruta, "', sep = '", separador, "', dec = '", sep.decimal, 
                "', header = ", encabezado, ", stringsAsFactors = T, data.table = F, check.names = T)\n")
  if (nombre.filas) {
    res <- paste0(res, "row.names(datos) <- datos[[1]]\n")
    res <- paste0(res, "datos[[1]] <- NULL\n")
  }
  res <- paste0(res, "\n", code.NA(incluir.NA))
  return(res)
}

carga.datos.np <- function(nombre.filas = T, ruta = NULL, separador = ";",
                           sep.decimal = ",", encabezado = T) {
  if(!is.null(ruta)) {
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  if(nombre.filas) {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T, row.names = 1)
  } else {
    res <- read.table(
      ruta, header = encabezado, sep = separador, dec = sep.decimal,
      stringsAsFactors = T)
  }
  return(res)
}