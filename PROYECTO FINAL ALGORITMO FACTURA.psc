Algoritmo distribuidora_KK_corregido
    definir p, opcion, es_f, total, num_productos, num_factura_actual como entero
    definir total_ventas_diarias como real
    definir continuar, producto_facturar como caracter
    
    // Inicializar variables
    num_productos <- 0
    num_factura_actual <- 1 // Inicializa el número de factura a 1
    total_ventas_diarias <- 0
    
    Dimension productos[100]
    Dimension inventario[100]
    Dimension precio[100]
    Dimension totales_facturas[20] // Almacena los totales de hasta 20 facturas por día
    
    Repetir
        Escribir "=============================================================="
        Escribir "|                    Distribuidora K&K                       |"
        Escribir "|         TODO LO QUE NECESITA EN EQUIPOS ELECTRONICOS       |"
        Escribir "=============================================================="
        Escribir "|","           ",    "    [1]. Registro de datos                ","       |"
        Escribir "|","           ",    "    [2]. Consulta de datos                ","       |"
        Escribir "|","           ",    "    [3]. Generar factura                  ","       |"
        Escribir "|","           ",    "    [4]. Informe de ventas                ","       |"
        Escribir "|","           ",    "    [5]. Salir                            ","       |"
        Escribir "=============================================================="
        Escribir "Para poder consultar datos y generar facturas, obligatoriamente debe ingresar datos"
        Escribir ""
        Escribir Sin Saltar "Seleccione una opción: "
        Leer opcion
        
        Segun opcion Hacer
            Caso 1:
                // Registro de datos
                Escribir Sin Saltar "Ingrese el número de productos a registrar: "
                Leer p
                
                Para i <- 1 Hasta p Con Paso 1 Hacer
                    Escribir Sin Saltar "Ingrese el nombre del producto ", num_productos + 1, ": "
                    Leer productos[num_productos + 1]
                    
                    // Verificar que el producto no esté repetido
                    Para h <- 1 Hasta num_productos Con Paso 1 Hacer
                        Si productos[num_productos + 1] = productos[h] Entonces
                            Escribir "Producto inválido"
                            Escribir "Por favor ingrese un producto no repetido"
                            Leer productos[num_productos + 1]
                            h <- 0 // Reiniciar la verificación
                        FinSi
                    FinPara
                    
                    Escribir Sin Saltar "Ingrese la cantidad inicial del producto ", productos[num_productos + 1], ": "
                    Leer inventario[num_productos + 1]
                    
                    // Generar precio aleatorio entre 10 y 50
                    precio[num_productos + 1] <- Aleatorio(10, 50)
                    
                    num_productos <- num_productos + 1
                FinPara
                
            Caso 2:
                // Consulta de datos
                Escribir "========================================"
                Escribir "|   PRODUCTO   |   CANT    |   PRECIO  |" 
                Escribir "========================================"
                
                Para i <- 1 Hasta num_productos Con Paso 1 Hacer
                    es_f <- 13 - Longitud(productos[i])
                    Escribir sin saltar "| ", productos[i]
                    Para l <- 1 Hasta es_f Con Paso 1 Hacer
                        Escribir Sin Saltar " "
                    FinPara
                    
                    Escribir Sin Saltar "| "
                    es_f <- 10 - Longitud(ConvertirATexto(inventario[i]) + " Unid")
                    Escribir Sin Saltar inventario[i], " Unid"
                    Para l <- 1 Hasta es_f Con Paso 1 Hacer
                        Escribir Sin Saltar " "
                    FinPara
                    
                    Escribir Sin Saltar "| "
                    es_f <- 10 - Longitud("$" + ConvertirATexto(precio[i]))
                    Escribir Sin Saltar "$", precio[i]
                    Para l <- 1 Hasta es_f Con Paso 1 Hacer
                        Escribir Sin Saltar " "
                    FinPara
                    
                    Escribir "|"
                    Escribir "========================================"
                FinPara
                
            Caso 3:
                Si num_factura_actual > 20 Entonces
                    Escribir "Límite de facturas diarias alcanzado. Intente nuevamente mañana."
                Sino
                    Definir nombre, ruc, direccion, articulo, continua, fecha_factura como caracter
                    Definir cantidad, nro_art, telefono, indice_producto, total_item como Entero
                    total <- 0 // Reiniciar el total para cada factura
                    
                    Dimension articulo(10)
                    Dimension cantidad(10)
                    
                    Repetir
						total = 0
                        Escribir "INGRESE DATOS DEL CLIENTE"
                        Escribir "================================================="
                        Escribir Sin Saltar "Nombre: " 
                        Leer nombre
                        Escribir Sin Saltar "RUC: "
                        Leer ruc
                        Escribir Sin Saltar "Dirección: "
                        Leer direccion
                        Escribir Sin Saltar "Teléfono: "
                        Leer telefono
                        Escribir ""
                        Escribir "INGRESE ARTÍCULOS"
                        Escribir "================================================="
                        continua <- "s"; nro_art <- 1
                        
                        Repetir
                            Escribir Sin Saltar "Artículo Nro ", nro_art, ": " 
                            Leer articulo(nro_art)
                            
                            // Buscar el índice del producto para obtener el precio unitario
                            indice_producto <- 0
                            Para i <- 1 Hasta num_productos Con Paso 1 Hacer
                                Si articulo(nro_art) = productos[i] Entonces
                                    indice_producto <- i
                                FinSi
                            FinPara
                            
                            Si indice_producto = 0 Entonces
                                Escribir "Producto no registrado. Intente nuevamente."
                            Sino
                                Escribir "Stock disponible para ", productos[indice_producto], ": ", inventario[indice_producto], " Unid"
                                
                                Escribir Sin Saltar "Cantidad: " 
                                Leer cantidad(nro_art)
                                
                                // Verificar que haya suficiente stock
                                Si cantidad(nro_art) > inventario[indice_producto] Entonces
                                    Escribir "Cantidad insuficiente. Stock disponible: ", inventario[indice_producto]
                                Sino
                                    Escribir "Precio Unitario (automático): S/. ", precio[indice_producto]
                                    total_item <- cantidad(nro_art) * precio[indice_producto]
                                    total <- total + total_item
                                    
                                    // Deducción del stock solo después de validar la cantidad disponible
                                    // Actualizar inventario solo después de validar la cantidad disponible
                                    
                                    nro_art <- nro_art + 1
                                FinSi
                            FinSi
                            
                            Escribir "Ingresa otro artículo (s/n)? "; Leer continua
                            Escribir ""
                        Hasta Que continua = "n"
                        
                        // Generar fecha aleatoriamente
                        fecha_factura <- ConvertirATexto(Aleatorio(1, 28)) + "/" + ConvertirATexto(Aleatorio(1, 12)) + "/" + ConvertirATexto(Aleatorio(2024,2025))
                        
                        Escribir ""
                        Escribir "=============================================================="
                        Escribir "|EMPRESA COMERCIAL K&K","                     ","FECHA: ", fecha_factura," |"
                        Escribir "|Cliente  :", nombre,"                            ","RUC:      ", ruc,         "     |"
                        Escribir "|Dirección: ", direccion,"                         ","FACTURA N°", num_factura_actual,"       |"
                        Escribir "=============================================================="
                        Escribir "|","    DESCRIPCIÓN    ","|","  CANT  ","|","  PRECIO UNIT  ","|","    P. TOTAL  "," |"
                        Escribir "=============================================================="
                        
                        Para i <- 1 Hasta (nro_art - 1) Con Paso 1 Hacer
                            // Obtener el índice del producto para obtener el precio correcto
                            indice_producto <- 0
                            Para j <- 1 Hasta num_productos Con Paso 1 Hacer
                                Si articulo(i) = productos[j] Entonces
                                    indice_producto <- j
                                FinSi
                            FinPara
                            
                            total_item <- cantidad(i) * precio[indice_producto]
                            Escribir "|", "  ",articulo(i), "       ", "   |  ", "  ",cantidad(i),"  "," |", "     " ,"S/.",precio[indice_producto],"    "," |","     ","S/.", total_item,"    |"
                            
                            // Actualizar inventario después de la factura
                            inventario[indice_producto] <- inventario[indice_producto] - cantidad(i)
                        FinPara
                        
                        Escribir "=============================================================="
                        Escribir "| Tenemos tus necesidades |",  "   SUBTOTAL:    ","|", "  S/. ", total                 ,"    ","    |"
                        Escribir "|     como prioridad      |",  "   IMPUESTO 15%:","|", "  S/. ", total * 0.15          ,"       ","  |"
                        Escribir "|  Gracias por su compra  |",  "   TOTAL:       ","|", "  S/. ", total + (total * 0.15),"       "," |"
                        Escribir "=============================================================="
                        Escribir ""
                        
                        // Guardar el total de la factura y actualizar total de ventas diarias
                        totales_facturas[num_factura_actual] <- total + (total * 0.15)
                        total_ventas_diarias <- total_ventas_diarias + totales_facturas[num_factura_actual]
                        
                        // Incrementar el número de factura para la siguiente factura
                        num_factura_actual <- num_factura_actual + 1
                        
                        // Preguntar si desea generar otra factura
                        Escribir "¿Desea generar otra factura? (s/n): "
                        Leer continuar
                        Escribir ""
                    Hasta Que continuar = "n"
                FinSi
                
            Caso 4:
                // Informe de ventas
                Escribir "=================================="
                Escribir "|       INFORME DE VENTAS        |"
                Escribir "=================================="
                Escribir "|  FACTURA N°  |  TOTAL FACTURA  |"
                Escribir "=================================="
                
                Para i <- 1 Hasta (num_factura_actual - 1) Con Paso 1 Hacer
                    Escribir "|     ", i, "        |   S/. ", totales_facturas[i], "       |"
                FinPara
                
                Escribir "=================================="
                Escribir "| T.VENTAS DÍA: S/. ", total_ventas_diarias, "          |"
                Escribir "=================================="
                
                // Mostrar stock restante
                Escribir "=========================================="
                Escribir "|      STOCK RESTANTE EN INVENTARIO      |"
                Escribir "=========================================="
                Escribir "|   PRODUCTO  |    CANT    |    PRECIO   |"
                Escribir "=========================================="
                
                Para i <- 1 Hasta num_productos Con Paso 1 Hacer
                    es_f <- 13 - Longitud(productos[i])
                    Escribir sin saltar "|", productos[i]
                    Para l <- 1 Hasta es_f Con Paso 1 Hacer
                        Escribir Sin Saltar " "
                    FinPara
                    
                    Escribir Sin Saltar "|"
                    es_f <- 10 - Longitud(ConvertirATexto(inventario[i]) + " Unid")
                    Escribir Sin Saltar "  ", inventario[i], " Unid"
                    Para l <- 1 Hasta es_f Con Paso 1 Hacer
                        Escribir Sin Saltar " "
                    FinPara
                    
                    Escribir Sin Saltar "|"
                    es_f <- 8 - Longitud(ConvertirATexto(precio[i]))
                    Escribir Sin Saltar "   $ ", precio[i]
                    Para l <- 1 Hasta es_f Con Paso 1 Hacer
                        Escribir Sin Saltar " "
                    FinPara
                    
                    Escribir "|"
                    Escribir "=========================================="
                FinPara
                
            Caso 5:
                Escribir "Saliendo del sistema..."
                
            De Otro Modo:
                Escribir "Opción no válida, por favor intente de nuevo."
        FinSegun
        
    Hasta Que opcion = 5
    
FinAlgoritmo

