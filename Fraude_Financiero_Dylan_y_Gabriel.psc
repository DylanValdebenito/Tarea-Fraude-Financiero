/////////////////////////////////////////////////////////////////
/// ESTE CÓDIGO FUE TESTEADO EN LA VERSIÓN 20240122 DE PSEINT ///
/// VERSIONES DISTINTAS A ESTA PODRÍAN ROMPERLO               ///
/////////////////////////////////////////////////////////////////

// Función para verificar si el país ingresado existe - ft. Copilot (IA)
Funcion isCountry <- onCheckCountry(orden)
    Definir pais, paises Como Caracter
	Definir i Como Entero
	Definir isCountry Como Logico
    paises = "Afganistán, Albania, Alemania, Andorra, Angola, Antigua y Barbuda, Arabia Saudita, Argelia, Argentina, Armenia, Australia, Austria, Azerbaiyán, Bahamas, Bangladés, Barbados, Baréin, Bélgica, Belice, Benín, Bielorrusia, Birmania, Bolivia, Bosnia y Herzegovina, Botsuana, Brasil, Brunéi, Bulgaria, Burkina Faso, Burundi, Bután, Cabo Verde, Camboya, Camerún, Canadá, Catar, Chad, Chile, China, Chipre, Ciudad del Vaticano, Colombia, Comoras, Corea del Norte, Corea del Sur, Costa de Marfil, Costa Rica, Croacia, Cuba, Dinamarca, Dominica, Ecuador, Egipto, El Salvador, Emiratos Árabes Unidos, Eritrea, Eslovaquia, Eslovenia, España, Estados Unidos, Estonia, Etiopía, Filipinas, Finlandia, Fiyi, Francia, Gabón, Gambia, Georgia, Ghana, Granada, Grecia, Guatemala, Guinea, Guinea-Bisáu, Guinea Ecuatorial, Guyana, Haití, Honduras, Hungría, India, Indonesia, Irak, Irán, Irlanda, Islandia, Islas Marshall, Islas Salomón, Israel, Italia, Jamaica, Japón, Jordania, Kazajistán, Kenia, Kirguistán, Kiribati, Kuwait, Laos, Lesoto, Letonia, Líbano, Liberia, Libia, Liechtenstein, Lituania, Luxemburgo, Macedonia del Norte, Madagascar, Malasia, Malaui, Maldivas, Malí, Malta, Marruecos, Mauricio, Mauritania, México, Micronesia, Moldavia, Mónaco, Mongolia, Montenegro, Mozambique, Namibia, Nauru, Nepal, Nicaragua, Níger, Nigeria, Noruega, Nueva Zelanda, Omán, Países Bajos, Pakistán, Palaos, Panamá, Papúa Nueva Guinea, Paraguay, Perú, Polonia, Portugal, Reino Unido, República Centroafricana, República Checa, República del Congo, República Democrática del Congo, República Dominicana, Ruanda, Rumania, Rusia, Samoa, San Cristóbal y Nieves, San Marino, San Vicente y las Granadinas, Santa Lucía, Santo Tomé y Príncipe, Senegal, Serbia, Seychelles, Sierra Leona, Singapur, Siria, Somalia, Sri Lanka, Suazilandia, Sudáfrica, Sudán, Sudán del Sur, Suecia, Suiza, Surinam, Tailandia, Tanzania, Tayikistán, Timor Oriental, Togo, Tonga, Trinidad y Tobago, Túnez, Turkmenistán, Turquía, Tuvalu, Ucrania, Uganda, Uruguay, Uzbekistán, Vanuatu, Venezuela, Vietnam, Yemen, Yibuti, Zambia, Zimbabue"
    isCountry = Falso
	orden = Mayusculas(orden)
	
	// Verificación de país existente
	i = 0
	Mientras i < Longitud(paises) Hacer
		pais = ""
		Mientras i < Longitud(paises) y Subcadena(paises, i, i) <> "," Hacer
			pais = pais + Subcadena(paises, i, i)
			i = i + 1
		FinMientras
		Si Mayusculas(pais) = orden Entonces
			isCountry = Verdadero
		FinSi
		i = i + 2
	FinMientras
FinFuncion

// Función para verificar si el RUT existe - ft. Carlos Mancilla T. (https://www.lawebdelprogramador.com/codigo/Pseudocodigo-Diagramas-de-Flujo/2512-Validar-el-RUT-Chileno.html#google_vignette)
Funcion isRut <- onCheckRut(rut)
	Definir numRut, a1, pa, c, sum, di, digi Como Entero
	dvt = Subcadena(rut, Longitud(rut) - 1, Longitud(rut) - 1)
	Para ii = 0 Hasta Longitud(rut) - 2 Con Paso 1 Hacer
		digitRut = Subcadena(rut, ii, ii)
		Si digitRut <> "." y digitRut <> "-" Entonces
			shortRut = shortRut + Subcadena(rut, ii, ii)
		FinSi
	FinPara
	numRut = ConvertirANumero(shortRut);
	pa = numRut;
	c = 2;
	sum = 0;
	Mientras numRut > 0 Hacer
		a1 = numRut MOD 10;
		numRut = trunc(numRut/10);
		sum = sum + (a1 * c);
		c = c + 1;
		Si c = 8 Entonces
			c = 2;
		FinSi
	FinMientras
	di = sum MOD 11;
	digi = 11 - di;
	digi1 = ConvertirATexto(digi);
	Si (digi1 = '10') Entonces
		digi1 = 'K';
	FinSi
	Si (digi1 = '11') Entonces
		digi1 = '0';
	FinSi
	Si (dvt = digi1) Entonces
		isRut = Verdadero
	FinSi
FinFuncion

// Función para llamar el título
Funcion title <- onTitle
	Limpiar Pantalla
	Escribir "------------------------------------------------------------------------"
	Escribir "                     Algoritmo de Fraude Financiero                     "
	Escribir "------------------------------------------------------------------------"
FinFuncion

// Función para llamar el título con usuario registrado
Funcion logedTitle <- onLogedTitle(session)
	Limpiar Pantalla
	Escribir "------------------------------------------------------------------------"
	Escribir "                     Algoritmo de Fraude Financiero         ", session
	Escribir "------------------------------------------------------------------------"
FinFuncion

// Algoritmo principal
Algoritmo Fraude_Financiero_Dylan_y_Gabriel
	Definir susList, motivo, motivoList, rut, session, orden, paraiso, continue Como Caracter
	Definir edad, origen, x Como Entero
	Definir monto, USD Como Real
	Definir isContinue, isSus, isFormatedRut Como Logico
	susList = ""
	motivoList = ""
	isContinue = Verdadero
	
	Mientras isContinue Hacer
		isSus = Falso
		isFormatedRut = Falso
		isRut = Falso
		
		// Recolección de datos
		//   RUT
		title <- onTitle
		Escribir "Ingrese el RUT de la persona (ejemplo: 12.345.678-9)"
		Mientras (isFormatedRut = Falso) o (isRut = Falso) Hacer
			Leer rut
			Si Subcadena(rut, Longitud(rut) - 10, Longitud(rut) - 10) = "." y Subcadena(rut, Longitud(rut) - 6, Longitud(rut) - 6) = "." y Subcadena(rut, Longitud(rut) - 2, Longitud(rut) - 2) = "-" Entonces
				isFormatedRut = Verdadero
				isRut <- onCheckRut(rut)
				Si isRut = Falso Entonces
					Escribir "Rut inexistente. Intente de nuevo."
				FinSi
			SiNo Escribir "Formato inválido. Intente de nuevo."
			FinSi
		FinMientras
		session = rut
		Escribir "Accediendo..."
		Esperar 1 Segundos
		//   Edad
		logedTitle <- onLogedTitle(session)
		Escribir "Ingrese la edad de la persona"
		Leer edad
		Mientras (edad <= 0) o (edad > 123) Hacer
			Escribir "Edad inválida. Intente de nuevo."
			Leer edad
		FinMientras
		//   Origen de Fondos
		logedTitle <- onLogedTitle(session)
		Escribir "Ingrese el origen de los fondos (1-5)"
		Escribir "1. Cheque"
		Escribir "2. Vale vista"
		Escribir "3. Depósito a plazo endosable"
		Escribir "4. Efectivo"
		Escribir "5. Otro"
		Leer origen
		Mientras (origen < 1) o (origen > 5) Hacer
			Escribir "Opción inválida. Intente de nuevo."
			Leer origen
		FinMientras
		//   Valor de Dolar y Monto
		logedTitle <- onLogedTitle(session)
		Escribir "Ingrese el valor del dólar"
		Leer USD
		Mientras USD <= 0 Hacer
			Escribir "Valor inválido. Intente de nuevo."
			Leer USD
		FinMientras
		Escribir "Ingrese el monto en CLP"
		Leer monto
		Mientras monto <= 0 Hacer
			Escribir "Cantidad inválida. Intente de nuevo."
			Leer monto
		FinMientras
		monto = monto / USD
		//   País de Orden
		logedTitle <- onLogedTitle(session)
		Escribir "Ingrese el país de orden de los fondos"
		Leer orden
		isCountry = onCheckCountry(orden)
		Mientras isCountry = Falso Hacer
			Escribir "País inválido. Intente de nuevo."
			Leer orden
			isCountry = onCheckCountry(orden)
		FinMientras
		//   Paraíso Fiscal
		logedTitle <- onLogedTitle(session)
		Escribir "¿Proviene de empresas constituidas en paraísos fiscales? (SI/NO)"
		Leer paraiso
		Mientras (Mayusculas(paraiso) <> "SI") y (Mayusculas(paraiso) <> "NO") Hacer
			Escribir "Respuesta inválida. Intente de nuevo."
			Leer paraiso
		FinMientras
		
		// Comprobación de sospechas
		Si (edad < 18) o (origen = 4) o (monto > 10000) o (Mayusculas(paraiso) = "SI") Entonces
			isSus = Verdadero
		FinSi
		Si edad < 18
			motivoList = "Menor de edad"
		FinSi
		Si origen = 4
			Si motivoList = "" Entonces
				motivoList = "Método de operación riesgoso"
			SiNo
				motivo = ", método de operación riesgoso"
				motivoList = motivoList + motivo
			FinSi
		FinSi
		Si origen = 5
			Si motivoList = "" Entonces
				motivoList = "Sin origen específico"
			SiNo
				motivo = ", sin origen específico"
				motivoList = motivoList + motivo
			FinSi
		FinSi
		Si monto > 10000
			Si motivoList = "" Entonces
				motivoList = "Monto excesivo"
			SiNo
				motivo = ", monto excesivo"
				motivoList = motivoList + motivo
			FinSi
		FinSi
		Si Mayusculas(paraiso) = "SI"
			Si motivoList = "" Entonces
				motivoList = "Proveniente de paraíso fiscal"
			SiNo
				motivo = ", proveniente de paraíso fiscal"
				motivoList = motivoList + motivo
			FinSi
		FinSi
		orden = Mayusculas(orden)
		Si (orden = "AFGANISTÁN") o (orden = "LOS BALCANES") o (orden = "BIELORUSIA") o (orden = "BURMA") o (orden = "CUBA") o (orden = "REPÚBLICA CENTROAFRICANA") o (orden = "REPÚBLICA DEMOCRÁTICA DEL CONGO") o (orden = "ETIOPÍA") o (orden = "HONG KONG") o (orden = "IRÁN") o (orden = "IRAK") o (orden = "LÍBANO") o (orden = "LIBIA") o (orden = "MALÍ") o (orden = "NICARAGUA") o (orden = "COREA DEL NORTE") o (orden = "RUSIA") o (orden = "SOMALIA") o (orden = "SUDÁN") o (orden = "SUDÁN DEL SUR") o (orden = "SIRIA") o (orden = "UCRANIA") o (orden = "VENEZUELA") o (orden = "YEMEN") o (orden = "ZIMBABUE") Entonces
			isSus = Verdadero
			Si motivoList = "" Entonces
				motivoList = "País prohibido"
			SiNo
				motivo = ", país prohíbido"
				motivoList = motivoList + motivo
			FinSi
		FinSi
		
		// Resultado de sospecha
		logedTitle <- onLogedTitle(session)
		Si isSus Entonces
			Si susList = "" Entonces
				susList = Concatenar(susList, rut)
			SiNo
				rut = Concatenar(", ", rut)
				susList = Concatenar(susList, rut)
			FinSi
			Escribir "Operación sospechosa detectada. Almacenando RUT..."
			Escribir "MOTIVO: ", motivoList
		SiNo
			Escribir "Sin sospechas. La operación se llevará a cabo en breves..."
		FinSi
		
		// Condicionalidad de cierre de algoritmo	
		Esperar 3 Segundos
		logedTitle <- onLogedTitle(session)
		Escribir "HECHO."
		Esperar 1 Segundos
		Escribir "¿Desea realizar otra operación? (SI/NO)"
		Leer continue
		Mientras (Mayusculas(continue) <> "SI") y (Mayusculas(continue) <> "NO") Hacer
			Escribir "Opción inválida. Intente de nuevo."
			Leer continue
		FinMientras
		Si (Mayusculas(continue) = "NO") Entonces
			isContinue = Falso
			Escribir "Cerrando sesión..."
			Esperar 1 Segundos
			title <- onTitle
			Si susList = "" Entonces
				Escribir "No hay RUTs sospechos reportados."
			SiNo
				Escribir "RUTs enviados al oficial de cumplimiento encargado de informar a la UAF:"
				Escribir susList
			FinSi
		FinSi
	FinMientras
FinAlgoritmo