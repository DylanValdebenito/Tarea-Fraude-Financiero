/////////////////////////////////////////////////////////////////
/// ESTE C�DIGO FUE TESTEADO EN LA VERSI�N 20240122 DE PSEINT ///
/// VERSIONES DISTINTAS A ESTA PODR�AN ROMPERLO               ///
/////////////////////////////////////////////////////////////////

// Funci�n para verificar si el pa�s ingresado existe - ft. Copilot (IA)
Funcion isCountry <- onCheckCountry(orden)
    Definir pais, paises Como Caracter
	Definir i Como Entero
	Definir isCountry Como Logico
    paises = "Afganist�n, Albania, Alemania, Andorra, Angola, Antigua y Barbuda, Arabia Saudita, Argelia, Argentina, Armenia, Australia, Austria, Azerbaiy�n, Bahamas, Banglad�s, Barbados, Bar�in, B�lgica, Belice, Ben�n, Bielorrusia, Birmania, Bolivia, Bosnia y Herzegovina, Botsuana, Brasil, Brun�i, Bulgaria, Burkina Faso, Burundi, But�n, Cabo Verde, Camboya, Camer�n, Canad�, Catar, Chad, Chile, China, Chipre, Ciudad del Vaticano, Colombia, Comoras, Corea del Norte, Corea del Sur, Costa de Marfil, Costa Rica, Croacia, Cuba, Dinamarca, Dominica, Ecuador, Egipto, El Salvador, Emiratos �rabes Unidos, Eritrea, Eslovaquia, Eslovenia, Espa�a, Estados Unidos, Estonia, Etiop�a, Filipinas, Finlandia, Fiyi, Francia, Gab�n, Gambia, Georgia, Ghana, Granada, Grecia, Guatemala, Guinea, Guinea-Bis�u, Guinea Ecuatorial, Guyana, Hait�, Honduras, Hungr�a, India, Indonesia, Irak, Ir�n, Irlanda, Islandia, Islas Marshall, Islas Salom�n, Israel, Italia, Jamaica, Jap�n, Jordania, Kazajist�n, Kenia, Kirguist�n, Kiribati, Kuwait, Laos, Lesoto, Letonia, L�bano, Liberia, Libia, Liechtenstein, Lituania, Luxemburgo, Macedonia del Norte, Madagascar, Malasia, Malaui, Maldivas, Mal�, Malta, Marruecos, Mauricio, Mauritania, M�xico, Micronesia, Moldavia, M�naco, Mongolia, Montenegro, Mozambique, Namibia, Nauru, Nepal, Nicaragua, N�ger, Nigeria, Noruega, Nueva Zelanda, Om�n, Pa�ses Bajos, Pakist�n, Palaos, Panam�, Pap�a Nueva Guinea, Paraguay, Per�, Polonia, Portugal, Reino Unido, Rep�blica Centroafricana, Rep�blica Checa, Rep�blica del Congo, Rep�blica Democr�tica del Congo, Rep�blica Dominicana, Ruanda, Rumania, Rusia, Samoa, San Crist�bal y Nieves, San Marino, San Vicente y las Granadinas, Santa Luc�a, Santo Tom� y Pr�ncipe, Senegal, Serbia, Seychelles, Sierra Leona, Singapur, Siria, Somalia, Sri Lanka, Suazilandia, Sud�frica, Sud�n, Sud�n del Sur, Suecia, Suiza, Surinam, Tailandia, Tanzania, Tayikist�n, Timor Oriental, Togo, Tonga, Trinidad y Tobago, T�nez, Turkmenist�n, Turqu�a, Tuvalu, Ucrania, Uganda, Uruguay, Uzbekist�n, Vanuatu, Venezuela, Vietnam, Yemen, Yibuti, Zambia, Zimbabue"
    isCountry = Falso
	orden = Mayusculas(orden)
	
	// Verificaci�n de pa�s existente
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

// Funci�n para verificar si el RUT existe - ft. Carlos Mancilla T. (https://www.lawebdelprogramador.com/codigo/Pseudocodigo-Diagramas-de-Flujo/2512-Validar-el-RUT-Chileno.html#google_vignette)
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

// Funci�n para llamar el t�tulo
Funcion title <- onTitle
	Limpiar Pantalla
	Escribir "------------------------------------------------------------------------"
	Escribir "                     Algoritmo de Fraude Financiero                     "
	Escribir "------------------------------------------------------------------------"
FinFuncion

// Funci�n para llamar el t�tulo con usuario registrado
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
		
		// Recolecci�n de datos
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
			SiNo Escribir "Formato inv�lido. Intente de nuevo."
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
			Escribir "Edad inv�lida. Intente de nuevo."
			Leer edad
		FinMientras
		//   Origen de Fondos
		logedTitle <- onLogedTitle(session)
		Escribir "Ingrese el origen de los fondos (1-5)"
		Escribir "1. Cheque"
		Escribir "2. Vale vista"
		Escribir "3. Dep�sito a plazo endosable"
		Escribir "4. Efectivo"
		Escribir "5. Otro"
		Leer origen
		Mientras (origen < 1) o (origen > 5) Hacer
			Escribir "Opci�n inv�lida. Intente de nuevo."
			Leer origen
		FinMientras
		//   Valor de Dolar y Monto
		logedTitle <- onLogedTitle(session)
		Escribir "Ingrese el valor del d�lar"
		Leer USD
		Mientras USD <= 0 Hacer
			Escribir "Valor inv�lido. Intente de nuevo."
			Leer USD
		FinMientras
		Escribir "Ingrese el monto en CLP"
		Leer monto
		Mientras monto <= 0 Hacer
			Escribir "Cantidad inv�lida. Intente de nuevo."
			Leer monto
		FinMientras
		monto = monto / USD
		//   Pa�s de Orden
		logedTitle <- onLogedTitle(session)
		Escribir "Ingrese el pa�s de orden de los fondos"
		Leer orden
		isCountry = onCheckCountry(orden)
		Mientras isCountry = Falso Hacer
			Escribir "Pa�s inv�lido. Intente de nuevo."
			Leer orden
			isCountry = onCheckCountry(orden)
		FinMientras
		//   Para�so Fiscal
		logedTitle <- onLogedTitle(session)
		Escribir "�Proviene de empresas constituidas en para�sos fiscales? (SI/NO)"
		Leer paraiso
		Mientras (Mayusculas(paraiso) <> "SI") y (Mayusculas(paraiso) <> "NO") Hacer
			Escribir "Respuesta inv�lida. Intente de nuevo."
			Leer paraiso
		FinMientras
		
		// Comprobaci�n de sospechas
		Si (edad < 18) o (origen = 4) o (monto > 10000) o (Mayusculas(paraiso) = "SI") Entonces
			isSus = Verdadero
		FinSi
		Si edad < 18
			motivoList = "Menor de edad"
		FinSi
		Si origen = 4
			Si motivoList = "" Entonces
				motivoList = "M�todo de operaci�n riesgoso"
			SiNo
				motivo = ", m�todo de operaci�n riesgoso"
				motivoList = motivoList + motivo
			FinSi
		FinSi
		Si origen = 5
			Si motivoList = "" Entonces
				motivoList = "Sin origen espec�fico"
			SiNo
				motivo = ", sin origen espec�fico"
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
				motivoList = "Proveniente de para�so fiscal"
			SiNo
				motivo = ", proveniente de para�so fiscal"
				motivoList = motivoList + motivo
			FinSi
		FinSi
		orden = Mayusculas(orden)
		Si (orden = "AFGANIST�N") o (orden = "LOS BALCANES") o (orden = "BIELORUSIA") o (orden = "BURMA") o (orden = "CUBA") o (orden = "REP�BLICA CENTROAFRICANA") o (orden = "REP�BLICA DEMOCR�TICA DEL CONGO") o (orden = "ETIOP�A") o (orden = "HONG KONG") o (orden = "IR�N") o (orden = "IRAK") o (orden = "L�BANO") o (orden = "LIBIA") o (orden = "MAL�") o (orden = "NICARAGUA") o (orden = "COREA DEL NORTE") o (orden = "RUSIA") o (orden = "SOMALIA") o (orden = "SUD�N") o (orden = "SUD�N DEL SUR") o (orden = "SIRIA") o (orden = "UCRANIA") o (orden = "VENEZUELA") o (orden = "YEMEN") o (orden = "ZIMBABUE") Entonces
			isSus = Verdadero
			Si motivoList = "" Entonces
				motivoList = "Pa�s prohibido"
			SiNo
				motivo = ", pa�s proh�bido"
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
			Escribir "Operaci�n sospechosa detectada. Almacenando RUT..."
			Escribir "MOTIVO: ", motivoList
		SiNo
			Escribir "Sin sospechas. La operaci�n se llevar� a cabo en breves..."
		FinSi
		
		// Condicionalidad de cierre de algoritmo	
		Esperar 3 Segundos
		logedTitle <- onLogedTitle(session)
		Escribir "HECHO."
		Esperar 1 Segundos
		Escribir "�Desea realizar otra operaci�n? (SI/NO)"
		Leer continue
		Mientras (Mayusculas(continue) <> "SI") y (Mayusculas(continue) <> "NO") Hacer
			Escribir "Opci�n inv�lida. Intente de nuevo."
			Leer continue
		FinMientras
		Si (Mayusculas(continue) = "NO") Entonces
			isContinue = Falso
			Escribir "Cerrando sesi�n..."
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