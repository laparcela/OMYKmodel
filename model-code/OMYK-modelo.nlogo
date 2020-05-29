extensions [ gis ]

breed [ biomasCombs biomasComb ]
breed [ hogares hogar ]
breed [ monos mono ]

globals [
  mapa
  tamanoCuadro
  longitudLadoCuadro
  contadorAnos

  temperatura_t? temperatura_t-1?
  precipitacion_t? precipitacion_t-1?
  presion_t? presion_t-1?
  turistas_t? turistas_t-1?
  turistasA_t? turistasA_t-1?

  huracan?
  velViento

  incendio?

  contadorCrearVecindad
  contadorRecorrerVecindades
  contadorBimestresTuristasAltoAno

  radioActividadesTransformado
  areaDeCultivoTransformado

  N_t+1

  areaQuemadaAno
  totalAreaQuemada
]

patches-own [
  tipoRaster

  edadSucesional
  tipo
  meQuemeEnBimestre
  cantidadBiomasaAqui_t-1
  cantidadBiomasaAqui_t
  ANP?
  agriculturaID
  apiculturaID
  vecindadID
  densidadMaxima
  numeroDeCuadrosDeMiVecindad
]

hogares-own [
  abrirMilpa_t? abrirMilpa_t-1?
  sembrarMilpa_t? sembrarMilpa_t-1?
  milpaJoven_t? milpaJoven_t-1?
  milpaAdulta_t? milpaAdulta_t-1?
  cosechaMilpa_t? cosechaMilpa_t-1?
  cosechaApicultura_t? cosechaApicultura_t-1?
  produccionCarbon_t? produccionCarbon_t-1?

  tiempoSinColmenas
  colmenas?

  hogarID

  sustentoBim
  listaSustentosAno
  mediaAnoSustento
  desviacionEstandarAnoSustento

  milpaH?
  apiculturaH?
  carbonH?
  turismoH?

  conjuntoDeParchesDondePuedoCultivar
]

biomasCombs-own [
  duracionBiomasComb
]

monos-own [
  miVecindadID
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SETUP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all

  set-default-shape hogares "house"
  set-default-shape biomasCombs "biomascomb"
  set-default-shape monos "mono"

  cargarMapa

  create-hogares numeroDeHogares [
    cargarArchivoHogares
    set xcor (xcor) set ycor (ycor)
    set color brown + 1
    set size 0.7
  ]

  set contadorAnos 1
  set tamanoCuadro 5367 / count patches with [ ANP? = true ];; se divide el tamaño del ANP en ha entre el número de cuadros en el modelo
  set longitudLadoCuadro sqrt( tamanoCuadro * 10000 )

  ask hogares [

    set abrirMilpa_t-1? false
    set sembrarMilpa_t-1? false
    set milpaJoven_t-1? false
    set milpaAdulta_t-1? false
    set cosechaMilpa_t-1? false
    set cosechaApicultura_t-1? true
    set produccionCarbon_t-1? true
    set tiempoSinColmenas 0

    set tipo "hogar"
    set edadSucesional 0

    set hogarID patch-here

    set listaSustentosAno []

    set areaQuemadaAno 0
    set totalAreaQuemada 0
  ]

  set temperatura_t-1? false
  set presion_t-1? true
  set precipitacion_t-1? true set precipitacion_t? false
  set turistas_t-1? true set turistas_t? true
  set turistasA_t-1? true set turistas_t? false

  set contadorBimestresTuristasAltoAno 0

  ask patches [ actualizarColor ]

  crearPoblacionMonos
  tormentas

  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; GO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  actualizarActividadesHogares
  actualizarTransformados
  clima
  tormentas
  turismo
  ask hogares [ actividadesHogar ]
  incendios
  ask monos [ moverse ]
  if ticks / 6 = contadorAnos [
    ask patches [ regenerarse ]
    dinamicaPoblacionalMonos
    actualizarVariablesAnuales
    set contadorAnos contadorAnos + 1
  ]
  memoriaRed
  ask patches [ actualizarColor ]
  tick
  ;export-view(word "mundo-"ticks ".png")
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PROCESOS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to actualizarActividadesHogares
  if-else leerActividades?
  [ leerActividadesHogar ]
  [
    ask hogares [
      if-else milpaT? [ set milpaH? true ] [ set milpaH? false ]
      if-else apiculturaT? [ set apiculturaH? true ] [ set apiculturaH? false ]
      if-else carbonT? [ set carbonH? true ] [ set carbonH? false ]
      if-else turismoT? [ set turismoH? true ] [ set turismoH? false ]
    ]
  ]
end

to actualizarTransformados
  set radioActividadesTransformado ( radioActividades * 1000 ) / longitudLadoCuadro
  set areaDeCultivoTransformado areaDeCultivo / tamanoCuadro
end

to clima
  if-else not precipitacion_t-1? [ set temperatura_t? true ] [ set temperatura_t? false ]
  if-else not temperatura_t-1? [ set presion_t? true ] [ set presion_t? false ]
  if-else not presion_t-1? [ set precipitacion_t? true ] [set precipitacion_t? false ]
end

to tormentas
  ask biomasCombs [ descomponerse ]
  set huracan? false
  set velViento 10
  acumularBiomasCombs
  if precipitacion_t? [
    if random-float 1.000 <= ( probOcurrTorTrop * multiplicarTormentas )     [ set velViento 20 acumularBiomasCombs]
    if random-float 1.000 <= ( probOcurrHuracanCat1 * multiplicarTormentas ) [ set velViento 74  set huracan? true acumularBiomasCombs  ask patches with [ tipo = "nada" ] [ set pcolor red + 2 ] ]
    if random-float 1.000 <= ( probOcurrHuracanCat2 * multiplicarTormentas ) [ set velViento 96  set huracan? true acumularBiomasCombs ask patches with [ tipo = "nada" ] [ set pcolor red + 1 ] ]
    if random-float 1.000 <= ( probOcurrHuracanCat3 * multiplicarTormentas ) [ set velViento 111 set huracan? true acumularBiomasCombs ask patches with [ tipo = "nada" ] [ set pcolor red ] ]
    if random-float 1.000 <= ( probOcurrHuracanCat4 * multiplicarTormentas ) [ set velViento 130 set huracan? true acumularBiomasCombs ask patches with [ tipo = "nada" ] [ set pcolor red - 1] ]
    if random-float 1.000 <= ( probOcurrHuracanCat5 * multiplicarTormentas ) [ set velViento 157 set huracan? true acumularBiomasCombs ask patches with [ tipo = "nada" ] [ set pcolor red - 2] ]
  ]
  ask patches [actualizarCantidadBiomas]
  if not huracan? [ ask patches with [ tipo = "nada" ] [ set pcolor black ]]
end

to descomponerse
  set duracionBiomasComb duracionBiomasComb - 1
  if duracionBiomasComb <= 0 [ die ]
end

to acumularBiomasCombs
  ask n-of ( (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2 )))) * (count patches with [ tipo = "selva" or tipo = "milpa" ] )) patches with [ tipo = "selva" or tipo = "milpa"] [ sprout-biomasCombs 1 [ set duracionBiomasComb random-normal mediaDuracionBiomasComb desviacionEstandarDuracionBiomasComb ]]
end

to actualizarCantidadBiomas
  set cantidadBiomasaAqui_t-1 cantidadBiomasaAqui_t  set cantidadBiomasaAqui_t count biomascombs-here
end

to turismo
  if-else ( precipitacion_t-1? or not precipitacion_t-1? ) and not huracan? [ set turistas_t? true ] [ set turistas_t? false ]
  if temporalidadTuristas = "2_bimestres_baja" [ if-else turistas_t-1? and not ( precipitacion_t-1? and not temperatura_t-1?) and not huracan?                                                          [ set turistasA_t? true set contadorBimestresTuristasAltoAno contadorBimestresTuristasAltoAno + 1 ] [ set turistasA_t? false ] ]
  if temporalidadTuristas = "2_bimestres_alta" [ if-else turistas_t-1? and temperatura_t-1? and (presion_t-1? or precipitacion_t-1?) and not huracan?                                                   [ set turistasA_t? true set contadorBimestresTuristasAltoAno contadorBimestresTuristasAltoAno + 1 ] [ set turistasA_t? false ] ]
  if temporalidadTuristas = "aleatoria_prob"   [ if-else turistas_t-1? and not huracan? and random-float 1.00 <= probTuristasAltoBim                                                                    [ set turistasA_t? true set contadorBimestresTuristasAltoAno contadorBimestresTuristasAltoAno + 1 ] [ set turistasA_t? false ] ]
  if temporalidadTuristas = "2_bimestres_alta_prob" [ if-else turistas_t-1? and temperatura_t-1? and (presion_t-1? or precipitacion_t-1?) and not huracan? and random-float 1.00 <= probTuristasAltoBim [ set turistasA_t? true set contadorBimestresTuristasAltoAno contadorBimestresTuristasAltoAno + 1 ] [ set turistasA_t? false ] ]
  ask hogares [ if count monos in-radius radioActividadesTransformado < numeroMinDeMonosParaTurismo [ set turistas_t? false set turistasA_t? false ] ]
  if not turistas? or not any? hogares with [ turismoH? = true ] [ set turistas_t? false set turistasA_t? false ]
end

to actividadesHogar
  let yo who
  ;;;;;;;;;;;;;;;; MILPA ;;;;;;;;;;;;;;;;
  if-else ( milpaH? and  temperatura_t-1? and not precipitacion_t-1? and presion_t-1? ) [ set abrirMilpa_t? true prepararParcela ] [ set abrirMilpa_t? false if (temperatura_t-1? and not precipitacion_t-1? and presion_t-1? and not milpaH?)  [ ask patches with [ agriculturaID = yo ] [ set agriculturaID nobody ] ]]
  if-else ( abrirMilpa_t-1? ) [ set sembrarMilpa_t? true ] [ set sembrarMilpa_t? false ]
  if-else ( sembrarMilpa_t-1? and precipitacion_t-1? ) and ( not huracan? or ( huracan? and sum([cantidadBiomasaAqui_t] of patches with [agriculturaID = yo]) <= sum([cantidadBiomasaAqui_t-1] of patches with [agriculturaID = yo]))) [ set milpaJoven_t? true ] [ set milpaJoven_t? false ]
  if-else ( milpaJoven_t-1? and precipitacion_t-1? ) and ( not huracan? or ( huracan? and sum([cantidadBiomasaAqui_t] of patches with [agriculturaID = yo]) <= sum([cantidadBiomasaAqui_t-1] of patches with [agriculturaID = yo])))  [ set milpaAdulta_t? true ] [ set milpaAdulta_t? false ]
  if-else ( milpaAdulta_t-1? and precipitacion_t-1? ) and ( not huracan? or ( huracan? and sum([cantidadBiomasaAqui_t] of patches with [agriculturaID = yo]) <= sum([cantidadBiomasaAqui_t-1] of patches with [agriculturaID = yo]))) [ set cosechaMilpa_t? true ] [ set cosechaMilpa_t? false ]

  ;;;;;;;;;;;;;; APICULTURA ;;;;;;;;;;;;;
  if-else apiculturaH? = true
  [ if not any? patches with [ apiculturaID = yo ] [ ask one-of patches in-radius radioActividadesTransformado with [ tipo != "agua" and tipo != "hogar" and tipo != "nada"] [ set apiculturaID yo ]]]
  [ ask patches with [ apiculturaID = yo ] [ set apiculturaID nobody ]]

  if-else huracan? and sum ([ cantidadBiomasaAqui_t] of patches with [apiculturaID = yo]) > sum([cantidadBiomasaAqui_t-1] of patches with [apiculturaID = yo])
  [
    set colmenas? false
    set tiempoSinColmenas 12
  ]
  [
    if-else tiempoSinColmenas > 0
    [ set colmenas? false set tiempoSinColmenas tiempoSinColmenas - 1 ]
    [ set colmenas? true ]
  ]

  if-else ( apiculturaH? and  not precipitacion_t-1?
                         and colmenas? )
                            [ set cosechaApicultura_t? true ] [ set cosechaApicultura_t? false ]

  ;;;;;;;;;;;;;;; CARBÓN ;;;;;;;;;;;;;;;;
  if-else ( carbonH? and not cosechaMilpa_t?
                     and any? biomasCombs-on patches in-radius radioActividadesTransformado )
                        [ set produccionCarbon_t? true ask one-of biomasCombs-on patches in-radius radioActividadesTransformado [die]]
                        [ set produccionCarbon_t? false ]

  ;;;;;;;;;;;; SUSTENTO ;;;;;;;;;;;;;;;;;
  ;; el sustento representa el salario mínimo diario promedio al bimestre, por ejemplo, si se cosecha la milpa el valor monetario equivale a que en el bimestre se ganara cada día 3.89 salarios mínimos
  set sustentoBim 0
  if cosechaMilpa_t? [ set sustentoBim sustentoBim + 3.89 ]
  if cosechaApicultura_t? [ set sustentoBim sustentoBim + 0.58 ]
  if produccionCarbon_t? [ set sustentoBim sustentoBim + 0.36 ]
  if turistasA_t? and turismoH? and not milpaH? [ set sustentoBim sustentoBim + 10.46 ]
  if turistas_t? and turismoH? and not milpaH? [ set sustentoBim sustentoBim + 2.09 ]
  if turistasA_t? and turismoH? and milpaH? [ set sustentoBim sustentoBim + 6.01 ]
  if turistas_t? and turismoH? and milpaH? [ set sustentoBim sustentoBim + 1.31 ]
  set listaSustentosAno lput sustentoBim listaSustentosAno
end

to prepararParcela
  let yo who
  if-else any? patches with [ agriculturaID = yo ]
  [
    if-else mean([ edadSucesional ] of patches with [ agriculturaID = yo ]) < tiempoUsoMismaParcela
      [ prepararMismaParcela ]
      [ ask patches with [ agriculturaID = yo ] [ set agriculturaID nobody ] prepararNuevaParcela ] ; abandona la parcela que ya tenía y prepara una nueva parcela
  ]
  [ prepararNuevaParcela ]
end

to prepararMismaParcela
  let yo who
  ask patches with [ agriculturaID = yo ] [ set tipo "milpa" ask biomasCombs-here [die]]
end

to prepararNuevaParcela
  actualizarParchesDondePuedoCultivar
  let yo who
  if-else count conjuntoDeParchesDondePuedoCultivar >= areaDeCultivoTransformado ;; esto hace que si no hay donde cultivar no cultive [ supuesto fuerte ]
    [ ask n-of areaDeCultivoTransformado conjuntoDeParchesDondePuedoCultivar [ set tipo "milpa" set agriculturaID yo  ask biomasCombs-here [ die ] set edadSucesional 0 ] ]
    [ set abrirMilpa_t? false ]
end

to actualizarParchesDondePuedoCultivar
  if cultivarEnSelva>50? and cultivarDentroDeANP? [
    set conjuntoDeParchesDondePuedoCultivar patches in-radius radioActividadesTransformado with [ edadSucesional >= periodoMinimoDeBarbecho and tipo != "hogar" and tipo != "agua" and tipo != "nada" ]
  ]
  if cultivarEnSelva>50? and not cultivarDentroDeANP? [
    set conjuntoDeParchesDondePuedoCultivar patches in-radius radioActividadesTransformado with [ not ANP? and edadSucesional >= periodoMinimoDeBarbecho and tipo != "hogar" and tipo != "agua" and tipo != "nada"]
  ]
  if not cultivarEnSelva>50? and cultivarDentroDeANP? [
    set conjuntoDeParchesDondePuedoCultivar patches in-radius radioActividadesTransformado with [ edadSucesional <= 50 and edadSucesional >= periodoMinimoDeBarbecho and tipo != "hogar" and tipo != "agua" and tipo != "nada"]
  ]
  if not cultivarEnSelva>50? and not cultivarDentroDeANP? [
    set conjuntoDeParchesDondePuedoCultivar patches in-radius radioActividadesTransformado with [ edadSucesional <= 50 and not ANP? and edadSucesional >= periodoMinimoDeBarbecho and tipo != "hogar" and tipo != "agua" and tipo != "nada"]
  ]
end

to incendios
  set incendio? false
  ask patches [ set meQuemeEnBimestre false ]
  if not precipitacion_t? and any? biomasCombs and random-float 1.00 <= ( probOcurrenciaIncendioBimSecas * multiplicarProbOcurIncendios ) [
    ask max-one-of patches [ count biomasCombs-here ]  [ quemarse ]
    set incendio? true
  ]
end

to quemarse
  set edadSucesional 0
  set meQuemeEnBimestre true
  ask biomasCombs-here [ die ]
  set tipo "quemado"
  ask neighbors4 with [ any? biomasCombs-here ] [
    if random-float 1.00 <= probQuemarse [ quemarse ]
  ]
end

to memoriaRed
  set temperatura_t-1? temperatura_t?
  set presion_t-1? presion_t?
  set precipitacion_t-1? precipitacion_t?
  set turistas_t-1? turistas_t?
  set turistasA_t-1? turistasA_t?
  ask hogares [
    set abrirMilpa_t-1? abrirMilpa_t?
    set sembrarMilpa_t-1? sembrarMilpa_t?
    set milpaJoven_t-1? milpaJoven_t?
    set milpaAdulta_t-1? milpaAdulta_t?
    set cosechaMilpa_t-1? cosechaMilpa_t?
    set cosechaApicultura_t-1? cosechaApicultura_t?
    set produccionCarbon_t-1? produccionCarbon_t?
  ]
end

to actualizarColor
  if tipo = "hogar"      [ set pcolor brown + 3 ]
  if tipo = "agua"       [ set pcolor blue      ]
  if tipo = "quemado"    [ set pcolor brown + 1 ]
  if tipo = "milpa"      [ set pcolor brown     ]
  if tipo = "selva" and edadSucesional >= 2 and edadSucesional <= 15 [ set pcolor green + 2 ]
  if tipo = "selva" and edadSucesional >= 16 and edadSucesional <= 30 [ set pcolor green + 1 ]
  if tipo = "selva" and edadSucesional >= 30 and edadSucesional <= 50 [ set pcolor green     ]
  if tipo = "selva" and edadSucesional > 50                           [ set pcolor green - 1 ]
  if tipo = "selva" and edadSucesional >= 30 and edadSucesional <= 50 [ set densidadMaxima ( densidadMax30-50 / 100 ) * tamanoCuadro ]
  if tipo = "selva" and edadSucesional > 50                           [ set densidadMaxima ( densidadMax>50 / 100) * tamanoCuadro ]
  if ( tipo = "selva" and edadSucesional < 30 ) or tipo != "selva"    [ set densidadMaxima 0 ]
end

to moverse
  if ((tipo = "quemado" or tipo = "milpa") and any? patches in-radius 100 with [ edadSucesional >= 30 ])
  [
    move-to min-one-of (patches in-radius 100 with [ edadSucesional >= 30 ]) [distance myself]
  ]
  if any? neighbors4 with [ edadSucesional >= 30 ] [
    if-else [ edadSucesional ] of patch-here > 50
    [
      if random-float 1.00 > probQuedarseEnSelva>50 [ move-to one-of neighbors4 with [ edadSucesional >= 30 ] ]
    ]
    [
      if random-float 1.00 > probQuedarseEnSelva30-50 [ move-to one-of neighbors4 with [ edadSucesional >= 30 ] ]
    ]
  ]
end

to regenerarse
 if (tipo = "selva" or tipo = "milpa" or tipo = "quemado") [
    set edadSucesional edadSucesional + 1
    if edadSucesional >= 2 and agriculturaID = nobody [ set tipo "selva" ]
  ]
end

to dinamicaPoblacionalMonos
  set contadorCrearVecindad 1
  ask patches [ set vecindadID 0 ]
  while [ any? patches with [ vecindadID = 0 and any? monos-here ]] [
    ask one-of patches with [ vecindadID = 0 and any? monos-here ] [ formarVecindad ]
    set contadorCrearVecindad contadorCrearVecindad + 1
  ]
  ask patches with [ vecindadID != 0] [ set numeroDeCuadrosDeMiVecindad count patches with [ vecindadID = [ vecindadID ] of myself ]]
  ask monos [ set miVecindadID vecindadID ]

  set contadorRecorrerVecindades 1
  while [ contadorRecorrerVecindades <= max [ vecindadID ] of patches ] [
    ask one-of patches with [ vecindadID = contadorRecorrerVecindades ] [
      let k numeroDeCuadrosDeMiVecindad * densidadMaxima
      if-else k = 0
      [ ask monos with [ miVecindadID = [ vecindadID ] of myself ] [ die ] ]
      [
        let N_t count monos with [ miVecindadID = [ vecindadID ] of myself ]
        set N_t+1 N_t * ( 1 + ( R - ( contadorBimestresTuristasAltoAno * ( R / 6 ))) * ( 1 - ( N_t / k )))
        if-else N_t+1 - N_t > 0
        [ sprout-monos round ( N_t+1 - N_t ) [ set miVecindadID vecindadID set color brown - 1 ]]
        [ if-else round ( N_t - N_t+1 ) <= count monos with [ miVecindadID = [ vecindadID ] of myself ]
          [ ask n-of round ( N_t - N_t+1 ) monos with [ miVecindadID = [ vecindadID ] of myself ] [ die ]]
          [ ask monos with [ miVecindadID = [ vecindadID ] of myself ] [ die ] ]
        ]
      ]
    ]
    set contadorRecorrerVecindades contadorRecorrerVecindades + 1
  ]
end

to formarVecindad
  set vecindadID contadorCrearVecindad
  ask neighbors4 with [ pcolor =  [ pcolor ] of myself and vecindadID = 0 ] [ formarVecindad ]
end

to actualizarVariablesAnuales
  set areaQuemadaAno 0
  set areaQuemadaAno count patches with [ tipo = "quemado" and edadSucesional = 1 ] * tamanoCuadro
  set totalAreaQuemada totalAreaQuemada + areaQuemadaAno
  set contadorBimestresTuristasAltoAno 0
  ask hogares [set mediaAnoSustento mean listaSustentosAno set desviacionEstandarAnoSustento standard-deviation listaSustentosAno set listaSustentosAno [] ]
end

to leerActividadesHogar
  file-open "./archivos-extra/actividadesHogares.txt"
  ask hogares
  [
    set milpaH? file-read
    set apiculturaH? file-read
    set carbonH? file-read
    set turismoH? file-read
  ]
  file-close
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;; PROCESOS INICIALIZACÓN ;;;;;;;;;;;;;;;;;;;;;;;

to cargarMapa
  if max-pxcor = 36 and max-pycor = 93 [ set mapa gis:load-dataset "./archivos-extra/omyk_2003_r_173_a.asc" ]
  if max-pxcor = 60 and max-pycor = 161 [ set mapa gis:load-dataset "./archivos-extra/omyk_2003_r_100_a.asc" ]
  gis:set-world-envelope ( gis:envelope-of mapa )
  gis:apply-raster mapa tipoRaster
;; Se traducen los atributos del raster a los atributos del modelo
;; Los valores de tipoRaster son:
;; 1: Agua somera, Agua profunda, Vegetacion inundable
;; 2: Agricultura
;; 3: Vegatacion menor_15, Urbano
;; 4: Vegetacion 16_29
;; 5: Vegetacion 30_50
;; 6: Selva madura
  ask patches [
    set agriculturaID nobody
    set ANP? false
    set tipo "nada"

    if tipoRaster = 1 [ set tipo "agua"  set edadSucesional 0  set ANP? true ]
    if tipoRaster = 2 [ set tipo "milpa" set edadSucesional 0  set ANP? true ]
    if tipoRaster = 3 [ set tipo "selva" set edadSucesional 2  set ANP? true ]
    if tipoRaster = 4 [ set tipo "selva" set edadSucesional 16 set ANP? true ]
    if tipoRaster = 5 [ set tipo "selva" set edadSucesional 30 set ANP? true ]
    if tipoRaster = 6 [ set tipo "selva" set edadSucesional 51 set ANP? true ]
  ]

  if not soloANP? [
    ask patches with [ ANP? = false ] [
      set tipo "selva"
      set edadSucesional round ( random-normal 20 2 )
    ]
  ]
end

to cargarArchivoHogares
  if max-pxcor = 36 and max-pycor = 93 [
    file-open "./archivos-extra/hogares3ha.txt"
    ask hogares
    [ setxy file-read file-read ]
    file-close
  ]
  if max-pxcor = 60 and max-pycor = 161 [
    file-open "./archivos-extra/hogares1ha.txt"
    ask hogares
    [ setxy file-read file-read ]
    file-close
  ]
end

to crearPoblacionMonos
  set contadorCrearVecindad 1
  ask patches [ set vecindadID 0 ]
  while [ any? patches with [ vecindadID = 0 ]] [
    ask one-of patches with [ vecindadID = 0 ] [ formarVecindad ]
    set contadorCrearVecindad contadorCrearVecindad + 1
  ]
  ask patches with [ vecindadID != 0] [ set numeroDeCuadrosDeMiVecindad count patches with [ vecindadID = [ vecindadID ] of myself ]]

  set contadorRecorrerVecindades 1
  while [ contadorRecorrerVecindades <= max [ vecindadID ] of patches ] [
    ask one-of patches with [ vecindadID = contadorRecorrerVecindades ] [
      let k numeroDeCuadrosDeMiVecindad * densidadMaxima
      sprout-monos k [ set miVecindadID vecindadID set color brown - 1]
    ]
    set contadorRecorrerVecindades contadorRecorrerVecindades + 1
  ]
  ask monos [ move-to one-of patches with [ vecindadID =  [miVecindadID] of myself ] ]
end
@#$#@#$#@
GRAPHICS-WINDOW
205
10
435
583
-1
-1
6.0
1
10
1
1
1
0
0
0
1
0
36
0
93
1
1
1
ticks
30.0

BUTTON
50
109
123
142
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
53
144
116
177
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
53
179
116
212
NIL
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
775
10
1594
130
plot 1
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"selva >50" 1.0 0 -14439633 true "" "plot count patches with [ edadSucesional  > 50 ]"
"selva 30-50" 1.0 0 -13840069 true "" "plot count patches with [ edadSucesional >= 30 and edadSucesional <= 50 ] "
"selva 16-30" 1.0 0 -11085214 true "" "plot count patches with [ edadSucesional >= 16 and edadSucesional <= 29 ]"
"selva 2-15" 1.0 0 -8330359 true "" "plot count patches with [ edadSucesional >= 2  and edadSucesional <= 15 ]"
"menor a 2" 1.0 0 -6459832 true "" "plot count patches with [ tipo = \"milpa\" or tipo = \"milpaNueva\" or tipo = \"quemado\" ]"

MONITOR
466
12
531
57
años
ticks / 6
3
1
11

PLOT
775
132
1594
252
plot 2
NIL
NIL
0.0
100.0
0.0
3.0
true
true
"" ""
PENS
"precipitacion?" 1.0 1 -4528153 true "" "if-else precipitacion_t? [ plot 3 ] [ plot 0 ]"
"turistas" 1.0 1 -11221820 true "" "if-else turistas_t? [ if-else turistasA_t? [plot 2.5] [ plot 1.9] ] [ plot 0 ]"
"tormentas" 1.0 1 -2674135 true "" "if-else velViento >= 20[if-else velViento > 74[if-else velViento > 96[if-else velViento > 111[if-else velViento > 130[if-else velViento > 157[plot 2.0][plot 1.8]][plot 1.6]][plot 1.4]][plot 1.2 ]][plot 0.6]][ plot 0]"
"incendios?" 1.0 1 -1184463 true "" "if-else incendio? = TRUE [ plot 0.5  + 3 * ( count patches with [ meQuemeEnBimestre ] / count patches with [ ANP? = true ]) ] [plot 0 ]"

SLIDER
540
352
758
385
parametroSigmoidea1
parametroSigmoidea1
0
0.1
0.025
0.001
1
NIL
HORIZONTAL

SLIDER
543
30
762
63
probQuemarse
probQuemarse
0
1
0.5
0.01
1
NIL
HORIZONTAL

SLIDER
542
66
762
99
probOcurrenciaIncendioBimSecas
probOcurrenciaIncendioBimSecas
0
1
0.093
0.001
1
NIL
HORIZONTAL

SLIDER
540
388
758
421
parametroSigmoidea2
parametroSigmoidea2
0
300
74.0
1
1
NIL
HORIZONTAL

SLIDER
540
424
758
457
mediaDuracionBiomasComb
mediaDuracionBiomasComb
0
24
12.0
1
1
bimestres
HORIZONTAL

SLIDER
540
460
758
493
desviacionEstandarDuracionBiomasComb
desviacionEstandarDuracionBiomasComb
0
24
3.0
1
1
bimestres
HORIZONTAL

CHOOSER
537
521
764
566
temporalidadTuristas
temporalidadTuristas
"2_bimestres_baja" "2_bimestres_alta" "aleatoria_prob" "2_bimestres_alta_prob"
3

SLIDER
537
570
764
603
probTuristasAltoBim
probTuristasAltoBim
0
1
0.7
0.01
1
NIL
HORIZONTAL

PLOT
775
255
1594
375
plot 3
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"cosecha apicultura" 1.0 0 -4699768 true "" "plot count hogares with [ cosechaApicultura_t? = true ]"
"produccion carbon" 1.0 0 -11221820 true "" "plot count hogares with [ produccionCarbon_t? = true ]"
"cosecha milpa" 1.0 0 -955883 true "" "plot count hogares with [ cosechaMilpa_t? = true ]"

PLOT
775
378
1593
498
plot 4
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"media bimestre sustento" 1.0 0 -11881837 true "" "plot mean [ sustentoBim ] of hogares"
"media media año sustentos " 1.0 0 -2674135 true "" "plot mean [ mediaAnoSustento ] of hogares"
"media d.e. sustentos año" 1.0 0 -2064490 true "" "plot mean [ desviacionEstandarAnoSustento ] of hogares"

TEXTBOX
545
10
706
28
PARÁMETROS INCENDIOS
12
0.0
1

TEXTBOX
543
110
727
140
PARÁMETROS TORMENTAS
12
0.0
1

TEXTBOX
539
501
689
519
PARÁMETROS TURISTAS
12
0.0
1

SLIDER
15
32
174
65
numeroDeHogares
numeroDeHogares
0
30
28.0
1
1
NIL
HORIZONTAL

TEXTBOX
14
622
184
652
PARÁMETROS ACTIVIDADES
12
0.0
1

SLIDER
12
640
254
673
tiempoUsoMismaParcela
tiempoUsoMismaParcela
1
5
2.0
1
1
años
HORIZONTAL

SLIDER
12
676
255
709
radioActividades
radioActividades
0.5
5
3.0
0.5
1
km
HORIZONTAL

SLIDER
12
711
255
744
periodoMinimoDeBarbecho
periodoMinimoDeBarbecho
0
20
5.0
1
1
años
HORIZONTAL

SLIDER
14
749
257
782
areaDeCultivo
areaDeCultivo
0
10
3.0
1
1
ha
HORIZONTAL

SWITCH
396
714
608
747
cultivarEnSelva>50?
cultivarEnSelva>50?
1
1
-1000

SWITCH
395
750
608
783
cultivarDentroDeANP?
cultivarDentroDeANP?
0
1
-1000

SWITCH
32
69
144
102
soloANP?
soloANP?
0
1
-1000

PLOT
1598
133
1798
253
duracion biomasa combustible
NIL
NIL
0.0
24.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ duracionBiomasComb ]of biomascombs"

PLOT
1598
256
1798
376
número de biomasa combustible por parche
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [count biomascombs-here] of patches "

PLOT
1598
10
1798
130
edad sucesional
NIL
NIL
0.0
200.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 1 -16777216 true "" "histogram [ edadSucesional ] of patches with [ ANP? = True and edadSucesional > 0 ]"

PLOT
1597
378
1797
498
velocidad del viento
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot velViento"

MONITOR
465
62
531
107
tamaño cuadro (ha)
tamanoCuadro
3
1
11

MONITOR
466
112
532
157
longitud lado cuadro
longitudLadoCuadro
3
1
11

SWITCH
263
604
390
637
milpaT?
milpaT?
0
1
-1000

SWITCH
263
640
391
673
apiculturaT?
apiculturaT?
0
1
-1000

SWITCH
263
676
391
709
carbonT?
carbonT?
0
1
-1000

SWITCH
635
644
763
677
turistas?
turistas?
0
1
-1000

TEXTBOX
1054
632
1358
662
PARÁMETROS DINÁMICA POBLACIONAL MONOS
12
0.0
1

SLIDER
1054
654
1286
687
R
R
0
2
0.1
0.01
1
NIL
HORIZONTAL

SLIDER
1052
691
1287
724
densidadMax30-50
densidadMax30-50
0
100
5.7
0.01
1
ind/km²
HORIZONTAL

SLIDER
1053
728
1288
761
densidadMax>50
densidadMax>50
0
100
38.2
0.01
1
ind/km²
HORIZONTAL

TEXTBOX
776
627
944
672
PARÁMETROS DESPLAZAMIENTO MONOS
12
0.0
1

SLIDER
773
662
1036
695
probQuedarseEnSelva30-50
probQuedarseEnSelva30-50
0
1
0.19
0.01
1
NIL
HORIZONTAL

PLOT
775
502
1593
622
plot 5
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"total monos" 1.0 0 -8431303 true "" "plot count monos"

SLIDER
536
608
764
641
numeroMinDeMonosParaTurismo
numeroMinDeMonosParaTurismo
1
30
15.0
1
1
NIL
HORIZONTAL

TEXTBOX
40
295
190
325
36 x 93   => 3 ha\n60 x 161 => 1 ha
10
0.0
1

MONITOR
1504
570
1587
615
total de monos
count monos
0
1
11

MONITOR
1391
448
1460
493
media bim sustento
mean [ sustentoBim ] of hogares
2
1
11

SLIDER
773
699
1035
732
probQuedarseEnSelva>50
probQuedarseEnSelva>50
0
1
0.81
0.01
1
NIL
HORIZONTAL

MONITOR
1800
327
1850
372
media BiomasaAqui
mean [ cantidadBiomasaAqui_t ] of patches
2
1
11

MONITOR
1800
379
1850
424
NIL
velViento
2
1
11

MONITOR
1801
502
1851
547
porcentaje cuadros afectados
100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))
2
1
11

SLIDER
540
167
764
200
probOcurrHuracanCat1
probOcurrHuracanCat1
0
1
0.01
0.001
1
NIL
HORIZONTAL

SLIDER
540
204
765
237
probOcurrHuracanCat2
probOcurrHuracanCat2
0
1
0.007
0.001
1
NIL
HORIZONTAL

SLIDER
540
241
764
274
probOcurrHuracanCat3
probOcurrHuracanCat3
0
1
0.01
0.001
1
NIL
HORIZONTAL

SLIDER
540
277
765
310
probOcurrHuracanCat4
probOcurrHuracanCat4
0
1
0.007
0.001
1
NIL
HORIZONTAL

SLIDER
540
131
764
164
probOcurrTorTrop
probOcurrTorTrop
0
1
0.083
0.001
1
NIL
HORIZONTAL

PLOT
1597
501
1797
623
porcentaje parches dañados
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot 100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))"

SLIDER
539
313
763
346
probOcurrHuracanCat5
probOcurrHuracanCat5
0
1
0.0
0.001
1
NIL
HORIZONTAL

MONITOR
1456
448
1525
493
media anual sustento
mean [ mediaAnoSustento ] of hogares
2
1
11

MONITOR
1523
448
1592
493
media d.e. sustento
mean [ desviacionEstandarAnoSustento ] of hogares
2
1
11

MONITOR
1801
10
1851
43
área quemada este año (ha)
areaQuemadaAno
17
1
8

MONITOR
1801
57
1851
90
area quemada agregado (ha)
totalAreaQuemada
17
1
8

MONITOR
1724
625
1796
658
% afecHurCat1
100 * (1 /(1 + exp( parametroSigmoidea1 * ( - 74\n + parametroSigmoidea2))))
2
1
8

MONITOR
1724
660
1796
693
% afecHurCat2
100 * (1 /(1 + exp( parametroSigmoidea1 * ( - 96 + parametroSigmoidea2))))
2
1
8

MONITOR
1724
695
1797
728
% afecHurCat3
100 * (1 /(1 + exp( parametroSigmoidea1 * ( - 111 + parametroSigmoidea2))))
2
1
8

MONITOR
1724
730
1796
763
% afecHurCat4
100 * (1 /(1 + exp( parametroSigmoidea1 * ( - 130 + parametroSigmoidea2))))
2
1
8

MONITOR
1801
132
1851
165
NIL
count biomasCombs
17
1
8

MONITOR
1649
625
1721
658
% afecNoTor
100 * (1 /(1 + exp( parametroSigmoidea1 * ( - 10 + parametroSigmoidea2))))
2
1
8

MONITOR
1648
661
1720
694
% afecTorTrop
100 * (1 /(1 + exp( parametroSigmoidea1 * ( - 20 + parametroSigmoidea2))))
2
1
8

MONITOR
1802
201
1853
234
cuadros con biomas
count patches with [any? biomasCombs-here ]
17
1
8

MONITOR
1802
237
1853
270
cuadros donde puede caer
count patches with [ ANP? = true and tipo != \"agua\"]
17
1
8

SWITCH
262
751
391
784
leerActividades?
leerActividades?
1
1
-1000

SWITCH
263
714
391
747
turismoT?
turismoT?
0
1
-1000

MONITOR
1802
166
1852
199
cuadros sin biomas
count patches with [ not any? biomasCombs-here and tipo = \"selva\"]
17
1
8

MONITOR
1724
765
1798
798
% afecHurCat5
100 * (1 /(1 + exp( parametroSigmoidea1 * ( - 157 + parametroSigmoidea2))))
2
1
8

TEXTBOX
1650
698
1722
766
NoTor-----> 10\nTorTrop--> 20\nCat1-------> 74\nCat2-------> 96\nCat3-------> 111\nCat4-------> 130\nCat5-------> 157
8
0.0
1

MONITOR
701
747
942
792
NIL
contadorBimestresTuristasAltoAno
17
1
11

SLIDER
1366
725
1574
758
multiplicarTormentas
multiplicarTormentas
0
30
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
1367
761
1575
794
multiplicarProbOcurIncendios
multiplicarProbOcurIncendios
0
30
1.0
0.5
1
NIL
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

Este es un modelo espacialmente explícito que simula varios procesos que ocurren el socio-ecosistema asociado al Área Natural Protegida *Otoch Ma'ax Yetel Kooh* (OMYK) en la Península de Yucatán, México. El modelo busca explorar como interactúan diversos procesos y como distintas prácticas de manejo afectan a la cobertura vegetal, a la población de monos araña que habita el sitio y a la economía de los hogares de los habitantes del sitio.

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

El modelo está formado por los siguientes 5 agentes/meta-agentes:

1. **Cuadros de vegetación**. Estos representan cuadros de 3 ha que pueden ser de los de tipo selva, milpa, quemado, hogar y agua. Los cuadros tipo selva, milpa y quemado aumentan su edad sucesional cada año. Los cuadros de tipo selva y milpa pueden acumular biomasa combustible lo que los hace suceptibles a quemarse.
2. **Hogares**. Estos representan un hogar de OMYK que puede dedicarse a la argicultura, a la apicultura, a la producción de carbón y a proveer servicios ecoturísticos. En el modelo hay disturbios como huracanes, incendios y fluctuaciones en el turismo que afectan a las diferentes actividades
3. **Monos**. Estos representan a los monos araña que habitan la reserva y que se pueden mover en los cuadros con edad sucesional mayor a 30 años. Estos nacen o mueren dependiendo del tamaño de su población local.
4. **Biomasa combustible**. Este agente representa material combustible. Estos se crean en las tormentas y huracanes y con el tiempo se van descomponiendo y desaparecen del mundo. Mientras están presentes en el mundo hacen suceptible a un cuadro a quemarse.
5. **Población local de monos**. Este representa a un conjunto de monos que comparten un mismo parche de vegetación (i.e., cuadros que comparten un mismo rango de edad sucesional (color) y que están conectados espacialmente por sus 4 vecinos). Las poblaciones locales crecen o disminuyen de acuerdo a una ecuación discreta de crecimiento logístico.

El modelo está formado por los siguientes 8 submodelos:

1. **Clima**. Simula la dinámica climática anual.
2. **Tormentas**. Simula la ocurrencia de tormentas tropicales y huracanes.
3. **Turistas**. Simula el flujo de turistas que llegan a la reserva.
4. **Actividades hogares**. Simula la realización de las actividades económicas de los hogares.
5. **Incendios**. Simula la ocurrencia de incendios.
6. **Desplazamiento monos**. Simula el movimiento de los monos en la selva.
7. **Regenereación selva**. Simula la regeneración de la selva.
8. **Dinámica poblacional monos**. Simula la dinámica poblacional de los monos araña.

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

Con el botón SETUP se crea el un mapa con las caracterísiticas de OMYK en 2003 y se crean los monos y los hogares. Con el botón GO empieza a correre el modelo. Cada iteración del modelo representa un bimestre (2 meses).

Los distintos parámetros se agrupan por el submodelo del que forman parte.

Para modificar las combinaciones de actividades que pueden realizar todos los hogares utilizar los parámetros milpaT?, apiculturaT?, carbonT?, turismoT?

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

Nótese que no se simulan las decisiones de los hogares. Estos siempre realizan las actividades que se le asignan.

Nótese que el modelo es muy sensible a los parámetros: probQuemarse, parametroSigmoidea1 y parametroSigmoidea2.

(suggested things for the user to notice while running the model)

## THINGS TO TRY

Intente simular distintas combinaciones de actividades moviendo los parámetros milpaT?, apiculturaT?, carbonT?, turismoT?

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

Se podría intentar modelar las decisiones de los hogares.


(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

El modelo utiliza los siguientes archivos que deben de estar guardados en la misma carpeta donde está el código:
1. actividadesHogares.txt (para modificar las actividades que realiza cada hogar individualmente)
2. hogares1ha.txt (para posicionar a los hogares en la inicialización)
3. hogares3ha.txt (para posicionar a los hogares en la inicialización)
4. hogares3ha2.txt (para posicionar a los hogares en la inicialización)
5. omyk_2003_r_100_a.asc (mapa con cuadros de 1 ha)
6. omyk_2003_r_173_a.asc (mapa con cuadros de 3 ha)

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

...

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES


(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

biomascomb
true
0
Polygon -6459832 true false 30 180 15 195 60 210 30 225 30 240 75 225 180 270 180 240 30 180 15 195
Polygon -6459832 true false 30 135 210 45 225 15 30 135
Polygon -6459832 true false 270 210 105 150 135 180 315 240

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

mono
false
3
Polygon -6459832 true true 135 135 195 135 225 135 225 150 225 165 210 150 165 150 105 150 135 135
Polygon -6459832 true true 195 30 165 30 135 45 120 75 135 120 180 120 210 105 210 75 210 60 195 30
Polygon -7500403 true false 165 90 120 225
Polygon -6459832 true true 150 210 210 180 225 225 225 240 255 255 225 255 210 240 195 210 165 225 135 225 150 210
Polygon -6459832 true true 105 255 45 270 15 240 15 195 15 105 30 75 30 225 45 255 90 240 120 225 120 240
Polygon -6459832 true true 120 225 180 210 195 255 195 270 225 285 195 285 180 270 165 240 135 255 105 240 105 225
Polygon -6459832 true true 90 165 150 165 180 165 180 180 180 195 165 180 120 180 105 180 90 165
Polygon -6459832 true true 135 105 90 120 75 180 90 210 105 240 150 240 180 225 150 180 150 150 180 105 135 75 150 90 90 120
Circle -7500403 true false 165 75 0
Polygon -2064490 true false 180 60 150 75 165 105 180 120 195 105 210 75 180 60

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment-calibracion1" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.02"/>
      <value value="0.025"/>
      <value value="0.03"/>
      <value value="0.035"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="18"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracion2" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.088"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
      <value value="0.05"/>
      <value value="0.075"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sensibilidad-sin-calibrados" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sensibilidad-con-calibrados" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.088"/>
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
      <value value="0.05"/>
      <value value="0.075"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sensibilidad-tamanoCuadro" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sensibilidad-probTuristasAltoBim" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.026"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioForrajeoAbejas">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sensibilidad-desviacionEstandarDuracionBiomasComb" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sensibilidad-probQuemarse" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sensibilidad-parametroSigmoidea1" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
      <value value="0.05"/>
      <value value="0.075"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sensibilidad-parametroSigmoidea2" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sensibilidad-mediaDuracionBiomasComb" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracion1parte2" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.1"/>
      <value value="0.075"/>
      <value value="0.05"/>
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracion1parte3" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.1"/>
      <value value="0.075"/>
      <value value="0.05"/>
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracionTOTAL" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.088"/>
      <value value="0.368"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
      <value value="0.05"/>
      <value value="0.075"/>
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracion3" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
      <value value="0.03"/>
      <value value="0.035"/>
      <value value="0.04"/>
      <value value="0.045"/>
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracion4" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" or tipo = "milpaNueva" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ jovenMilpa_t? = true ]</metric>
    <metric>count hogares with [ adultaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ colmenas_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.105"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.035"/>
      <value value="0.04"/>
      <value value="0.045"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;aleatoria_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.08"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.013"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.003"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracion5" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.026"/>
      <value value="0.027"/>
      <value value="0.028"/>
      <value value="0.029"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioForrajeoAbejas">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracionsensi" repetitions="30" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.027"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioForrajeoAbejas">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracion6" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
      <value value="0.56"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.026"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="bimestresAltoParaAfectarMonos">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="16"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioForrajeoAbejas">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="r-max">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-base" repetitions="0" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-calibracion" repetitions="100" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.02"/>
      <value value="0.025"/>
      <value value="0.03"/>
      <value value="0.035"/>
      <value value="0.04"/>
      <value value="0.045"/>
      <value value="0.05"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sens-tamanoCuadro" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sens-parametroSigmoidea1" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.05"/>
      <value value="0.045"/>
      <value value="0.04"/>
      <value value="0.035"/>
      <value value="0.03"/>
      <value value="0.025"/>
      <value value="0.02"/>
      <value value="0.015"/>
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sens-parametroSigmoidea2" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
      <value value="96"/>
      <value value="111"/>
      <value value="130"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sens-probQuemarse" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.56"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sens-mediaDuracionBiomasaComb" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sens-desviacionEstandarDuracionBiomasComb" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="1"/>
      <value value="3"/>
      <value value="6"/>
      <value value="9"/>
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sens-probTuristasAltoBim" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0"/>
      <value value="0.1"/>
      <value value="0.2"/>
      <value value="0.3"/>
      <value value="0.4"/>
      <value value="0.5"/>
      <value value="0.6"/>
      <value value="0.7"/>
      <value value="0.8"/>
      <value value="0.9"/>
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-sens-numeroMinDeMonosParaTurismo" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1200"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="10"/>
      <value value="15"/>
      <value value="20"/>
      <value value="25"/>
      <value value="30"/>
      <value value="35"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-aumentoOcurrHuracanes" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-modifProbFlujoAltoTuristas" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.3"/>
      <value value="0.5"/>
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-aumentoProbOcurIncendios" repetitions="30" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
      <value value="3"/>
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-validacion" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="72"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experiment-casos" repetitions="100" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="300"/>
    <metric>count patches with [ tipo = "milpa" ]</metric>
    <metric>count patches with [ tipo = "quemado" ]</metric>
    <metric>count patches with [ edadSucesional &lt; 2 ]</metric>
    <metric>count patches with [ edadSucesional &lt; 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 2  and edadSucesional &lt;= 15 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 16 and edadSucesional &lt;= 29 ]</metric>
    <metric>count patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count patches with [ edadSucesional &gt; 50 ]</metric>
    <metric>mean [ sustentoBim ] of hogares</metric>
    <metric>mean [ mediaAnoSustento ] of hogares</metric>
    <metric>mean [ desviacionEstandarAnoSustento ] of hogares</metric>
    <metric>count monos-on patches with [ edadSucesional &gt;= 30 and edadSucesional &lt;= 50 ]</metric>
    <metric>count monos-on patches with [ edadSucesional  &gt; 50 ]</metric>
    <metric>count monos</metric>
    <metric>precipitacion_t?</metric>
    <metric>huracan?</metric>
    <metric>incendio?</metric>
    <metric>velViento</metric>
    <metric>turistas_t?</metric>
    <metric>turistasA_t?</metric>
    <metric>count hogares with [ abrirMilpa_t? = true ]</metric>
    <metric>count hogares with [ sembrarMilpa_t? = true ]</metric>
    <metric>count hogares with [ milpaJoven_t? = true ]</metric>
    <metric>count hogares with [ milpaAdulta_t? = true ]</metric>
    <metric>count hogares with [ cosechaMilpa_t? = true ]</metric>
    <metric>count hogares with [ cosechaApicultura_t? = true ]</metric>
    <metric>count hogares with [ produccionCarbon_t? = true ]</metric>
    <metric>mean [ duracionBiomasComb ] of biomasCombs</metric>
    <metric>mean [ count biomasCombs-here ] of patches</metric>
    <metric>count biomasCombs</metric>
    <metric>100 * (1 /(1 + exp( parametroSigmoidea1 * ( - velViento + parametroSigmoidea2))))</metric>
    <metric>areaQuemadaAno</metric>
    <metric>totalAreaQuemada</metric>
    <enumeratedValueSet variable="parametroSigmoidea1">
      <value value="0.025"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="parametroSigmoidea2">
      <value value="74"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuemarse">
      <value value="0.5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="mediaDuracionBiomasComb">
      <value value="12"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="desviacionEstandarDuracionBiomasComb">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probTuristasAltoBim">
      <value value="0.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroMinDeMonosParaTurismo">
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="milpaT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="carbonT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="apiculturaT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turismoT?">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="numeroDeHogares">
      <value value="28"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="soloANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="temporalidadTuristas">
      <value value="&quot;2_bimestres_alta_prob&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="turistas?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarEnSelva&gt;50?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="cultivarDentroDeANP?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="leerActividades?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrenciaIncendioBimSecas">
      <value value="0.093"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarProbOcurIncendios">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrTorTrop">
      <value value="0.083"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat1">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat2">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat3">
      <value value="0.01"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat4">
      <value value="0.007"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOcurrHuracanCat5">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="multiplicarTormentas">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax30-50">
      <value value="5.7"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densidadMax&gt;50">
      <value value="38.2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="R">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva30-50">
      <value value="0.19"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probQuedarseEnSelva&gt;50">
      <value value="0.81"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="tiempoUsoMismaParcela">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="areaDeCultivo">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="radioActividades">
      <value value="3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="periodoMinimoDeBarbecho">
      <value value="5"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
