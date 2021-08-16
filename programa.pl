% a) Modelando Vocaloids
vocaloid(megurineLuka,cancion(nightFever,4)).
vocaloid(hatsuneMiku,cancion(tellYourWorld,4)).
vocaloid(gumi,cancion(foreverYoung,4)).
vocaloid(gumi,cancion(tellYourWorld,5)).
vocaloid(seeU,cancion(novemberRain,6)).
vocaloid(seeU,cancion(nightFever,5)).
esVocaloid(Vocaloid):- vocaloid(Vocaloid,_).

% 1)
esNovedoso(Vocaloid):-
    sabeAlMenosDosCanciones(Vocaloid),
    tiempoTotalDeCanciones(Vocaloid,TiempoTotal),
    TiempoTotal < 15.

sabeAlMenosTresCanciones(Vocaloid):-
    sabeCuantasCanciones(Vocaloid,Cuantas),
    Cuantas >= 3.

sabeAlMenosDosCanciones(Vocaloid):-
    sabeCuantasCanciones(Vocaloid,Cuantas),
    Cuantas >= 2.

sabeCuantasCanciones(Vocaloid,CantidadCanciones):-
    esVocaloid(Vocaloid),
    findall(Cancion,vocaloid(Vocaloid,cancion(Cancion,_)),Canciones),
    list_to_set(Canciones,CancionesFiltradas),
    length(CancionesFiltradas, CantidadCanciones).
    
tiempoTotalDeCanciones(Vocaloid,TiempoTotal):-
    esVocaloid(Vocaloid),
    findall(Tiempo,vocaloid(Vocaloid,cancion(_,Tiempo)),Tiempos),
    sumlist(Tiempos,TiempoTotal).

%2)
cantanteAcelerado(Vocaloid):-
    esVocaloid(Vocaloid),
    not(cantaCancionLarga(Vocaloid)).
    
cantaCancionLarga(Vocaloid):-
    vocaloid(Vocaloid,cancion(_,Duracion)),
    Duracion > 4.

% b) Modelando conciertos
% 1)
concierto(mikuExpo,estadosUnidos,2000,gigante(2,6)).
concierto(magicalMirai,japon,3000,gigante(3,10)).
concierto(vocalekt,estadosUnidos,1000,mediano(9)).
concierto(mikuFest,argentina,100,pequenio(4)).

%2)
puedeParticiparEn(Concierto,hatsuneMiku):- concierto(Concierto,_,_,_).

puedeParticiparEn(Concierto,Vocaloid):-
    esVocaloid(Vocaloid),
    Vocaloid \= hatsuneMiku,
    concierto(Concierto,_,_,Requisitos),
    cumpleRequistosConcierto(Vocaloid,Requisitos).

cumpleRequistosConcierto(Vocaloid,gigante(CantCancionesMin,DuracionMinima)):-
    sabeCuantasCanciones(Vocaloid,Cantidad),
    Cantidad >= CantCancionesMin,
    tiempoTotalDeCanciones(Vocaloid,TiempoTotal),
    TiempoTotal >= DuracionMinima.

cumpleRequistosConcierto(Vocaloid,mediano(DuracionMaxima)):-
    tiempoTotalDeCanciones(Vocaloid,TiempoTotal),
    DuracionMaxima >= TiempoTotal.

cumpleRequistosConcierto(Vocaloid,pequenio(CantidadTiempoMin)):-
    vocaloid(Vocaloid,cancion(_,Duracion)),
    Duracion >= CantidadTiempoMin.

% 3)
masFamoso(Vocaloid):-
    nivelFama(Vocaloid,Nivel),
    forall(nivelFama(_,OtroNivel),
        Nivel >= OtroNivel).

nivelFama(Vocaloid,Nivel):-
    sabeCuantasCanciones(Vocaloid,CuantasCanciones),
    famaTotalDeConciertos(Vocaloid,FamaTotal),
    Nivel is (FamaTotal*CuantasCanciones).

famaTotalDeConciertos(Vocaloid,FamaTotal):-
    esVocaloid(Vocaloid),
    findall(Fama,famaPorConcierto(Vocaloid,_,Fama),Famas),
    list_to_set(Famas,FamasFilt),
    sumlist(FamasFilt,FamaTotal).

famaPorConcierto(Vocaloid,Concierto,Fama):-
    puedeParticiparEn(Concierto,Vocaloid),
    concierto(Concierto,_,Fama,_).

% 4) -> Conoce tiene que hacerse con revursividad debido a que se pueden conocer directa o indirectamente
conoce(megurineLuka,hatsuneMiku).
conoce(megurineLuka,gumi).
conoce(gumi,seeU).
conoce(seeU,kaito).
conoce(tito,lean). % -> Caso Prueba

conocidoTotal(Vocaloid1,Vocaloid2):-
    conoce(Vocaloid1,Vocaloid2).

conocidoTotal(Vocaloid1,Vocaloid2):-
    conoce(Vocaloid1,Vocaloid3),
    conocidoTotal(Vocaloid3,Vocaloid2).

unicoEnConcierto(Vocaloid):-
    puedeParticiparEn(Concierto,Vocaloid),
    forall(conocidoTotal(Vocaloid,OtroVocaloid),
        not(puedeParticiparEn(Concierto,OtroVocaloid))).

% 5) Si aparece un nuevo concierto solo habria que declararlo en el
% predicado concierto y sus especificacion no seran necesarias ya que 
% gracias al polimorfismo (tratando adecuadamente los functores),
% hay un modelo generico para cada tipo de concierto.






    

    






