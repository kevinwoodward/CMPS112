% $Id: functions.pl,v 1.3 2016-11-08 15:04:13-08 - - $

mathfns( X, List ) :-
   S is sin( X ),
   C is cos( X ),
   Q is sqrt( X ),
   List = [S, C, Q].

constants( List ) :-
   Pi is pi,
   E is e,
   Epsilon is epsilon,
   List = [Pi, E, Epsilon].

sincos( X, Y ) :-
   Y is sin( X ) ** 2 + cos( X ) ** 2.

get_radians(IATA, RadLat, RadLon):-
  airport(IATA, _, degmin(A, B), degmin(C, D)),
  DLatmin is B/60 ,
  DLonmin is D/60 ,
  DegreeLat is A + DLatmin,
  DegreeLon is C + DLonmin,
  RadLat is DegreeLat * pi/180,
  RadLon is DegreeLon * pi/180.

haversine_radians( Lat1, Lon1, Lat2, Lon2, Distance ) :-
   Dlon is Lon2 - Lon1,
   Dlat is Lat2 - Lat1,
   A is sin( Dlat / 2 ) ** 2
      + cos( Lat1 ) * cos( Lat2 ) * sin( Dlon / 2 ) ** 2,
   Dist is 2 * atan2( sqrt( A ), sqrt( 1 - A )),
   Distance is Dist * 3961.
   not( X ) :- X, !, fail.
   not( _ ).
haversine_cities(IATA1, IATA2, Distance):-
  get_radians(IATA1, Lat1, Lon1),
  get_radians(IATA2, Lat2, Lon2),
  haversine_radians(Lat1, Lon1, Lat2, Lon2, Distance).


   %
   % Is there a path from one node to another?
   %
   writeallpaths( Node, Node ) :-
      write( Node ), write( ' is ' ), write( Node ), nl.
   writeallpaths( Node, Next ) :-
      listpath( Node, Next, [Node], List ),
      write( Node ), write( ' to ' ), write( Next ), write( ' is ' ),
      writepath( List ).

   writepath( [] ) :-
      nl.
   writepath( [Head|Tail] ) :-
      write( ' ' ), write( Head ), writepath( Tail ).

   listpath( Node, End, Outlist ) :-
      listpath( Node, End, [Node], Outlist ).

   listpath( Node, Node, _, [Node] ).
   listpath( Node, End, Tried, [Node|List] ) :-
      flight( Node, Next, _ ),
      not( member( Next, Tried )),
      listpath( Next, End, [Next|Tried], List ).


biggest( Number ) :- mynumber( Number ), nobigger( Number ).
nobigger( Number ) :- mynumber( Other ), Other > Number, !, fail.
nobigger( _ ).
 %converts degrees to rads
flightduration(Distance, Time):-
  Time is (Distance/500).



fly(Source, Destination):-
  airport(Source, _, _, _),
  airport(Destination, _, _, _),
  writeallpaths(Source, Destination).
