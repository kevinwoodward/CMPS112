% Kevin Woodward, keawoodw@ucsc.edu
% Megan Sharp, mesharp@ucsc.edu

not( X ) :- X, !, fail.
not( _ ).

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

haversine_cities(IATA1, IATA2, Distance):-
    get_radians(IATA1, Lat1, Lon1),
    get_radians(IATA2, Lat2, Lon2),
    haversine_radians(Lat1, Lon1, Lat2, Lon2, Distance).

% Capital names for airports
uppercaseairport( 'atl', 'ATL').
uppercaseairport( 'bos', 'BOS').
uppercaseairport( 'chi', 'CHI').
uppercaseairport( 'den', 'DEN').
uppercaseairport( 'dfw', 'DFW').
uppercaseairport( 'lax', 'LAX').
uppercaseairport( 'mia', 'MIA').
uppercaseairport( 'nyc', 'NYC').
uppercaseairport( 'sea', 'SEA').
uppercaseairport( 'sfo', 'SFO').
uppercaseairport( 'sjc', 'SJC').

flightpossible(_, _, AfterHours, AfterMinutes) :-
    AfterTime is (AfterHours * 60 ) + AfterMinutes,
    AfterTime >= 1440,
    !, fail.

flightpossible(BeforeHours, BeforeMinutes, AfterHours, AfterMinutes) :-
    BeforeTime is (BeforeHours * 60 ) + BeforeMinutes,
    AfterTime is (AfterHours * 60 ) + AfterMinutes,
    AfterTime >= (BeforeTime + 30).

combinetimes(H1, M1, H2, M2, R1, R2) :-
    T is ((H1 + H2) * 60) + (M1 + M2),
    R1 is T // 60,
    R2 is T mod 60.

writeallpaths( Node, Next ) :-
    listpath( Node, Next, time(0, 0), [Node], List, Times ),
    writepath( List, Times ).

writepath( [_|[]], _) :-
  nl.

writepath( [LocHead|LocTail], [time(Hours, Minutes)|TimeTail]) :-
    LocTail = [LocNext|_],
    TimeTail = [time(NextHours, NextMinutes)|TimesToPass],
    airport(LocHead, From, _, _),
    airport(LocNext, To, _, _),
    uppercaseairport(LocHead, UpperLocHead),
    uppercaseairport(LocNext, UpperLocNext),
    %Format: depart  <iata>  <location> <time>
    format('depart  %s  %s %02d:%02d\n',
        [UpperLocHead, From, Hours, Minutes]),
    format('arrive  %s  %s %02d:%02d\n',
        [UpperLocNext, To, NextHours, NextMinutes]),
    writepath( LocTail, TimesToPass  ).


%listpath( Node, Node, _, _, [Node|List], _ ). % Done case, old
listpath( Node, Node, _, _, [Node|_], _ ). % Done case

listpath( Node, End,
    time(PrevHours, PrevMins), Tried, [Node|List],
    [time(DepHours, DepMinutes),time(NewHours, NewMinutes)|Rest] ) :-

    flight( Node, Next, time(DepHours, DepMinutes) ),

    not( member( Next, Tried )),
    haversine_cities(Node, Next, Distance),
    ArrivalHours is floor(Distance / 500),
    ArrivalMinutes is round(((Distance / 500) - ArrivalHours) * 60),
    combinetimes(DepHours, DepMinutes,
                 ArrivalHours, ArrivalMinutes,
                 NewHours, NewMinutes),
    flightpossible(PrevHours, PrevMins, DepHours, DepMinutes),

    listpath( Next, End, time(NewHours, NewMinutes),
              [Next|Tried], List, Rest ).

fly(Source, Source) :-
    write('Error: Source and destination cannot be identical!\n'),
    !, fail.

fly(Source, Destination):-
    airport(Source, _, _, _),
    airport(Destination, _, _, _),
    writeallpaths(Source, Destination).

fly(Source, Destination):-
    airport(Source, _, _, _),
    airport(Destination, _, _, _),
    not(writeallpaths(Source, Destination)),
    write('Path does not exist!\n'),
    !, fail.

fly(_, _) :-
    write('Error: Source or destination does not exist!\n'),
    !, fail.
