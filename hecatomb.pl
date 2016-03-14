%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                  %
% Hecatomb                         %
%                                  %
%                                  %
%                                  %
% Author : Francisco Couto         %
%          Joel Dinis              %
%                                  %
% ?- menu.                         %
% to start the game                %
%                                  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/* -*- Mode:Prolog; coding:iso-8859-1; -*- */
:-use_module(library(random)). /*used to randomize which player starts*/
:-use_module(library(lists)).
:-use_module(library(system)). 

%Initializes Board (8x8) $-> green soldier, '  '->void, &-> blue soldier, +->green king, *->blue king
board(B):-
    B=[
        [' $ ',' $ ',' $ ',' $ ',' + ',' $ ',' $ ',' $ '],
        [' $ ',' $ ',' $ ',' $ ',' $ ',' $ ',' $ ',' $ '],
        [' $ ',' $ ',' $ ',' $ ',' $ ',' $ ',' $ ',' $ '],
        [' $ ',' $ ',' $ ',' $ ',' $ ',' $ ',' $ ',' $ '],
        [' & ',' & ',' & ',' & ',' & ',' & ',' & ',' & '],
        [' & ',' & ',' & ',' & ',' & ',' & ',' & ',' & '],
        [' & ',' & ',' & ',' & ',' & ',' & ',' & ',' & '],
        [' & ',' & ',' & ',' & ',' * ',' & ',' & ',' & ']
        ].

%Allow the players to visualize the initial board and plan their moves        
heca:-board(B),nl, printB(B).

%Auxiliar function to print the board in a user friendly way
printB(H):- 
        write('   0   1   2   3   4   5   6   7'), nl,
        printB(H, 0).
    
printB([], _).

printB([H|T], Line):-
        write(Line), write(' '), printL(H), nl,
        NLine is Line+1, 
        printB(T, NLine). 
        
printL([]).

printL([H|T]):-
        write(H),write('|'), printL(T).
   
%Initial Menu Predicate, Lets the player choose the game mode
menu:-
write('*------------------------*'),nl,
write('*  WELCOME TO HECATOMB  !*'),nl,
write('*------------------------*'),nl,
write('Escolha um modo de jogo: pvp/pvb/bvb'),nl,nl,
read(Choice),gameMode(Choice),nl.

%Game choice menu settings preparation
gameMode(pvp):-write('Prepare to face another player!'),nl,nl, play(pvp).

gameMode(pvb):-write('Prepare to face the computer!'),nl,nl, 
        menuDificuldade(D),
        (D == 'f' ->play('hbf'); 
        play('hbd')).

gameMode(bvb):-write('Computer against itself!'),nl,nl,
        menuDificuldade(D),
        (D == 'f' ->play(bvb,'bbf'); 
        play(bvb,'bbd')).

gameMode(_):-write('Wrong option, choose a new one!'), nl, read(Choice), gameMode(Choice),nl.


%Launch the game in the desired move and difficulty
play(pvp):-
    random(1,3, Player), write('Player number '), 
    write(Player), write(' will start the game!'),nl,
    board(B),printB(B),pvpgame(Player, B, 10).

play(hbf):-random(1,3, Player), 
    ((Player == 1)->write('Human starts playing!'); 
    write('Bot starts playing!')),
    nl,board(B),printB(B), 
    pvbgame(Player, B, 10, hbf).

play(hbd):-random(1,3, Player), 
    ((Player == 1)->write('Human starts playing!'); 
    write('Bot starts playing!')),
    nl,board(B),printB(B),
    pvbgame(Player, B, 10, hbd).

play(bvb,Dif):-random(1,3, Player),write('Player number '), 
    write(Player), write(' will start the game!'),nl,
    board(B),printB(B),
    bvbgame(Player, B, 10,Dif).

%Checks if the game is over
pvpgame(_,Board,_):-            
        game_over(Board).

%Checks if the game is a draw
pvpgame(_,Board,0):-gameDraw(Board).

%Game Engine for a player versus player game
pvpgame(P,Board,Turns):-
    Turns > 0, 
    Turns2 is Turns-1,
    printPlayer(P,h), 
    movement(Xi,Yi,Xf,Yf,P,Board),
    getPiece(Xi,Yi,Board, Element),
    replace(Board, Xf, Yf, Element, B2), 
    replace(B2, Xi, Yi, '   ', B3), printB(B3),changePlayer(P,Novo),pvpgame(Novo,B3,Turns2).

%check if we shall stop the game now and not continue to the next play
pvbgame(_,Board,_,_):-            
        game_over(Board).

%the game reached 10 turns
pvbgame(_,Board,0,_):-gameDraw(Board).

%Game Engine for a player versus bot game - bot is always the second player (2)    
pvbgame(P,Board,Turns,Dif):-
    Turns > 0,
    Turns2 is Turns-1,
    printPlayer(P,h),
    ((P == 1)->movement(Xi,Yi,Xf,Yf,P,Board);
    ((Dif == hbf)->getRandomMove(Board,2,_,Xi,Yi,Xf,Yf);
    getBestMove(Board,2,_,Xi,Yi,Xf,Yf))),
    printPlay(P,Xf,Yf),
    getPiece(Xi,Yi,Board,Element),
    replace(Board, Xf, Yf, Element, B2), 
    replace(B2, Xi, Yi, '   ', B3), printB(B3),sleep(1),changePlayer(P,Novo), pvbgame(Novo,B3,Turns2,Dif).

bvbgame(_,Board,_,_):-            
        game_over(Board).

bvbgame(_,Board,0,_):-gameDraw(Board).

%Game Engine for a bot versus bot game    
bvbgame(P,Board,Turns,Dif):-
    Turns > 0,
    Turns2 is Turns-1,
    printPlayer(P,h),
    ((Dif == bbf)->getRandomMove(Board,P,_,Xi,Yi,Xf,Yf);
    getBestMove(Board,P,_,Xi,Yi,Xf,Yf)),
    getPiece(Xi,Yi,Board,Element),
    replace(Board, Xf, Yf, Element, B2), 
    replace(B2, Xi, Yi, '   ', B3), printB(B3),
    printPlay(P,Xf,Yf),
    sleep(2),
    changePlayer(P,Novo),
    bvbgame(Novo,B3,Turns2,Dif).

%Predicate that allows the replacement of a piece in the desired position of the matrix     
replace( L , Y , X , Z , R ) :-
  append(RowPfx,[Row|RowSfx],L),     % decompose the list-of-lists into a prefix, a list and a suffix
  length(RowPfx,X) ,                 % check the prefix length: do we have the desired list?
  append(ColPfx,[_|ColSfx],Row) ,    % decompose that row into a prefix, a column and a suffix
  length(ColPfx,Y) ,                 % check the prefix length: do we have the desired column?
  append(ColPfx,[Z|ColSfx],RowNew) , % if so, replace the column with its new value
  append(RowPfx,[RowNew|RowSfx],R).   % and assemble the transformed list-of-lists
  
% Get Piece From [X,Y] of matrix
getPiece(X,Y,Board,Element):-
       nth0(Y,Board,Elem),nth0(X,Elem,Element).       

%Retrieves input from the player about his desired move       
getPlayerMovement(Xi,Yi,Xf,Yf) :-           
        print('Coordenada X da peça a mover : '), read(Xi),
        print('Coordenada Y da peça a mover : '), read(Yi),
        print('Coordenada X da casa destino : '), read(Xf),
        print('Coordenada Y da casa destino : '), read(Yf).

%Checks if the input coordinates are inside the board        
checkOutOfBounds(Xi,Yi,Xf,Yf) :- 
    (Xf > -1, Xi > -1, 
    Xf < 8, Xi < 8,
    Yf > -1, Yi > -1,
    Yf < 8, Yi < 8).

% Checks if the input coordinates are different              
positionsDifferent(Xi,Yi,Xf,Yf) :-
        (Xi \= Xf; Yi \= Yf).

%Check if for each player the pieces involved in the play are correct
checkCorrectPiece(1,Board,Xi,Yi,Xf,Yf):-getPiece(Xi,Yi,Board,InP), getPiece(Xf,Yf,Board,FnP),
    ((InP == ' $ '; InP == ' + '),(FnP == ' & '; FnP == ' * '; FnP == '   '));
    (write('You can only play YOUR pieces and eat your oponent´s!'),nl,fail).
    
checkCorrectPiece(2,Board,Xi,Yi,Xf,Yf):-getPiece(Xi,Yi,Board,InP), getPiece(Xf,Yf,Board,FnP),
    ((InP == ' & '; InP == ' * '),(FnP == ' $ '; FnP == ' + '; FnP == '   '));
    (write('You can only play YOUR pieces and eat your oponent´s!'),nl,fail).
    
checkMovement(Xi,Yi,Xf,Yf,Board):-
    getPiece(Xi,Yi,Board,P), checkPlay(Xi,Yi,Xf,Yf,P,D), checkPath(Xi,Yi,Xf,Yf,P,D,Board,0).

%Checks the if a piece movement is valid either diagonaly, horizontaly or verticaly   
checkPlay(Xi,Yi,Xf,Yf,' $ ',D):-
    (Xi == Xf, Yi \= Yf)->(D = 'V');
    (Xi \= Xf, Yi == Yf)->(D = 'H');
    (abs(Xi - Xf) =:= abs(Yi - Yf))->(D = 'D');
    (write('Soldiers can only move orthogonaly or diagonaly!'),nl,fail).
    
checkPlay(Xi,Yi,Xf,Yf,' & ',D):-
    (Xi == Xf, Yi \= Yf)->(D = 'V');
    (Xi \= Xf, Yi == Yf)->(D = 'H');
    (abs(Xi - Xf) =:= abs(Yi - Yf))->(D = 'D');
    (write('Soldiers can only move orthogonaly or diagonaly!'),nl,fail).
    
checkPlay(Xi,Yi,Xf,Yf,' + ',_):-
    ((Xi == Xf, (Yi == (Yf-1); Yi == (Yf+1)));
    (((Xi == (Xf-1); Xi == Xf+1)), Yi == Yf);
    ((abs(Xi - Xf) =:= abs(Yi - Yf)),abs(Xi - Xf) =:= 1));
    (write('Kings can only move orthogonaly or diagonaly to an adjacent cell!'),nl,fail).
    
checkPlay(Xi,Yi,Xf,Yf,' * ',_):-
    ((Xi == Xf, (Yi == (Yf-1); Yi == (Yf+1)));
    (((Xi == (Xf-1); Xi == Xf+1)), Yi == Yf);
    ((abs(Xi - Xf) =:= abs(Yi - Yf)),abs(Xi - Xf) =:= 1));
    (write('Kings can only move orthogonaly or diagonaly to an adjacent cell!'),nl,fail).

%Checks if there are no obstacles between the initial and final positions of the play-vertically,horizontaly and diagonaly
checkPath(Xi,Yi,Xf,Yf,P,'V',Board,N):-
    (N > 1)->fail;
    Yi \= Yf,
    (Yi < Yf)->( 
    Y2 is Yi + 1,
    getPiece(Xi,Y2,Board,Piece),
    ((Piece == '   ')->(N2 is N, checkPath(Xi,Y2,Xf,Yf,P,'V',Board,N));
    (Piece \= P)->(N2 is N + 1, checkPath(Xi,Y2,Xf,Yf,P,'V',Board,N2));
    fail)
    );
    (Yi > Yf)->( 
    Y2 is Yi - 1,
    getPiece(Xi,Y2,Board,Piece),
    ((Piece == '   ')->(N2 is N, checkPath(Xi,Y2,Xf,Yf,P,'V',Board,N));
    (Piece \= P)->(N2 is N + 1, checkPath(Xi,Y2,Xf,Yf,P,'V',Board,N2));
    fail)
    );
    (write('Clear Path'),nl).

checkPath(Xi,Yi,Xf,Yf,P,'H',Board,N):-
    (N > 1)->fail;
    Xi \= Xf,
    (Xi < Xf)->( 
    X2 is Xi + 1,
    getPiece(X2,Yi,Board,Piece),
    ((Piece == '   ')->(N2 is N, checkPath(X2,Yi,Xf,Yf,P,'H',Board,N));
    (Piece \= P)->(N2 is N + 1, checkPath(X2,Yi,Xf,Yf,P,'H',Board,N2));
    fail)
    );
    (Xi > Xf)->( 
    X2 is Xi - 1,
    getPiece(X2,Yi,Board,Piece),
    ((Piece == '   ')->(N2 is N, checkPath(X2,Yi,Xf,Yf,P,'H',Board,N));
    (Piece \= P)->(N2 is N + 1, checkPath(X2,Yi,Xf,Yf,P,'H',Board,N2));
    fail)
    );
    (write('Clear Path'),nl).

checkPath(Xi,Yi,Xf,Yf,P,'D',Board,N):-
    (N > 1)->fail;
    Yi \= Yf,
    Xi \= Xf,
    (Yi < Yf,Xi < Xf)->(
    Y2 is Yi + 1,
    X2 is Xi + 1,
    getPiece(X2,Y2,Board,Piece),
    ((Piece == '   ')->(N2 is N, checkPath(X2,Y2,Xf,Yf,P,'D',Board,N));
    (Piece \= P)->(N2 is N + 1, checkPath(X2,Y2,Xf,Yf,P,'D',Board,N2));
    fail)
    );
    (Yi > Yf, Xi > Xf)->(
    Y2 is Yi - 1,
    X2 is Xi - 1,
    getPiece(X2,Y2,Board,Piece),
    ((Piece == '   ')->(N2 is N, checkPath(X2,Y2,Xf,Yf,P,'D',Board,N));
    (Piece \= P)->(N2 is N + 1, checkPath(X2,Y2,Xf,Yf,P,'D',Board,N2));
    fail)
    );
    (Yi > Yf, Xi < Xf)->(
    Y2 is Yi - 1,
    X2 is Xi + 1,
    getPiece(X2,Y2,Board,Piece),
    ((Piece == '   ')->(N2 is N, checkPath(X2,Y2,Xf,Yf,P,'D',Board,N));
    (Piece \= P)->(N2 is N + 1, checkPath(X2,Y2,Xf,Yf,P,'D',Board,N2));
    fail)
    );
    (Yi < Yf, Xi > Xf)->(
    Y2 is Yi + 1,
    X2 is Xi - 1,
    getPiece(X2,Y2,Board,Piece),
    ((Piece == '   ')->(N2 is N, checkPath(X2,Y2,Xf,Yf,P,'D',Board,N));
    (Piece \= P)->(N2 is N + 1, checkPath(X2,Y2,Xf,Yf,P,'D',Board,N2));
    fail)
    );
    (write('Clear Path'),nl).    

%Loop that must check each and every step of a play before it proceeds
movePiece(Xi,Yi,Xf,Yf,Player,Board):- 
         repeat,
         getPlayerMovement(Xi,Yi,Xf,Yf),
         positionsDifferent(Xi,Yi,Xf,Yf),
         checkOutOfBounds(Xi,Yi,Xf,Yf),
         checkCorrectPiece(Player,Board,Xi,Yi,Xf,Yf),
         checkMovement(Xi,Yi,Xf,Yf,Board).

movement(Xi,Yi,Xf,Yf,Player,Board):-movePiece(Xi,Yi,Xf,Yf,Player,Board),!.

%retrieve all the possible movements to list
listAllPossibleMoves(Board,Player,Value,List):-
    findall(X-Y-XF-YF-Value,
    allPossibleMoves(Board,Player,Value,X,Y,XF,YF),List).

%Tests if the Move can be done
testMoves(Board,Value,XIndex,YIndex,XFIndex,YFIndex):-
    write('\33\[2J'),
    checkMovement(XIndex,YIndex,XFIndex,YFIndex,Board),
    checkValue(XIndex,YIndex,XFIndex,YFIndex,Value).                                                         

%Generate and Test all movements                                                                                                                                                          
allPossibleMoves(Board,2,Value,X,Y,XF,YF):- 
    (getPiece(XIndex,YIndex,Board,' & '); getPiece(XIndex,YIndex,Board,' * ')),
    (getPiece(XFIndex,YFIndex,Board,' $ ');getPiece(XFIndex,YFIndex,Board,' + ')),
    testMoves(Board,Value,XIndex,YIndex,XFIndex,YFIndex),
    X is XIndex,Y is YIndex, XF is XFIndex, YF is YFIndex.

allPossibleMoves(Board,1,Value,X,Y,XF,YF):- 
    (getPiece(XIndex,YIndex,Board,' $ '); getPiece(XIndex,YIndex,Board,' + ')),
    (getPiece(XFIndex,YFIndex,Board,' & ');getPiece(XFIndex,YFIndex,Board,' * ')),
    testMoves(Board,Value,XIndex,YIndex,XFIndex,YFIndex),
    X is XIndex,Y is YIndex, XF is XFIndex, YF is YFIndex.

%Auxiliar function to get a play for the easy bot move (random)
getRandomMove(Board,Player,Value,X,Y,XF,YF):-
    listAllPossibleMoves(Board,Player,Value,List),
    random_member(X-Y-XF-YF-Value,List).

%Auxiliar function to get a play for the hard bot move (per value)
getBestMove(Board,Player,Value,X,Y,XF,YF):-
    listAllPossibleMoves(Board,Player,Value,List),
    chooseBestMove(List,X-Y-XF-YF-Value).

%retrieves the best play
chooseBestMove([PH|PT], BestMove):-
        maxList(PT, PH, BestMove).

%recursivly checks for the best value play
maxList([], Max, Max).
maxList([Xi-Yi-Xf-Yf-V|T], CXi-CYi-CXf-CYf-Cv, Max) :- 
        ( V > Cv -> maxList(T, Xi-Yi-Xf-Yf-V, Max)
        ; maxList(T, CXi-CYi-CXf-CYf-Cv, Max) ).

%calculates the value of a certain play from (XY,YI) to (XF,YF)
checkValue(XIndex, YIndex, XFIndex,YFIndex,Value):-
    Xv is XFIndex - XIndex,
    Yv is YFIndex - YIndex,
    Value is round(sqrt(exp(Xv,2) + exp(Yv,2))).

% Changes the player each turn
changePlayer(1,Novo):-
        Novo is 2.
changePlayer(2,Novo):-
        Novo is 1.
                                                              
% Writes whose turn it is to play
printPlayer(1,h):- write('Player 1 turn: '), nl.
printPlayer(2,h):- write('Player 2 turn: '), nl.

%Auxiliar predicate to print a play to make it easier for the user to understand
printPlay(P,Xf,Yf):-write('Player number '), write(P), write(' played to '), write(Xf-Yf),nl.

% Auxiliar menu to allow human to choose bot difficulty
menuDificuldade(Dificuldade):-
         print('Dificuldade do Jogo'), nl,
         print('Fácil (f)'), nl,
         print('Dificil (d)'), nl,
         read(Dificuldade).  

% Function to stop the game once we reach the 10th turn        
gameDraw(Board):-printB(Board), write('The Game ended in a draw (reached 10 turns), this is the final board!').

%Checks if player 1 has won
game_over(Board):-          
        \+ (getPiece(_,_,Board,' + ')),
        write('Game has ended! Player 2 wins!'), nl.

%Checks if player 2 has won        
game_over(Board):-           
        \+ (getPiece(_,_,Board,' * ')),
        write('Game has ended! Player 1 wins!'), nl.
