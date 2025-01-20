## Tic Tac Toe

## Entity
1. [Board](Board.java)
2. [PlayerSymbol ( Enum )](PlayerSymbol.java)
3. [Player](Player.java)

## Flow 
1. Create nxn Board
2. Define Two Players -> PlayerX and PlayerY
3. Play in turns ( Take a integer which will hold value like 0 -> 1 -> 0 -> 1 )
4. For each turn , select row and col where you want to move, if there is already marked then try again with different empty slot
5. After each turn, check if the current move leads to winner or not
6. If totalMoves are n*n means there is no winner so "Match Draw"