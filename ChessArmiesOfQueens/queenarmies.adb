-- Name: Jake Goode
-- Student Number: 1202742
-- Date: 02-05-2024
-- Assignment 4: Fun with Languages; Chess Armies of Queens

with Ada.Text_IO; use Ada.Text_IO;
with showboard; use showboard;

--This procedure finds at least two unique solutions, up to a maximum of 3 to the chess armies of queens algorithm
--using recursion. All solutions have at least 2 unique solutions for m up to a value of 6 and n up to a value of 12
--except for 1 black and white Queen on a 3x3 board (https://christopherwolfram.com/projects/queen-armies/).
--This is because rotating or mirroring the pieces on different locations around the board generates the same solution.
--The solutions are sent to showboard.adb to be displayed on screen.
procedure queenarmies is

    m: integer; --Number of Queens.
    n: integer; --Board size (n x n).
    solution: integer := 0;
    bQueensPlaced: integer := 0;
    wQueensPlaced: integer := 0;
    maxSolutions: constant integer := 3;  --Change value if you wish to see more or less solutions.

    --Check to see if user entered valid number of Queens or board size.
    function isvalid(check: in integer; verify: in integer) return boolean is
    
    begin
        if check = 1 then
            if((verify >= 1) and (verify <= 6)) then
                return true;
            end if;

        else
            if((verify >= 3) and (verify <= 12)) then
                return true;
            end if;
        end if;

        return false;

    end isvalid;

    --Reads the number of Queens and board size to solve from user.
    procedure readBoardInfo(m: out integer; n: out integer) is

    begin
        put_line("Please enter the number of Queens between 1-6:");

        --Loop to validate number of Queens.
        loop
            begin                
                m := integer'value(get_line);

                if isvalid(1, m) then
                    exit; --Exists loop when valid number of Queens.
                
                else
                    put_line("Invalid entry, please use a value between 1-6:");
                end if;

                exception --If user entered a word instead of a proper integer for the number of Queens.
                    when Constraint_Error => put_line("Invalid entry, please use a value between 1-6:");
            end;
        end loop;

        new_line;
        put_line("Please enter a board size between 3-12:");

        --Loop to validate board size.
        loop
            begin                
                n := integer'value(get_line);

                if isvalid(2, n) then
                    if m >= n then --Checks to make sure number of Queens entered is not larger than board size.
                        put_line("Too many queens for the specified board size. Please enter a larger board size.");

                    else
                        exit; --Exists loop when valid board size.
                    end if;
                    
                else
                    put_line("Invalid entry, please use a value between 3-12:");
                end if;

                exception --If user entered a word instead of a proper integer for the board size.
                    when Constraint_Error => put_line("Invalid entry, please use a value between 3-12:");
            end;
        end loop;

    end readBoardInfo;

    --Checks to see if the currently placed Queen can attack the opposing colour.
    function attacking(nextQueen: in position; pos: in position) return boolean is

    begin
        --Returns true if position is attacking the Queen horizontally, vertically, or diagonally. Otherwise returns false.
        return (nextQueen.x = pos.x) or (nextQueen.y = pos.y) or ((abs(nextQueen.x - pos.x)) = (abs(nextQueen.y - pos.y)));

    end attacking;
    
    --This procedure rotates each Queens position by 90 degrees to check all possible locations and to ensure a soft lock
    --in the algorithm won't occur. Example (0,0) becomes (0,3) after rotation.
    procedure rotateQueens(n: in integer; blackQ, whiteQ: in out queenArray; bQueensPlaced, wQueensPlaced: in integer) is 

        --Temp arrays to hold old locations for both black and white Queens.
        tempBlackQ: constant queenArray := blackQ;
        tempWhiteQ: constant queenArray := whiteQ;

    begin
        --Loop to rotate all the black Queens on the board.
        for i in 1..bQueensPlaced loop
            blackQ(i).x := tempBlackQ(i).y; --Swap x and y. 
            blackQ(i).y := (n - 1) - tempBlackQ(i).x; --Swap y and (n - 1 - x).
        end loop;

        --Loop to rotate all white Queens on the board.
        for i in 1..wQueensPlaced loop
            whiteQ(i).x := tempWhiteQ(i).y;
            whiteQ(i).y := (n - 1) - tempWhiteQ(i).x;
        end loop;

    end rotateQueens;
    
    --Creates up to maxSolutions number of unique solutions to placing m number of Queens on a n x n sized board.
    --This algorithm uses recursion to find the solutions.
    procedure buildBoard(m, n: in integer; blackQ, whiteQ: in out queenArray; bQueensPlaced, wQueensPlaced, solution: in out integer) is

        blackTurn: boolean := true; --Current turn is for the black Queen.

    begin
        --Base case. All pieces of each colour have been successfully placed.
        if m = 0 then
            
            --No more solutions are needed to be printed or counted.
            if solution >= maxSolutions then
                return;
            end if;
            
            solution := solution + 1;
            printBoard(n, blackQ, whiteQ);
            return;
        end if;
        
        --Loop to place either a white or black Queen onto the board.
        for i in 0..(n - 1) loop
            for j in 0..(n - 1) loop
                declare
                    pos: constant position := (x => i, y => j);
                    validPlace: boolean := true;
                    nextQueen: position;

                begin
                    --Check to see if there is at least 1 black Queen placed.
                    if bQueensPlaced /= 0 then
                        --Loop to make sure current piece is not attacking or if a Queen is in the current position.
                        for q in 1..bQueensPlaced loop
                            
                            nextQueen := blackQ(q);

                            if (pos = nextQueen) or ((not blackTurn) and (attacking(nextQueen, pos))) then
                                validPlace := false; --Skip to end of j loop.
                                exit;
                            end if;
                        end loop;
                    end if;

                    --Check to see if there is at least 1 white Queen placed.
                    if wQueensPlaced /= 0 then
                        if validPlace then
                            --Loop to make sure current piece is not attacking or if a Queen is in the current position.
                            for q in 1..wQueensPlaced loop

                                nextQueen := whiteQ(q);

                                if (pos = nextQueen) or (blackTurn and (attacking(nextQueen, pos))) then
                                    validPlace := false; --Skip to end of j loop.
                                    exit;
                                end if;
                            end loop;
                        end if;
                    end if;
                    
                    --Place the Queen based on current turn.
                    if validPlace then
                        if blackTurn then
                            bQueensPlaced := bQueensPlaced + 1;
                            blackQ(bQueensPlaced) := pos;
                            blackTurn := false; --White Queens turn is next.
                        
                        else
                            wQueensPlaced := wQueensPlaced + 1;
                            whiteQ(wQueensPlaced) := pos;

                            --Rotate board by 90 degrees when one Queen of each colour has been placed.
                            rotateQueens(n, blackQ, whiteQ, bQueensPlaced, wQueensPlaced);

                            --Exit when all necessary solutions have been found.
                            if solution >= maxSolutions then
                                return;
                            
                            else                                
                                --Find placement for next Queens.
                                buildBoard(m - 1, n, blackQ, whiteQ, bQueensPlaced, wQueensPlaced, solution);
                            end if;

                            --Back track.
                            bQueensPlaced := bQueensPlaced - 1; --Pop black Queen from array.
                            wQueensPlaced := wQueensPlaced - 1; --Pop white Queen from array.
                            blackTurn := true; --Black Queens turn is next.
                        end if;
                    end if;
                end;
            end loop;
        end loop;

        --Pop black Queen from array if it is not black's current turn.
        if not blackTurn then
            bQueensPlaced := bQueensPlaced - 1;
        end if;

    end buildBoard;

begin
    new_line;
    put_line("Chess Armies of Queens");
    put_line("----------------------");
    put_line("This program will display up to 3 unique solutions from the provided number of Queens and board size.");
    put_line("Once the appropriate data has been entered, the program will print each solution or notify if no solutions are available");
    put_line("If the number of Queens to place exceeds or is equal to the board size, an error will be displayed and ask for re-entry.");
    put_line("This is due in fact that no solutions will be available.");
    put_line("One case where only one solution will be displayed is if 1 Queen of each colour is placed on a 3x3 board.");
    put_line("Only one unique solution is available for this case as any other solution is either a mirror or rotation of the original solution.");
    put_line("This is discussed and can be visualized here: https://christopherwolfram.com/projects/queen-armies/.");
    new_line;
    
    readBoardInfo(m, n); --Get the number of Queen pieces and board size from user.

    declare
        
        blackQ: queenArray(1..m); --Number of black Queens to place on board.
        whiteQ: queenArray(1..m); --Number of white Queens to place on board.

    begin
        new_line; --Move to next line.
        
        --Start finding solutions.
        buildBoard(m, n, blackQ, whiteQ, bQueensPlaced, wQueensPlaced, solution);
            
        if solution = 0 then
            new_line;
            put_line("No solutions exist for the provided number of queens and board size.");

        else
            put_line("Solutions have been provided above.");
        end if;

        new_line; --Move to next line.
    end;

end queenarmies;
