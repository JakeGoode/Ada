-- Name: Jake Goode
-- Student Number: 1202742
-- Date: 02-05-2024
-- Assignment 4: Fun with Languages; Chess Armies of Queens

with Ada.Wide_Text_IO; use Ada.Wide_Text_IO;

--This package holds the procedure to print the board to screen if there is a solution provided from queenarmies.abd.
package body showboard is

    --This procedure prints the solved board out to screen along with the border in Unicode.
    procedure printBoard(n: in integer; blackQ, whiteQ: in queenArray) is
        
        type chessBoard is array(integer range <>, integer range <>) of wide_string(1..1);
        newBoard: chessBoard(0..n - 1, 0..n - 1);
        
        --Game pieces and board colour.
        bSpace: constant wide_string := "◼";
        wSpace: constant wide_string := " ";
        bQueen: constant wide_string := "♛";
        wQueen: constant wide_string := "♕";
        --Game border.
        tlCorner: constant wide_string := "┏";
        trCorner: constant wide_string :=  "┓";
        blCorner: constant wide_string := "┗";
        brCorner: constant wide_string := "┛";
        vDivider: constant wide_string := "┃";
        hDivider: constant wide_string := "━";
        lDivider: constant wide_string := "┣";
        rDivider: constant wide_string := "┫";
        tDivider: constant wide_string := "┳";
        bDivider: constant wide_string := "┻";
        mDivider: constant wide_string := "╋";

    begin
        --Create the board to place the Queen pieces on.
        for i in 0..(n - 1)  loop
            for j in 0..(n - 1) loop

                if (i mod 2) = (j mod 2) then
                    newBoard(i, j) := bSpace;

                else
                    newBoard(i,j) := wSpace;
                end if;
            end loop;
        end loop;

        --Place white Queen pieces to board.
        for queen of whiteQ loop
            newBoard(queen.x, queen.y) := wQueen;
        end loop;

        --Place black Queen pieces to board.
        for queen of blackQ loop
            newBoard(queen.x, queen.y) := bQueen;
        end loop;

        --Print board to screen.
        for i in 0..(n - 1) loop
            --Print top border of game board.
            if i = 0 then
                put(tlCorner);

                for k in 0..(n - 1) loop
                    if k = n - 1 then
                        put(hDivider & trCorner);

                    else
                        put(hDivider & tDivider);
                    end if;
                end loop;

            else --Print middle border of game board.
                put(lDivider);

                for k in 0..(n - 1) loop
                    if k = n - 1 then
                        put(hDivider & rDivider);

                    else
                        put(hDivider & mDivider);
                    end if;
                end loop;
            end if;

            new_line; --Move to next line.
            put(vDivider); --Print beginning of border.

            --Loop to print pieces along with border.
            for j in 0..(n - 1) loop
                put(newBoard(i, j) & vDivider);
            end loop;

            new_line;

            --Print bottom border of game board.
            if i = n - 1 then
                put(blCorner);

                for k in 0..(n - 1) loop
                    if k = n - 1 then
                        put(hDivider & brCorner);

                    else
                        put(hDivider & bDivider);
                    end if;
                end loop;

                new_line;
            end if;        
        end loop;

        new_line; --New line for spacing.

    end printBoard;

end showboard;
