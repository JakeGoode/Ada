-- Name: Jake Goode
-- Student Number: 1202742
-- Date: 02-05-2024
-- Assignment 4: Fun with Languages; Chess Armies of Queens

--Header file for the library to show the board output to screen and position array for each queen type.
package showboard is
    
    --Queen position struct.
    type position is record
        x: integer;
        y: integer;
    end record;

    type queenArray is array(integer range <>) of position;
    
    procedure printBoard(n: in integer; blackQ, whiteQ: in queenArray);

end showboard;