-- Name: Jake Goode
-- Student Number: 1202742
-- Date: 01-28-2024
-- Assignment 2: Printing a Calendar

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; --use Ada.Integer_Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

--This procedure gets the year and language from the user and displays the year's calendar on screen.
--Variable and subprogram names listed on the assignment document left as shown on the PDF.
procedure cal is

    --Declare Types and variables to use in program.
    type buildCal is array(0..23, 0..20) of integer;
    
    year : integer;
    firstday : integer;
    lang : character := '?';
    indent : constant integer := 14;
    daysArray : buildCal := (others => (others => 0));
    
    --Check to see if year entered by user is after when the Gregorian calendar was 
    --established (1582) and before the year 9999.
    function isvalid(year: in integer) return boolean is
    
    begin
        if((year >= 1582) and (year <= 9999)) then
            return true;
        end if;

        return false;

    end isvalid;

    --Reads the year and language (English or French) the user wishes to see the calendar in.
    procedure readcalinfo(year: out integer; firstday: out integer; lang: out character) is

        y: integer;

    begin
        put_line("Please enter a valid year between 1582-9999:");

        --Loop to validate year input from user.
        loop
            begin                
                year := integer'value(get_line);

                if isvalid(year) then
                    exit; --Exists loop when valid year is entered.
                
                else
                    put_line("Invalid year, please enter a year between 1582-9999:");
                end if;

                exception --If user entered a word instead of a proper integer for the year.
                    when Constraint_Error => put_line("Invalid entry, please enter a year between 1582-9999:");
            end;
        end loop;

        new_line;
        put_line("Please enter your preferred calendar language. Type e for English, f for French.");

        --Loop to validate language character input from user.
        loop
            begin                
                get(lang);
                skip_line; --Clears rest of entry by user.

                --Print calendar in English.
                if ((lang = 'e') or (lang = 'E')) then
                    exit;

                --Print calendar in French.
                elsif ((lang = 'f') or (lang = 'F')) then
                    exit;

                else
                    put_line("Invalid entry, please type e for English or f for French.");
                end if;

                exception --Exception if user entered string instead of character.
                    when Constraint_Error => put_line("Invalid entry, please type e for English or f for French.");
            end;
        end loop;

        y := year - 1;
        firstday := (36 + y + (y/4) - (y/100) + (y/400)) mod 7;

    end readcalinfo;

    --Determines if entered year is a leap year or not.
    function leapyear(year: in integer) return boolean is
    
    begin
        if ((year mod 4 = 0) and (year mod 100 /= 0)) or (year mod 400 = 0) then
            return true;
        end if;

        return false;

    end leapyear;

    --Finds and returns how many days are in each month in the year.
    function numdaysinmonth(month: in integer; year: in integer) return integer is
    
    begin
        if (month = 3) or (month = 5) or (month = 8) or (month = 10) then --When April, June, September, or November.
            return 30;
        
        elsif month = 1 then  --When February.
            if leapyear(year) then                    
                return 29;

            else                    
                return 28;
            end if;

        else
            --When January, March, May, July, August, October, or December.
            return 31;
        end if;

    end numdaysinmonth;

    --Creates a 2D array that holds all of the days in the year in proper order.
    procedure buildcalendar(daysArray: in out buildCal; year: in integer; firstday: in integer) is

        j : integer;
        k : integer;
        n : integer;
        days : array(0..11) of integer := (others => 0);
        startDay : array(0..11) of integer := (others => 0);

    begin
        --Find the number of days in each month.
        for month in 0..11 loop
            days(month) := numdaysinmonth(month, year);
        end loop;

        startDay(0) := firstday;

        --Find which day of the week each month starts.
        for i in 1..11 loop
            startDay(i) := (startDay(i - 1) + days(i - 1)) mod 7;
        end loop;
        
        --Loop to insert the days of each month into the array.
        --Each month is offset from the origin (0,0).
        for month in 0..11 loop

            n := 1; --Current day in month.
            k := days(month);
            j := startDay(month);

            Outer:
            for i in 0..5 loop
                while j <= 6 loop

                    if month = 0 then --January
                        daysArray(i, j) := n;
                        n := n + 1;

                    elsif month = 1 then --February
                        daysArray(i, j + 7) := n;
                        n := n + 1;
                    
                    elsif month = 2 then --March
                        daysArray(i, j + 14) := n;
                        n := n + 1;

                    elsif month = 3 then --April
                        daysArray(i + 6, j) := n;
                        n := n + 1;

                    elsif month = 4 then --May
                        daysArray(i + 6, j + 7) := n;
                        n := n + 1;
                    
                    elsif month = 5 then --June
                        daysArray(i + 6, j + 14) := n;
                        n := n + 1;

                    elsif month = 6 then --July
                        daysArray(i + 12, j) := n;
                        n := n + 1;

                    elsif month = 7 then --August
                        daysArray(i + 12, j + 7) := n;
                        n := n + 1;
                    
                    elsif month = 8 then --September
                        daysArray(i + 12, j + 14) := n;
                        n := n + 1;

                    elsif month = 9 then --October
                        daysArray(i + 18, j) := n;
                        n := n + 1;

                    elsif month = 10 then --November
                        daysArray(i + 18, j + 7) := n;
                        n := n + 1;
                    
                    else --December
                        daysArray(i + 18, j + 14) := n;
                        n := n + 1;
                    end if;

                    j := j + 1;  --Move to next day of the week.
                    exit Outer when n > k;  --Exit outer loop when all days in the month have been placed.
                end loop;

                j := 0;  --Reset index location.
            end loop Outer;
        end loop;

    end buildcalendar;

    --Prints the header of the three months along with the days of the week for each row.
    procedure printrowheading(monthRow: in integer; lang: in character) is
    
    begin        
        if monthRow = 1 then              
            if (lang = 'e' or lang = 'E') then
                put_line("      January                 February                 March");
                put_line("Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa");
            
            else                    
                put_line("      Janvier                 Février                   Mars");
                put_line("Di Lu Ma Me Je Ve Sa    Di Lu Ma Me Je Ve Sa    Di Lu Ma Me Je Ve Sa");
            end if;

        elsif monthRow = 2 then                
            if (lang = 'e' or lang = 'E') then                    
                put_line("       April                    May                     June");
                put_line("Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa");
            
            else                    
                put_line("       Avril                    Mai                     Juin");
                put_line("Di Lu Ma Me Je Ve Sa    Di Lu Ma Me Je Ve Sa    Di Lu Ma Me Je Ve Sa");
            end if;

        elsif monthRow = 3 then                
            if (lang = 'e' or lang = 'E') then                    
                put_line("        July                   August                September");
                put_line("Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa");
            
            else                    
                put_line("      Juillet                   Août                 Septembre");
                put_line("Di Lu Ma Me Je Ve Sa    Di Lu Ma Me Je Ve Sa    Di Lu Ma Me Je Ve Sa");
            end if;

        else                
            if (lang = 'e' or lang = 'E') then                    
                put_line("      October                 November                December");
                put_line("Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa    Su Mo Tu We Th Fr Sa");
            
            else                    
                put_line("      Octobre                 Novembre                Décembre");
                put_line("Di Lu Ma Me Je Ve Sa    Di Lu Ma Me Je Ve Sa    Di Lu Ma Me Je Ve Sa");
            end if;
        end if;

    end printrowheading;

    --Prints out the six rows of each three month row.
    procedure printrowmonth(monthRow: in integer; daysArray: in buildCal) is

    begin
        if monthRow = 1 then
            for i in 0..5 loop --Print the first three month row.
                for j in 0..20 loop

                    if daysArray(i, j) = 0 then
                        put("  "); --Prints a empty space becuase the month does not start on this day.

                    else
                        --Prints the integer from the array to screen. Only case Ada.Integer_Text_IO is used.
                        Ada.Integer_Text_IO.put(daysArray(i, j), width => 2);
                    end if;

                    --If the final day of the week is printed or not.
                    if j = 6 or j = 13 then
                        put("    "); --Space between months.

                    else
                        put(" "); --Space between days.
                    end if;
                end loop;

                new_line; --Moves to next row for printing.
            end loop;

            new_line; --New line for spacing betwen rows.

        elsif monthRow = 2 then
            for i in 6..11 loop --2D array offset for second three month row.
                for j in 0..20 loop

                    if daysArray(i, j) = 0 then
                        put("  ");

                    else
                        --Prints the integer from the array to screen. Only case Ada.Integer_Text_IO is used.
                        Ada.Integer_Text_IO.put(daysArray(i, j), width => 2);
                    end if;

                    if j = 6 or j = 13 then
                        put("    ");

                    else
                        put(" ");
                    end if;
                end loop;

                new_line;
            end loop;

            new_line;

        elsif monthRow = 3 then
            for i in 12..17 loop --2D array offset for third three month row.
                for j in 0..20 loop

                    if daysArray(i, j) = 0 then
                        put("  ");

                    else
                        --Prints the integer from the array to screen. Only case Ada.Integer_Text_IO is used.
                        Ada.Integer_Text_IO.put(daysArray(i, j), width => 2);
                    end if;

                    if j = 6 or j = 13 then
                        put("    ");

                    else
                       put(" ");
                    end if;
                end loop;

                new_line;
            end loop;

            new_line;

        else
            for i in 18..23 loop --2D array offset for fourth three month row.
                for j in 0..20 loop

                    if daysArray(i, j) = 0 then
                        put("  ");

                    else
                        --Prints the integer from the array to screen. Only case Ada.Integer_Text_IO is used.
                        Ada.Integer_Text_IO.put(daysArray(i, j), width => 2);
                    end if;

                    if j = 6 or j = 13 then
                        put("    ");

                    else
                        put(" ");
                    end if;
                end loop;

                new_line;
            end loop;

            new_line;
        end if;

    end printrowmonth;

    --Prints the banner out for the given year provided by the user.
    procedure banner(year: in integer; indent: in integer) is

        type header is array(1..10) of string(1..7); --Holds the digit for the banner.
        type digitsString is array(1..4) of header;  --Holds all four banner digits.

        --Seven by Ten 2D string of each number in large font.
        zero : constant header := (" 00000 ", "0000000", "00   00", "00   00", "00   00", "00   00", "00   00", "00   00", "0000000", " 00000 ");
        one : constant header := ("  111  ", " 1111  ", "   11  ", "   11  ", "   11  ", "   11  ", "   11  ", "   11  ", "   11  ", " 111111");
        two : constant header := (" 22222 ", "22   22", "     22", "     22", "    22 ", "   22  ", "  22   ", " 22    ", "22     ", "2222222");
        three : constant header := (" 33333 ", "3333333", "     33", "     33", "  33333", "  33333", "     33", "     33", "3333333", " 33333 ");
        four : constant header := ("    44 ", "   444 ", "  4444 ", " 44 44 ", "44  44 ", "4444444", "    44 ", "    44 ", "    44 ", "    44 ");
        five : constant header := (" 55555 ", "5555555", "55     ", "55     ", "555555 ", " 555555", "     55", "     55", "5555555", " 55555 ");
        six : constant header := (" 66666 ", "6666666", "66     ", "66     ", "666666 ", "6666666", "66   66", "66   66", "6666666", " 66666 ");
        seven : constant header := ("7777777", " 777777", "     77", "    77 ", "    77 ", "   77  ", "   77  ", "  77   ", "  77   ", "  77   ");
        eight : constant header := (" 88888 ", "8888888", "88   88", "88   88", "8888888", "88   88", "88   88", "88   88", "8888888", " 88888 ");
        nine : constant header := (" 99999 ", "9999999", "99   99", "99   99", "9999999", " 999999", "     99", "     99", "9999999", " 99999 ");

        space : constant string := " ";
        spacing : constant string := indent * space;
        rawYear : constant string := integer'image(year); --Convert year to string to check each digit.
        yearString : constant string := trim(rawYear, ada.strings.left); --Trims excess space from string.
        bannerString : digitsString;
        
    begin
        --Loop to determine what each year digit is and where is belongs on the banner.
        for i in 1..4 loop

            if yearString(i) = '0' then
                bannerString(i):= zero;

            elsif yearString(i) = '1' then
                bannerString(i):= one;

            elsif yearString(i) = '2' then
                bannerString(i):= two;

            elsif yearString(i) = '3' then  
                bannerString(i):= three;

            elsif yearString(i) = '4' then
                bannerString(i):= four;

            elsif yearString(i) = '5' then
                bannerString(i):= five;

            elsif yearString(i) = '6' then
                bannerString(i):= six;

            elsif yearString(i) = '7' then
                bannerString(i):= seven;

            elsif yearString(i) = '8' then
                bannerString(i):= eight;

            else
                bannerString(i):= nine;
            end if;
        end loop;

        new_line(2); --Prints spacing between user entry and banner.

        --Loop to print the banner out row by row.
        for i in 1..10 loop
            put_line(spacing & bannerString(1)(i) & "    " & bannerString(2)(i) & "    " & bannerString(3)(i) & "    " & bannerString(4)(i));
        end loop;

        new_line(2); --Prints spacing between banner and calendar.

    end banner;
    
begin
    readcalinfo(year, firstday, lang); --Get the year and language preference.

    buildcalendar(daysArray, year, firstday); --Build the calendar to 2D array.

    banner(year, indent); --Print the banner.

    --Loop to print the month header and days.
    for monthRow in 1..4 loop
        printrowheading(monthRow, lang);
        printrowmonth(monthRow, daysArray);
    end loop;

    new_line;

end cal;
