with Ada.Text_IO;
with Ada.Characters.Handling;

package body Q_FP_Data.Q_Validation is

   -- Validate Callsign (alphanumeric, up to 7 characters)
   function F_VALIDATE_CALLSIGN(Callsign : T_CALLSIGN) return Boolean is
   begin
      if Callsign'Length > 7 then
         return False;
      end if;

      for C of Callsign loop
         if not Ada.Characters.Handling.Is_Alphanumeric(C) then
            return False;
         end if;
      end loop;

      return True;
   end F_VALIDATE_CALLSIGN;

   -- Validate ADEP and ADES (exactly 4 alphabetic characters)
   function F_VALIDATE_AIRPORT_CODE(Code : String) return Boolean is
   begin
      if Code'Length /= 4 then
         return False;
      end if;

      for C of Code loop
         if not Ada.Characters.Handling.Is_Letter(C) then
            return False;
         end if;
      end loop;

      return True;
   end F_VALIDATE_AIRPORT_CODE;

   -- Validate EOBT (format: H1H2M1M2, where H1H2 is 00-23 and M1M2 is 00-59)
   function F_VALIDATE_EOBT(EOBT : String) return Boolean is
      Hours : Integer;
      Minutes : Integer;
   begin
      if EOBT'Length /= 4 then
         return False;
      end if;

      Hours := Integer'Value(EOBT(1 .. 2));
      Minutes := Integer'Value(EOBT(3 .. 4));

      return (Hours >= 0 and Hours <= 23) and (Minutes >= 0 and Minutes <= 59);
   exception
      when others =>
         return False;
   end F_VALIDATE_EOBT;

   -- Validate EOBD (format: YYMMDD, with valid ranges for year, month, and day)
   function F_VALIDATE_EOBD(EOBD : String) return Boolean is
      Year : Integer;
      Month : Integer;
      Day : Integer;
   begin
      if EOBD'Length /= 6 then
         return False;
      end if;

      Year := Integer'Value(EOBD(1 .. 2));
      Month := Integer'Value(EOBD(3 .. 4));
      Day := Integer'Value(EOBD(5 .. 6));

      if Month < 1 or Month > 12 then
         return False;
      end if;

      case Month is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
            return Day >= 1 and Day <= 31;
         when 4 | 6 | 9 | 11 =>
            return Day >= 1 and Day <= 30;
         when 2 =>
            return Day >= 1 and Day <= 29; -- Simplified leap year handling
         when others =>
            return False;
      end case;
   exception
      when others =>
         return False;
   end F_VALIDATE_EOBD;

   -- Validate Aircraft Number (1 to 2 numeric characters)
   function F_VALIDATE_AIRCRAFT_NUMBER(Number : String) return Boolean is
   begin
      if Number'Length < 1 or Number'Length > 2 then
         return False;
      end if;

      for C of Number loop
         if not Ada.Characters.Handling.Is_Digit(C) then
            return False;
         end if;
      end loop;

      return True;
   end F_VALIDATE_AIRCRAFT_NUMBER;

   -- Validate Aircraft Type (2 to 4 alphanumeric characters)
   function F_VALIDATE_AIRCRAFT_TYPE(V_TYPE : String) return Boolean is
   begin
      if V_TYPE'Length < 2 or V_TYPE'Length > 4 then
         return False;
      end if;

      for C of V_TYPE loop
         if not Ada.Characters.Handling.Is_Alphanumeric(C) then
            return False;
         end if;
      end loop;

      return True;
   end F_VALIDATE_AIRCRAFT_TYPE;


   -- Validate SSR Code (4 octal digits)
   function F_VALIDATE_SSR_CODE(Code : String) return Boolean is
   begin
      if Code'Length /= 4 then
         return False;
      end if;

      for C of Code loop
         if not (C in '0' .. '7') then
            return False;
         end if;
      end loop;

      return True;
   end F_VALIDATE_SSR_CODE;

   function F_VALIDATE_FPL(FPL : T_FLIGHT_PLAN) return Boolean is
   begin
      if not F_VALIDATE_CALLSIGN(FPL.R_CALLSIGN) then
         Ada.Text_IO.Put_Line("Invalid Callsign");
         return False;
      end if;

      if not F_VALIDATE_AIRPORT_CODE(FPL.R_ADEP) then
         Ada.Text_IO.Put_Line("Invalid ADEP");
         return False;
      end if;

      if not F_VALIDATE_AIRPORT_CODE(FPL.R_ADES) then
         Ada.Text_IO.Put_Line("Invalid ADES");
         return False;
      end if;

      if not F_VALIDATE_EOBT(FPL.R_EOBT) then
         Ada.Text_IO.Put_Line("Invalid EOBT");
         return False;
      end if;

      if not F_VALIDATE_EOBD(FPL.R_EOBD) then
         Ada.Text_IO.Put_Line("Invalid EOBD");
         return False;
      end if;

      if not F_VALIDATE_AIRCRAFT_NUMBER(FPL.R_AIRCRAFT_NUMBER) then
         Ada.Text_IO.Put_Line("Invalid Aircraft Number");
         return False;
      end if;

      if not F_VALIDATE_AIRCRAFT_TYPE(FPL.R_AIRCRAFT_TYPE) then
         Ada.Text_IO.Put_Line("Invalid Aircraft Type");
         return False;
      end if;

      if not F_VALIDATE_SSR_CODE(FPL.R_SSR_CODE) then
         Ada.Text_IO.Put_Line("Invalid SSR Code");
         return False;
      end if;

      return True;

   exception
      when others =>
         Ada.Text_IO.Put_Line("Validation failed due to an unexpected error.");
         return False;

   end F_VALIDATE_FPL;

end Q_FP_Data.Q_Validation;