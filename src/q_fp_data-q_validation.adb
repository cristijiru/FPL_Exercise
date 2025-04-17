with Ada.Text_IO;
with Ada.Characters.Handling;

package body Q_FP_Data.Q_Validation is

   -- Validate Callsign (alphanumeric, up to 7 characters)
   function Validate_Callsign(Callsign : T_CALLSIGN) return Boolean is
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
   end Validate_Callsign;

   -- Validate ADEP and ADES (exactly 4 alphabetic characters)
   function Validate_Airport_Code(Code : String) return Boolean is
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
   end Validate_Airport_Code;

   -- Validate EOBT (format: H1H2M1M2, where H1H2 is 00-23 and M1M2 is 00-59)
   function Validate_EOBT(EOBT : String) return Boolean is
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
   end Validate_EOBT;

   -- Validate EOBD (format: YYMMDD, with valid ranges for year, month, and day)
   function Validate_EOBD(EOBD : String) return Boolean is
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
   end Validate_EOBD;

   -- Validate Aircraft Number (1 to 2 numeric characters)
   function Validate_Aircraft_Number(Number : String) return Boolean is
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
   end Validate_Aircraft_Number;

   -- Validate Aircraft Type (2 to 4 alphanumeric characters)
   function Validate_Aircraft_Type(V_TYPE : String) return Boolean is
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
   end Validate_Aircraft_Type;


   -- Validate SSR Code (4 octal digits)
   function Validate_SSR_Code(Code : String) return Boolean is
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
   end Validate_SSR_Code;

end Q_FP_Data.Q_Validation;