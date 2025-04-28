with Ada.Text_IO;
with Ada.Characters.Handling;

package body Q_FP_DATA.Q_VALIDATION is

   -- Validate Callsign (alphanumeric, up to 7 characters)
   function F_VALIDATE_CALLSIGN (V_CALLSIGN : T_CALLSIGN) return BOOLEAN is
   begin
      if V_CALLSIGN'LENGTH > 7 then
         return FALSE;
      end if;

      for C of V_CALLSIGN loop
         if not Ada.Characters.Handling.Is_Alphanumeric (C) then
            return FALSE;
         end if;
      end loop;

      return TRUE;
   end F_VALIDATE_CALLSIGN;

   -- Validate ADEP and ADES (exactly 4 alphabetic characters)
   function F_VALIDATE_AIRPORT_CODE (V_CODE : STRING) return BOOLEAN is
   begin
      if V_CODE'LENGTH /= 4 then
         return FALSE;
      end if;

      for C of V_CODE loop
         if not Ada.Characters.Handling.Is_Letter (C) then
            return FALSE;
         end if;
      end loop;

      return TRUE;
   end F_VALIDATE_AIRPORT_CODE;

   -- Validate EOBT (format: H1H2M1M2, where H1H2 is 00-23 and M1M2 is 00-59)
   function F_VALIDATE_EOBT (V_EOBT : STRING) return BOOLEAN is
      V_HOURS   : INTEGER;
      V_MINUTES : INTEGER;
   begin
      if V_EOBT'LENGTH /= 4 then
         return FALSE;
      end if;

      V_HOURS := INTEGER'VALUE (V_EOBT (V_EOBT'FIRST .. V_EOBT'FIRST + 1));
      V_MINUTES :=
        INTEGER'VALUE (V_EOBT (V_EOBT'FIRST + 2 .. V_EOBT'FIRST + 3));

      return
        (V_HOURS >= 0 and then V_HOURS <= 23)
        and then (V_MINUTES >= 0 and then V_MINUTES <= 59);
   exception
      when others =>
         return FALSE;
   end F_VALIDATE_EOBT;

   -- Validate EOBD (format: YYMMDD, with valid ranges for year, month, and day)
   function F_VALIDATE_EOBD (V_EOBD : STRING) return BOOLEAN is
      V_YEAR  : INTEGER;
      V_MONTH : INTEGER;
      V_DAY   : INTEGER;
   begin
      if V_EOBD'LENGTH /= 6 then
         return FALSE;
      end if;

      V_YEAR := INTEGER'VALUE (V_EOBD (V_EOBD'FIRST .. V_EOBD'FIRST + 1));
      V_MONTH := INTEGER'VALUE (V_EOBD (V_EOBD'FIRST + 2 .. V_EOBD'FIRST + 3));
      V_DAY := INTEGER'VALUE (V_EOBD (V_EOBD'FIRST + 4 .. V_EOBD'FIRST + 5));

      if V_MONTH < 1 or else V_MONTH > 12 then
         return FALSE;
      end if;

      case V_MONTH is
         when 1 | 3 | 5 | 7 | 8 | 10 | 12 =>
            return V_DAY >= 1 and then V_DAY <= 31;

         when 4 | 6 | 9 | 11 =>
            return V_DAY >= 1 and then V_DAY <= 30;

         when 2 =>
            return
              V_DAY >= 1 and then V_DAY <= 29; -- Simplified leap year handling

         when others =>
            return FALSE;
      end case;
   exception
      when others =>
         return FALSE;
   end F_VALIDATE_EOBD;

   -- Validate Aircraft Number (1 to 2 numeric characters)
   function F_VALIDATE_AIRCRAFT_NUMBER (V_NUMBER : STRING) return BOOLEAN is
   begin
      if V_NUMBER'LENGTH < 1 or else V_NUMBER'LENGTH > 2 then
         return FALSE;
      end if;

      for C of V_NUMBER loop
         if not Ada.Characters.Handling.Is_Digit (C) then
            return FALSE;
         end if;
      end loop;

      return TRUE;
   end F_VALIDATE_AIRCRAFT_NUMBER;

   -- Validate Aircraft Type (2 to 4 alphanumeric characters)
   function F_VALIDATE_AIRCRAFT_TYPE (V_TYPE : STRING) return BOOLEAN is
   begin
      if V_TYPE'LENGTH < 2 or else V_TYPE'LENGTH > 4 then
         return FALSE;
      end if;

      for C of V_TYPE loop
         if not Ada.Characters.Handling.Is_Alphanumeric (C) then
            return FALSE;
         end if;
      end loop;

      return TRUE;
   end F_VALIDATE_AIRCRAFT_TYPE;

   -- Validate SSR Code (4 octal digits)
   function F_VALIDATE_SSR_CODE (V_CODE : STRING) return BOOLEAN is
   begin
      if V_CODE'LENGTH /= 4 then
         return FALSE;
      end if;

      for C of V_CODE loop
         if not (C in '0' .. '7') then
            return FALSE;
         end if;
      end loop;

      return TRUE;
   end F_VALIDATE_SSR_CODE;

   function F_VALIDATE_FPL (V_FPL : T_FLIGHT_PLAN) return BOOLEAN is
   begin
      if not F_VALIDATE_CALLSIGN (V_FPL.R_CALLSIGN) then
         Ada.Text_IO.Put_Line ("Invalid Callsign");
         return FALSE;
      end if;

      if not F_VALIDATE_AIRPORT_CODE (V_FPL.R_ADEP) then
         Ada.Text_IO.Put_Line ("Invalid ADEP");
         return FALSE;
      end if;

      if not F_VALIDATE_AIRPORT_CODE (V_FPL.R_ADES) then
         Ada.Text_IO.Put_Line ("Invalid ADES");
         return FALSE;
      end if;

      if not F_VALIDATE_EOBT (V_FPL.R_EOBT) then
         Ada.Text_IO.Put_Line ("Invalid EOBT");
         return FALSE;
      end if;

      if not F_VALIDATE_EOBD (V_FPL.R_EOBD) then
         Ada.Text_IO.Put_Line ("Invalid EOBD");
         return FALSE;
      end if;

      if not F_VALIDATE_AIRCRAFT_NUMBER (V_FPL.R_AIRCRAFT_NUMBER) then
         Ada.Text_IO.Put_Line ("Invalid Aircraft Number");
         return FALSE;
      end if;

      if not F_VALIDATE_AIRCRAFT_TYPE (V_FPL.R_AIRCRAFT_TYPE) then
         Ada.Text_IO.Put_Line ("Invalid Aircraft Type");
         return FALSE;
      end if;

      if not F_VALIDATE_SSR_CODE (V_FPL.R_SSR_CODE) then
         Ada.Text_IO.Put_Line ("Invalid SSR Code");
         return FALSE;
      end if;

      return TRUE;

   exception
      when others =>
         Ada.Text_IO.Put_Line
           ("Validation failed due to an unexpected error.");
         return FALSE;

   end F_VALIDATE_FPL;

end Q_FP_DATA.Q_VALIDATION;
