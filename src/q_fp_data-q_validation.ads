with Q_FP_Data;

package Q_FP_Data.Q_Validation is

   -- Validate Callsign (alphanumeric, up to 7 characters)
   function F_VALIDATE_CALLSIGN(Callsign : T_CALLSIGN) return Boolean;

   -- Validate ADEP and ADES (exactly 4 alphabetic characters)
   function F_VALIDATE_AIRPORT_CODE(Code : String) return Boolean;

   -- Validate EOBT (format: H1H2M1M2, where H1H2 is 00-23 and M1M2 is 00-59)
   function F_VALIDATE_EOBT(EOBT : String) return Boolean;

   -- Validate EOBD (format: YYMMDD, with valid ranges for year, month, and day)
   function F_VALIDATE_EOBD(EOBD : String) return Boolean;

   -- Validate Aircraft Number (1 to 2 numeric characters)
   function F_VALIDATE_AIRCRAFT_NUMBER(Number : String) return Boolean;

   -- Validate Aircraft Type (2 to 4 alphanumeric characters)
   function F_VALIDATE_AIRCRAFT_TYPE(V_Type : String) return Boolean;

   -- Validate SSR Code (4 octal digits)
   function F_VALIDATE_SSR_CODE(Code : String) return Boolean;

   -- Validate Entire FPL
   function F_VALIDATE_FPL(FPL : T_FLIGHT_PLAN) return Boolean;

end Q_FP_Data.Q_Validation;