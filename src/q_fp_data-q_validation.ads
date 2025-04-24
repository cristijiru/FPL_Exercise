package Q_FP_DATA.Q_VALIDATION is

   -- Validate Callsign (alphanumeric, up to 7 characters)
   function F_VALIDATE_CALLSIGN (V_CALLSIGN : T_CALLSIGN) return BOOLEAN;

   -- Validate ADEP and ADES (exactly 4 alphabetic characters)
   function F_VALIDATE_AIRPORT_CODE (V_CODE : STRING) return BOOLEAN;

   -- Validate EOBT (format: H1H2M1M2, where H1H2 is 00-23 and M1M2 is 00-59)
   function F_VALIDATE_EOBT (V_EOBT : STRING) return BOOLEAN;

   -- Validate EOBD (format: YYMMDD, with valid ranges for year, month, and day)
   function F_VALIDATE_EOBD (V_EOBD : STRING) return BOOLEAN;

   -- Validate Aircraft Number (1 to 2 numeric characters)
   function F_VALIDATE_AIRCRAFT_NUMBER (V_NUMBER : STRING) return BOOLEAN;

   -- Validate Aircraft Type (2 to 4 alphanumeric characters)
   function F_VALIDATE_AIRCRAFT_TYPE (V_TYPE : STRING) return BOOLEAN;

   -- Validate SSR Code (4 octal digits)
   function F_VALIDATE_SSR_CODE (V_CODE : STRING) return BOOLEAN;

   -- Validate Entire FPL
   function F_VALIDATE_FPL (V_FPL : T_FLIGHT_PLAN) return BOOLEAN;

end Q_FP_DATA.Q_VALIDATION;
