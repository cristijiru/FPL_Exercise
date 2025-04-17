with Q_FP_Data;

package Q_FP_Data.Q_Validation is

   -- Validate Callsign (alphanumeric, up to 7 characters)
   function Validate_Callsign(Callsign : T_CALLSIGN) return Boolean;

   -- Validate ADEP and ADES (exactly 4 alphabetic characters)
   function Validate_Airport_Code(Code : String) return Boolean;

   -- Validate EOBT (format: H1H2M1M2, where H1H2 is 00-23 and M1M2 is 00-59)
   function Validate_EOBT(EOBT : String) return Boolean;

   -- Validate EOBD (format: YYMMDD, with valid ranges for year, month, and day)
   function Validate_EOBD(EOBD : String) return Boolean;

   -- Validate Aircraft Number (1 to 2 numeric characters)
   function Validate_Aircraft_Number(Number : String) return Boolean;

   -- Validate Aircraft Type (2 to 4 alphanumeric characters)
   function Validate_Aircraft_Type(V_Type : String) return Boolean;

   -- Validate SSR Code (4 octal digits)
   function Validate_SSR_Code(Code : String) return Boolean;

end Q_FP_Data.Q_Validation;