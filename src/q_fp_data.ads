package Q_FP_DATA is

   type T_FLIGHT_RULE is ('I', 'C', 'Y', 'Z', 'V');
   type T_FLIGHT_TYPE is ('S', 'N', 'G', 'M', 'X');
   type T_WAKE_TURBULENCE is ('L', 'M', 'H', 'J');
   subtype T_CALLSIGN is STRING (1 .. 7);

   C_EMPTY_CALLSIGN : constant T_CALLSIGN := (others => ' ');

   -- Define the record for flight plan (FP) fields
   type T_FLIGHT_PLAN is record
      -- Alphanumeric STRING, up to 7 characters
      R_CALLSIGN        : T_CALLSIGN;
      -- Sequence of 4 alphabetic characters
      R_ADEP            : STRING (1 .. 4);
      -- Sequence of 4 alphabetic characters 
      R_ADES            : STRING (1 .. 4);
      -- Format: H1H2M1M2 (00-23 for H1H2, 00-59 for M1M2)
      R_EOBT            : STRING (1 .. 4);
      -- Format: YYMMDD
      R_EOBD            : STRING (1 .. 6);
      -- One of: I, C, Y, Z, V 
      R_FLIGHT_RULE     : T_FLIGHT_RULE;
      -- One of: S, N, G, M, X
      R_FLIGHT_TYPE     : T_FLIGHT_TYPE;
      -- 1 to 2 numeric characters
      R_AIRCRAFT_NUMBER : STRING (1 .. 2);
      -- 2 to 4 alphanumeric characters
      R_AIRCRAFT_TYPE   : STRING (1 .. 4);
      -- One of: L, M, H, J
      R_WAKE_TURBULENCE : T_WAKE_TURBULENCE;
      -- Up to 64 alphabetic characters
      R_NCA_EQUIPMENT   : STRING (1 .. 64);
      -- Up to 20 alphabetic characters
      R_SSR_EQUIPMENT   : STRING (1 .. 20);
      -- 4 octal digits
      R_SSR_CODE        : STRING (1 .. 4);
      -- Formats: KC1C2C3C4, NC1C2C3C4, MC1C2C3
      R_TAS             : STRING (1 .. 5);
      -- Formats: AC1C2C3, FC1C2C3, MC1C2C3C4, SC1C2C3C4, VFR
      R_RFL             : STRING (1 .. 5);
      -- Up to 300 alphabetic characters
      R_ROUTE           : STRING (1 .. 300);
   end record;

   function "<" (V_LEFT, V_RIGHT : T_FLIGHT_PLAN) return BOOLEAN;
   function ">" (V_LEFT, V_RIGHT : T_FLIGHT_PLAN) return BOOLEAN;
   function "=" (V_LEFT, V_RIGHT : T_FLIGHT_PLAN) return BOOLEAN;

end Q_FP_DATA;
