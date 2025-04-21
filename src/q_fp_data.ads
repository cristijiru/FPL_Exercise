package Q_FP_Data is

   type T_FLIGHT_RULE is ('I', 'C', 'Y', 'Z', 'V');
   type T_FLIGHT_TYPE is ('S', 'N', 'G', 'M', 'X');
   type T_WAKE_TURBULENCE is ('L', 'M', 'H', 'J');
   subtype T_CALLSIGN is String(1 .. 7);

   C_EMPTY_CALLSIGN : constant Q_FP_Data.T_CALLSIGN := (others => ' ');

   -- Define the record for flight plan (FP) fields
   type T_FLIGHT_PLAN is record
      R_CALLSIGN         : T_CALLSIGN;        -- Alphanumeric string, up to 7 characters
      R_ADEP             : String(1 .. 4);    -- Sequence of 4 alphabetic characters
      R_ADES             : String(1 .. 4);    -- Sequence of 4 alphabetic characters
      R_EOBT             : String(1 .. 4);    -- Format: H1H2M1M2 (00-23 for H1H2, 00-59 for M1M2)
      R_EOBD             : String(1 .. 6);    -- Format: YYMMDD
      R_FLIGHT_RULE      : T_FLIGHT_RULE;     -- One of: I, C, Y, Z, V
      R_FLIGHT_TYPE      : T_FLIGHT_TYPE;     -- One of: S, N, G, M, X
      R_AIRCRAFT_NUMBER  : String(1 .. 2);    -- 1 to 2 numeric characters
      R_AIRCRAFT_TYPE    : String(1 .. 4);    -- 2 to 4 alphanumeric characters
      R_WAKE_TURBULENCE  : T_WAKE_TURBULENCE; -- One of: L, M, H, J
      R_NCA_EQUIPMENT    : String(1 .. 64);   -- Up to 64 alphabetic characters
      R_SSR_EQUIPMENT    : String(1 .. 20);   -- Up to 20 alphabetic characters
      R_SSR_CODE         : String(1 .. 4);    -- 4 octal digits
      R_TAS              : String(1 .. 5);    -- Formats: KC1C2C3C4, NC1C2C3C4, MC1C2C3
      R_RFL              : String(1 .. 5);    -- Formats: AC1C2C3, FC1C2C3, MC1C2C3C4, SC1C2C3C4, VFR
      R_ROUTE            : String(1 .. 300);  -- Up to 300 alphabetic characters
   end record;

   function "<" (Left, Right : T_FLIGHT_PLAN) return Boolean;
   function ">" (Left, Right : T_FLIGHT_PLAN) return Boolean;
   function "=" (Left, Right : T_FLIGHT_PLAN) return Boolean;

end Q_FP_Data;