package body Q_FP_DATA is

   function "<" (V_LEFT, V_RIGHT : T_FLIGHT_PLAN) return BOOLEAN is
   begin
      return V_LEFT.R_CALLSIGN < V_RIGHT.R_CALLSIGN;
   end "<";
   function ">" (V_LEFT, V_RIGHT : T_FLIGHT_PLAN) return BOOLEAN is
   begin
      return V_LEFT.R_CALLSIGN > V_RIGHT.R_CALLSIGN;
   end ">";
   function "=" (V_LEFT, V_RIGHT : T_FLIGHT_PLAN) return BOOLEAN is
   begin
      return V_LEFT.R_CALLSIGN = V_RIGHT.R_CALLSIGN;
   end "=";

end Q_FP_DATA;
