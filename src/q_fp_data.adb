package body Q_FP_Data is

   function "<" (Left, Right : T_FLIGHT_PLAN) return Boolean is
   begin
      return Left.R_CALLSIGN < Right.R_CALLSIGN;
   end "<";
   function ">" (Left, Right : T_FLIGHT_PLAN) return Boolean is
   begin
      return Left.R_CALLSIGN > Right.R_CALLSIGN;
   end ">";
   function "=" (Left, Right : T_FLIGHT_PLAN) return Boolean is
   begin
      return Left.R_CALLSIGN = Right.R_CALLSIGN;
   end "=";

end Q_FP_Data;