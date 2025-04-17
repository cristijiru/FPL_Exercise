with Ada.Text_IO; 
with Q_FP_Data;
with Q_FP_Data.Q_Manager;
with Q_FP_Data.Q_Validation;

procedure Fpl_Exercise is
   V_CURRENT_FPL : Q_FP_Data.T_FLIGHT_PLAN;

   function F_CREATE_FLIGHT_PLAN (CALLSIGN : Q_FP_Data.T_CALLSIGN) return Q_FP_Data.T_FLIGHT_PLAN is
      V_TEMP_FPL : Q_FP_Data.T_FLIGHT_PLAN;
   begin
      if CALLSIGN'Length > 7 then
         Ada.Text_IO.Put_Line("Callsign too long. Maximum length is 7 characters.");
         return V_TEMP_FPL;
      end if;
      -- Example of adding a flight plan
      V_TEMP_FPL.R_CALLSIGN := "       ";
      V_TEMP_FPL.R_CALLSIGN(1 .. CALLSIGN'Length) := CALLSIGN;
      V_TEMP_FPL.R_ADEP := "EGLL";
      V_TEMP_FPL.R_ADES := "EGKK";
      V_TEMP_FPL.R_EOBT := "1200";
      V_TEMP_FPL.R_EOBD := "230101";
      V_TEMP_FPL.R_FLIGHT_RULE := 'I';
      V_TEMP_FPL.R_FLIGHT_TYPE := 'S';
      V_TEMP_FPL.R_AIRCRAFT_NUMBER := "01";
      V_TEMP_FPL.R_AIRCRAFT_TYPE := "A320";
      V_TEMP_FPL.R_WAKE_TURBULENCE := 'M';
      V_TEMP_FPL.R_NCA_EQUIPMENT(1..3) := "N/A";
      V_TEMP_FPL.R_SSR_EQUIPMENT(1..3) := "N/A";
      V_TEMP_FPL.R_SSR_CODE := "1234";
      V_TEMP_FPL.R_TAS(1..4) := "250K";
      V_TEMP_FPL.R_RFL := "FL350";
      V_TEMP_FPL.R_ROUTE(1..21) := "EGLL ACORN XAMAN EGKK";
      return V_TEMP_FPL;
   end F_CREATE_FLIGHT_PLAN;
begin
   Q_FP_Data.Q_Manager.P_ADD_FLIGHT_PLAN(F_CREATE_FLIGHT_PLAN("ABC1234"));
   Q_FP_Data.Q_Manager.P_ADD_FLIGHT_PLAN(F_CREATE_FLIGHT_PLAN("XYZ4567"));
   Q_FP_Data.Q_Manager.P_ADD_FLIGHT_PLAN(F_CREATE_FLIGHT_PLAN("LMN7890"));
   Q_FP_Data.Q_Manager.P_ADD_FLIGHT_PLAN(F_CREATE_FLIGHT_PLAN("DEF0123"));
   Q_FP_Data.Q_Manager.P_ADD_FLIGHT_PLAN(F_CREATE_FLIGHT_PLAN("GHI3456"));

   Q_FP_Data.Q_Manager.P_DELETE_FLIGHT_PLAN(INDEX => 0);
   Q_FP_Data.Q_Manager.P_DELETE_FLIGHT_PLAN(CALLSIGN => "XYZ4567");
   Q_FP_Data.Q_Manager.P_DELETE_FLIGHT_PLAN(INDEX => 1, CALLSIGN => "GHI3456");

   Ada.Text_IO.Put_Line("Unsorted Flight Plans:");
   Q_FP_Data.Q_Manager.P_DISPLAY_FLIGHT_PLANS;
   Q_FP_Data.Q_Manager.P_SORT_FLIGHT_PLANS;
   Ada.Text_IO.Put_Line("Sorted Flight Plans:");
   Q_FP_Data.Q_Manager.P_DISPLAY_FLIGHT_PLANS;

end Fpl_Exercise;