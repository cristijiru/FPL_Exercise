with Ada.Text_IO;
with Q_FP_Data;
with Q_FP_Data.Q_Validation;

package body Q_FP_Data.Q_Manager is

   procedure P_ADD_FLIGHT_PLAN(FPL : in Q_FP_Data.T_FLIGHT_PLAN) is
   begin
      if Natural(V_FLIGHT_PLAN_LIST.Length) < C_MAX_FLIGHT_PLANS and then Q_FP_Data.Q_Validation.F_VALIDATE_FPL(FPL) then
         V_FLIGHT_PLAN_LIST.Append(FPL);
      else
         Ada.Text_IO.Put_Line("Flight plan list is full. Cannot add more plans.");
      end if;
   end P_ADD_FLIGHT_PLAN;

procedure P_DELETE_FLIGHT_PLAN(INDEX : in Natural := Natural'Last; CALLSIGN : in T_CALLSIGN := (others => ' ')) is
   begin
      if INDEX /= Natural'Last and then CALLSIGN /= "       " then
         Ada.Text_IO.Put_Line("Both index and callsign provided. No flight plan will be deleted.");
         return;
      end if;
      
      if INDEX /= Natural'Last then
         if INDEX <= Natural(V_FLIGHT_PLAN_LIST.Length) then
            V_FLIGHT_PLAN_LIST.Delete(INDEX);
            return;
         else
            Ada.Text_IO.Put_Line("Invalid index. Cannot delete flight plan.");
            return;
         end if;
      end if;

      if CALLSIGN /= "       " then
         declare
         begin
            for I in V_FLIGHT_PLAN_LIST.First_Index .. V_FLIGHT_PLAN_LIST.Last_Index loop
               if V_FLIGHT_PLAN_LIST(I).R_CALLSIGN = CALLSIGN then
                  V_FLIGHT_PLAN_LIST.Delete(I);
                  return;
               end if;
            end loop;
         end;
      end if;

   end P_DELETE_FLIGHT_PLAN;

   procedure P_EDIT_FLIGHT_PLAN(INDEX : in Natural; FPL : in Q_FP_Data.T_FLIGHT_PLAN) is
   begin
      if INDEX <= Natural(V_FLIGHT_PLAN_LIST.Length) then
         V_FLIGHT_PLAN_LIST(INDEX) := FPL;
      else
         Ada.Text_IO.Put_Line("Invalid index. Cannot edit flight plan.");
      end if;
   end P_EDIT_FLIGHT_PLAN;

   -- Display all flight plans
   procedure P_DISPLAY_FLIGHT_PLANS is
   begin
      for V_FPL of V_FLIGHT_PLAN_LIST loop
         Ada.Text_IO.Put_Line("Flight Plan " &
                              V_FPL.R_CALLSIGN & " " &
                              V_FPL.R_ADEP & " " &
                              V_FPL.R_ADES);
      end loop;
   end P_DISPLAY_FLIGHT_PLANS;

   procedure P_SORT_FLIGHT_PLANS is
   begin
      FP_Sort.Sort(V_FLIGHT_PLAN_LIST);
   end P_SORT_FLIGHT_PLANS;

end Q_FP_Data.Q_Manager;