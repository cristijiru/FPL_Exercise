with Ada.Text_IO;
with Q_FP_DATA.Q_VALIDATION;

package body Q_FP_DATA.Q_MANAGER is

   procedure P_ADD_FLIGHT_PLAN (V_FPL : in Q_FP_DATA.T_FLIGHT_PLAN) is
   begin
      if NATURAL (V_FLIGHT_PLAN_LIST.Length) < C_MAX_FLIGHT_PLANS
        and then Q_FP_DATA.Q_VALIDATION.F_VALIDATE_FPL (V_FPL)
      then
         V_FLIGHT_PLAN_LIST.Append (V_FPL);
      else
         Ada.Text_IO.Put_Line
           ("Flight plan list is full. Cannot add more plans.");
      end if;
   end P_ADD_FLIGHT_PLAN;

   procedure P_DELETE_FLIGHT_PLAN
     (V_INDEX    : in NATURAL := NATURAL'LAST;
      V_CALLSIGN : in T_CALLSIGN := (others => ' ')) is
   begin
      if V_INDEX /= NATURAL'LAST
        and then V_CALLSIGN /= Q_FP_DATA.C_EMPTY_CALLSIGN
      then
         Ada.Text_IO.Put_Line
           ("Both index and callsign provided. No flight plan will be deleted.");
         return;
      end if;

      if V_INDEX /= NATURAL'LAST then
         if V_INDEX <= NATURAL (V_FLIGHT_PLAN_LIST.Length) then
            V_FLIGHT_PLAN_LIST.Delete (V_INDEX);
            return;
         else
            Ada.Text_IO.Put_Line ("Invalid index. Cannot delete flight plan.");
            return;
         end if;
      end if;

      if V_CALLSIGN /= Q_FP_DATA.C_EMPTY_CALLSIGN then
         declare
         begin
            for I
              in V_FLIGHT_PLAN_LIST.FIRST_INDEX
                 .. V_FLIGHT_PLAN_LIST.LAST_INDEX
            loop
               if V_FLIGHT_PLAN_LIST (I).R_CALLSIGN = V_CALLSIGN then
                  V_FLIGHT_PLAN_LIST.Delete (I);
                  return;
               end if;
            end loop;
         end;
      end if;

   end P_DELETE_FLIGHT_PLAN;

   procedure P_EDIT_FLIGHT_PLAN
     (V_INDEX : in NATURAL; V_FPL : in Q_FP_DATA.T_FLIGHT_PLAN) is
   begin
      if V_INDEX <= NATURAL (V_FLIGHT_PLAN_LIST.Length)
        and then Q_FP_DATA.Q_VALIDATION.F_VALIDATE_FPL (V_FPL)
      then
         V_FLIGHT_PLAN_LIST (V_INDEX) := V_FPL;
      else
         Ada.Text_IO.Put_Line ("Invalid index. Cannot edit flight plan.");
      end if;
   end P_EDIT_FLIGHT_PLAN;

   -- Display all flight plans
   procedure P_DISPLAY_FLIGHT_PLANS is
   begin
      for V_FPL of V_FLIGHT_PLAN_LIST loop
         Ada.Text_IO.Put_Line
           ("Flight Plan "
            & V_FPL.R_CALLSIGN
            & " "
            & V_FPL.R_SSR_CODE
            & " "
            & V_FPL.R_ADEP
            & " "
            & V_FPL.R_ADES);
      end loop;
   end P_DISPLAY_FLIGHT_PLANS;

   procedure P_SORT_FLIGHT_PLANS is
   begin
      Q_FP_SORT.Sort (V_FLIGHT_PLAN_LIST);
   end P_SORT_FLIGHT_PLANS;

end Q_FP_DATA.Q_MANAGER;
