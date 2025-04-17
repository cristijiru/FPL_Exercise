with Q_FP_Data;
with Ada.Containers.Vectors;

package Q_FP_Data.Q_Manager is

   package FP_Vector is new Ada.Containers.Vectors(
      Index_Type   => Natural,
      Element_Type => Q_FP_Data.T_FLIGHT_PLAN
   );
   
   package FP_Sort is new FP_Vector.Generic_Sorting;
   
   --type T_SORT_CRITERIA is (CALLSIGN, EOBT, EOBD, AIRCRAFT_NUMBER, SSR_CODE);

   V_FLIGHT_PLAN_LIST : FP_Vector.Vector;

   C_MAX_FLIGHT_PLANS : constant Natural := 1000;

   procedure P_ADD_FLIGHT_PLAN(FPL : in Q_FP_Data.T_FLIGHT_PLAN);

   procedure P_DELETE_FLIGHT_PLAN(INDEX : in Natural := Natural'Last; CALLSIGN : in T_CALLSIGN := (others => ' '));

   procedure P_EDIT_FLIGHT_PLAN(INDEX : in Natural; FPL : in Q_FP_Data.T_FLIGHT_PLAN);

   procedure P_DISPLAY_FLIGHT_PLANS;
   
   procedure P_SORT_FLIGHT_PLANS;

end Q_FP_Data.Q_Manager;