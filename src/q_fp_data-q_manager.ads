with Ada.Containers.Vectors;

package Q_FP_DATA.Q_MANAGER is

   package Q_FP_VECTOR is new
     Ada.Containers.Vectors
       (Index_Type   => NATURAL,
        Element_Type => Q_FP_DATA.T_FLIGHT_PLAN);

   package Q_FP_SORT is new Q_FP_VECTOR.Generic_Sorting;

   -- type T_SORT_CRITERIA is (CALLSIGN, EOBT, EOBD, AIRCRAFT_NUMBER, SSR_CODE);

   V_FLIGHT_PLAN_LIST : Q_FP_VECTOR.Vector;

   C_MAX_FLIGHT_PLANS : constant NATURAL := 1000;

   procedure P_ADD_FLIGHT_PLAN (V_FPL : in Q_FP_DATA.T_FLIGHT_PLAN);

   procedure P_DELETE_FLIGHT_PLAN
     (V_INDEX    : in NATURAL := NATURAL'LAST;
      V_CALLSIGN : in T_CALLSIGN := (others => ' '));

   procedure P_DISPLAY_FLIGHT_PLANS;

   procedure P_SORT_FLIGHT_PLANS;

end Q_FP_DATA.Q_MANAGER;
