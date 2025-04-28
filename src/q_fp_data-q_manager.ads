with Ada.Containers.Vectors;

package Q_FP_DATA.Q_MANAGER is

   package Q_FP_VECTOR is new
     Ada.Containers.Vectors
       (Index_Type   => NATURAL,
        Element_Type => Q_FP_DATA.T_FLIGHT_PLAN);

   package Q_FP_SORT_CALLSIGN is new
     Q_FP_VECTOR.Generic_Sorting (F_FP_COMPARE_CALLSIGN);
   package Q_FP_SORT_EOBT is new
     Q_FP_VECTOR.Generic_Sorting (F_FP_COMPARE_EOBT);
   package Q_FP_SORT_EOBD is new
     Q_FP_VECTOR.Generic_Sorting (F_FP_COMPARE_EOBD);
   package Q_FP_SORT_AIRCRAFT_NUMBER is new
     Q_FP_VECTOR.Generic_Sorting (F_FP_COMPARE_AIRCRAFT_NUMBER);
   package Q_FP_SORT_SSR_CODE is new
     Q_FP_VECTOR.Generic_Sorting (F_FP_COMPARE_SSR_CODE);

   type T_SORT_CRITERION is
     (E_CALLSIGN, E_EOBT, E_EOBD, E_AIRCRAFT_NUMBER, E_SSR_CODE);

   V_FLIGHT_PLAN_LIST : Q_FP_VECTOR.Vector;

   C_MAX_FLIGHT_PLANS : constant NATURAL := 1000;

   procedure P_ADD_FLIGHT_PLAN (V_FPL : in Q_FP_DATA.T_FLIGHT_PLAN);

   procedure P_DELETE_FLIGHT_PLAN
     (V_INDEX    : in NATURAL := NATURAL'LAST;
      V_CALLSIGN : in T_CALLSIGN := (others => ' '));

   procedure P_DISPLAY_FLIGHT_PLANS;

   procedure P_SORT_FLIGHT_PLANS (V_CRITERION : T_SORT_CRITERION);

end Q_FP_DATA.Q_MANAGER;
