with Ada.Containers.Hashed_Maps;

package Core is

   type Direction is (North, South, East, West);
   function Equivalent_Key (Left, Right: Direction) return Boolean;
   function Hash_Func(Key: Direction) return Ada.Containers.Hash_Type;

end Core;
