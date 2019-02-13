package body Core is

   function Equivalent_Key (Left, Right: Direction) return Boolean is
   begin
      return Left = Right;
   end Equivalent_Key;

   function Hash_Func(Key: Direction) return Ada.Containers.Hash_Type is
   begin
      return Direction'Pos(Key);
   end Hash_Func;

end Core;
