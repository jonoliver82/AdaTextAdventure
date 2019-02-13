with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Core; use Core;

package Location is

   type GameLocation is tagged private;
   
   function CreateGameLocation(LocationName: String; LocationDescription: String) return GameLocation;
   
   package Direction_Location_Hashed_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Direction,
      Element_Type => Unbounded_String,
      Hash => Hash_Func,
      Equivalent_Keys => Equivalent_Key);
   
   procedure AddLink(Self: in out GameLocation; direction: Core.Direction; name: Unbounded_String);
   function Get_Description(Self: GameLocation) return Unbounded_String;
   function Get_Name(Self: GameLocation) return Unbounded_String;
   function Get_LinkedLocations(Self: GameLocation) return Direction_Location_Hashed_Maps.Map;
 
private
  type GameLocation is tagged 
      record
         Name: Unbounded_String;
         Description: Unbounded_String;
         LinkedLocations : Direction_Location_Hashed_Maps.Map;
      end record;
   
end Location;
