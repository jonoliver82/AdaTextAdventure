package body Location is
   
   function CreateGameLocation(LocationName: String; LocationDescription: String) return GameLocation is
      item : GameLocation;
   begin
      item.Name := To_Unbounded_String(LocationName);
      item.Description := To_Unbounded_String(LocationDescription);
      return item;
   end CreateGameLocation;
   
   procedure AddLink(Self: in out GameLocation; direction: Core.Direction; name: Unbounded_String) is
   begin
      Self.LinkedLocations.Include(direction, name);
   end;
   
   function Get_Description(Self: GameLocation) return Unbounded_String is
   begin
      return Self.Description;
   end;
   
   function Get_Name(Self: GameLocation) return Unbounded_String is
   begin
        return Self.Name;
   end;
   
   function Get_LinkedLocations(Self: GameLocation) return Direction_Location_Hashed_Maps.Map is
   begin
      return self.LinkedLocations;
   end;
   
end Location;
