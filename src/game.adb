with Ada.Containers.Hashed_Maps;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Equal_Case_Insensitive;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Hash;
with Ada.Strings.Unbounded.Equal_Case_Insensitive;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Text_IO; use Ada.Text_IO;
with Location; use Location;
with Core; use Core;

package body Game is

   package Location_Hashed_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type => Unbounded_String,
      Element_Type => GameLocation,
      Hash => Ada.Strings.Unbounded.Hash,
      Equivalent_Keys => Ada.Strings.Unbounded.Equal_Case_Insensitive);
   
   Current_Location: GameLocation;   
   Locations: Location_Hashed_Maps.Map;
   
   CHAR_QUIT: constant Character := 'q';
   
   procedure Initialise is
      WoodsKey : Unbounded_String := To_Unbounded_String("woods");
      LakeKey: Unbounded_String := To_Unbounded_String("lake");
      CastleKey: Unbounded_String := To_Unbounded_String("castle");
      
      Woods: GameLocation := CreateGameLocation("The woods", "You are in the woods. There are lots of trees.");
      Lake: GameLocation := CreateGameLocation("The lake", "You are by the lake. It is very watery.");
      Castle: GameLocation := CreateGameLocation("The castle", "You are in the castle. It is dark");
   begin
      Locations.Include(Key => WoodsKey, New_Item => Woods);
      Locations.Include(Key => LakeKey,  New_Item => Lake);
      Locations.Include(Key => CastleKey,  New_Item => Castle);
      
      Locations(WoodsKey).AddLink(North, LakeKey);
      Locations(LakeKey).AddLink(South, WoodsKey);
      Locations(LakeKey).AddLink(East, CastleKey);
      Locations(CastleKey).AddLink(West, LakeKey);
      
      Current_Location := Locations(WoodsKey);
   end;
   
   procedure Run is
      Linked_Locations: Direction_Location_Hashed_Maps.Map;
      Temp_Key: Unbounded_String;
      Command: Unbounded_String;
      Continue: Boolean := True;
      Commanded_Direction : Direction;
      Valid_Direction: Boolean := False;
      New_Location_Key: Unbounded_String;
      First_Char: Character;
   begin
      Put_Line("Q to quit");
      
      while Continue loop
   
         -- Display description of current location
         Put_Line(To_String(Current_Location.Get_Description));
         
         -- Display neighbouring locations
         Linked_Locations := Current_Location.Get_LinkedLocations;         
         for Linked_Location_Cursor in Linked_Locations.Iterate loop
            Temp_Key := Direction_Location_Hashed_Maps.Element(Linked_Location_Cursor);
            Put_Line (Direction_Location_Hashed_Maps.Key(Linked_Location_Cursor)'Image & ": " & To_String(Locations(Temp_Key).Get_Name));
         end loop;
         
         -- Read player input
         Valid_Direction := False;
         Put(">");
         Command := Get_Line;
         
         If Length(Command) > 0 Then
            First_Char := To_Lower(To_String(Command)(1));
            If First_Char = CHAR_QUIT Then
               Continue := False;
            Else
               -- Convert Command to Direction
               for Current_Direction in Direction loop
                  If Ada.Strings.Equal_Case_Insensitive(Current_Direction'Image, To_String(Command)) Then
                     Commanded_Direction := Current_Direction;
                     Valid_Direction := True;
                  End If;
               end loop;
               
               If Valid_Direction Then
                  -- See if the current location has a step in that direction
                  If Linked_Locations.Contains(Commanded_Direction) Then
                     -- Change our location to the location in the requested direction
                     New_Location_Key := Linked_Locations(Commanded_Direction);
                     Current_Location := Locations(New_Location_Key);                  
                  Else
                     Put_Line("You cannot go that way");
                  End If;
               Else
                  -- Help the user
                  Put("Try one of: ");
                  for Current_Direction in Direction loop
                     Put(Current_Direction'Image & " ");
                  end loop;
                  New_Line;                  
               End If;                              
            End If;
         End If;
                        
      end loop;
      
      Put_Line("Exiting");
   end;
      
end Game;
