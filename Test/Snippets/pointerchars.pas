unit pointerchars;

interface

implementation

procedure FormKeyPress(Sender: TObject; var Key: Char);
begin
   if Key = ^\ then 
   begin
      //some code
   end;

   if Key = ^M then 
   begin
      //some code
   end;

   if Key = ^] then 
   begin
      //some code
   end;
end;

end.