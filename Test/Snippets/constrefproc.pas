unit constrefproc;

interface
 
type
  TClass = class 
  public
    procedure Make(constref Thing, OtherThing: TObject; 
                   var VarParam: integer; 
                   const constParam: string;
                   out outParam: IUnknown);
  end;

implementation

procedure TClass.Make(constref Thing, OtherThing: TObject; 
                      var VarParam: integer; 
                      const constParam: string;
                      out outParam: IUnknown);
begin
  
end;

end.
