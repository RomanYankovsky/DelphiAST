### Abstract Syntax Tree Builder for Delphi 
With DelphiAST you can take real Delphi code and get an abstract syntax tree. One unit at time and without a symbol table though. 

Free Pascal and Lazarus compatible.

#### Sample input
```delphi
unit Unit1;

interface

uses
  Unit2;

function Sum(A, B: Integer): Integer;

implementation

function Sum(A, B: Integer): Integer;
begin
  Result := A + B;
end;

end.
```

#### Sample outcome
```xml
<UNIT line="1" col="1" name="Unit1">
  <INTERFACE line="3" col="1">
    <USES line="5" col="1">
      <UNIT line="6" col="3" name="Unit2"/>
    </USES>
    <METHOD line="8" col="1" name="Sum" kind="function">
      <PARAMETERS line="8" col="13">
        <PARAMETER>
          <NAME line="8" col="14" value="A"/>
          <TYPE line="8" col="20" name="Integer"/>
        </PARAMETER>
        <PARAMETER>
          <NAME line="8" col="17" value="B"/>
          <TYPE line="8" col="20" name="Integer"/>
        </PARAMETER>
      </PARAMETERS>
      <RETURNTYPE line="8" col="30">
        <TYPE line="8" col="30" name="Integer"/>
      </RETURNTYPE>
    </METHOD>
  </INTERFACE>
  <IMPLEMENTATION line="10" col="1">
    <METHOD line="12" col="1" name="Sum" kind="function">
      <PARAMETERS line="12" col="13">
        <PARAMETER>
          <NAME line="12" col="14" value="A"/>
          <TYPE line="12" col="20" name="Integer"/>
        </PARAMETER>
        <PARAMETER>
          <NAME line="12" col="17" value="B"/>
          <TYPE line="12" col="20" name="Integer"/>
        </PARAMETER>
      </PARAMETERS>
      <RETURNTYPE line="12" col="30">
        <TYPE line="12" col="30" name="Integer"/>
      </RETURNTYPE>
      <STATEMENTS end_line="15" begin_line="14" end_col="1" begin_col="3">
        <ASSIGN line="14" col="3">
          <LHS>
            <IDENTIFIER line="14" col="3" name="Result"/>
          </LHS>
          <RHS>
            <EXPRESSION line="14" col="13">
              <ADD line="14" col="15">
                <IDENTIFIER line="14" col="13" name="A"/>
                <IDENTIFIER line="14" col="17" name="B"/>
              </ADD>
            </EXPRESSION>
          </RHS>
        </ASSIGN>
      </STATEMENTS>
    </METHOD>
  </IMPLEMENTATION>
</UNIT>
```

#### Copyright
Copyright (c) 2014-2015 Roman Yankovsky (roman@yankovsky.me)

DelphiAST is released under the Mozilla Public License, v. 2.0

See LICENSE for details.
