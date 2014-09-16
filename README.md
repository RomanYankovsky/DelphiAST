### Abstract Syntax Tree builder for Delphi
With DelphiAST you can take real Delphi code and get an abstract syntax tree. One unit at time and without a symbol table though. 

#### Sample input:
```delphi
unit Unit1;

interface

function Sum(A, B: Integer): Integer;

implementation

function Sum(A, B: Integer): Integer;
begin
  Result := A + B;
end;

end.
```
#### Sample outcome:
```xml
<?xml version="1.0"?>
<UNIT line="0" col="5" name="Unit1">
  <INTERFACE line="2" col="0">
    <METHOD line="4" col="0" name="Sum" kind="function">
      <NAME line="4" col="9">
        <NAME line="4" col="9" value="Sum"/>
      </NAME>
      <PARAMETERS line="4" col="27">
        <PARAMETER>
          <NAME line="4" col="13" value="A"/>
          <TYPE line="4" col="19" name="Integer"/>
        </PARAMETER>
        <PARAMETER>
          <NAME line="4" col="16" value="B"/>
          <TYPE line="4" col="19" name="Integer"/>
        </PARAMETER>
      </PARAMETERS>
      <TYPE line="4" col="29" name="Integer"/>
    </METHOD>
  </INTERFACE>
  <IMPLEMENTATION line="6" col="0">
    <METHOD line="8" col="0" name="Sum" kind="function">
      <NAME line="8" col="9">
        <NAME line="8" col="9" value="Sum"/>
      </NAME>
      <PARAMETERS line="8" col="27">
        <PARAMETER>
          <NAME line="8" col="13" value="A"/>
          <TYPE line="8" col="19" name="Integer"/>
        </PARAMETER>
        <PARAMETER>
          <NAME line="8" col="16" value="B"/>
          <TYPE line="8" col="19" name="Integer"/>
        </PARAMETER>
      </PARAMETERS>
      <TYPE line="8" col="29" name="Integer"/>
      <STATEMENTS end_line="11" begin_line="10" end_col="0" begin_col="2">
        <ASSIGN line="10" col="2">
          <LHS>
            <IDENTIFIER line="10" col="2" name="Result"/>
          </LHS>
          <RHS>
            <EXPRESSION line="10" col="12">
              <ADD line="10" col="14">
                <IDENTIFIER line="10" col="12" name="A"/>
                <IDENTIFIER line="10" col="16" name="B"/>
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
Copyright (c) 2014 Roman Yankovsky (roman@yankovsky.me)

DelphiAST is released under the Mozilla Public License, v. 2.0
See LICENSE for details.