### Abstract Syntax Tree Builder for Delphi
With DelphiAST you can take real Delphi code and get an abstract syntax tree. One unit at time and without a symbol table though. 

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
<UNIT line="0" col="0" name="Unit1">
  <INTERFACE line="2" col="0">
    <USES line="4" col="0">
      <UNIT line="5" col="2" name="Unit2"/>
    </USES>
    <METHOD line="7" col="0" name="Sum" kind="function">
      <NAME line="7" col="9">
        <NAME line="7" col="9" value="Sum"/>
      </NAME>
      <PARAMETERS line="7" col="12">
        <PARAMETER>
          <NAME line="7" col="13" value="A"/>
          <TYPE line="7" col="19" name="Integer"/>
        </PARAMETER>
        <PARAMETER>
          <NAME line="7" col="16" value="B"/>
          <TYPE line="7" col="19" name="Integer"/>
        </PARAMETER>
      </PARAMETERS>
      <TYPE line="7" col="29" name="Integer"/>
    </METHOD>
  </INTERFACE>
  <IMPLEMENTATION line="9" col="0">
    <METHOD line="11" col="0" name="Sum" kind="function">
      <NAME line="11" col="9">
        <NAME line="11" col="9" value="Sum"/>
      </NAME>
      <PARAMETERS line="11" col="12">
        <PARAMETER>
          <NAME line="11" col="13" value="A"/>
          <TYPE line="11" col="19" name="Integer"/>
        </PARAMETER>
        <PARAMETER>
          <NAME line="11" col="16" value="B"/>
          <TYPE line="11" col="19" name="Integer"/>
        </PARAMETER>
      </PARAMETERS>
      <TYPE line="11" col="29" name="Integer"/>
      <STATEMENTS end_line="14" begin_line="13" end_col="0" begin_col="2">
        <ASSIGN line="13" col="2">
          <LHS>
            <IDENTIFIER line="13" col="2" name="Result"/>
          </LHS>
          <RHS>
            <EXPRESSION line="13" col="12">
              <ADD line="13" col="14">
                <IDENTIFIER line="13" col="12" name="A"/>
                <IDENTIFIER line="13" col="16" name="B"/>
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
