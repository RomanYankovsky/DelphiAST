unit ifcompilerversiondecimal;

interface

implementation

// Decimal operands in $IF CompilerVersion. The thresholds are far above and
// below any real compiler, so the expected branch is the same whatever the
// compiler version. The branch that must NOT be taken is invalid Pascal, so
// parsing fails if the lexer evaluates the condition wrongly.

{$IF CompilerVersion < 9999.5}
procedure Foo;
begin
end;
{$ELSE}
this branch must not be parsed
{$IFEND}

{$IF CompilerVersion > 9999.5}
this branch must not be parsed
{$ELSE}
procedure Foo2;
begin
end;
{$IFEND}

{$IF CompilerVersion >= 0.0}
procedure Foo3;
begin
end;
{$ELSE}
this branch must not be parsed
{$IFEND}

{$IF RTLVersion <= 9999.5}
procedure Foo4;
begin
end;
{$ELSE}
this branch must not be parsed
{$IFEND}

initialization

end.
