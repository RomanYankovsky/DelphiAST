unit SimplerParser.Lexer.Config;

{$mode delphi}

interface

uses
  Classes, SysUtils, Types;

type
  TDelphiParserOS = (osWindows, osLinux, osMac, osIOS, osAndroid);
  TDelphiParserCPUType = (ct32, ct64, ctARM32, ctARM64);

  { TDelphiParserConfig }

  TDelphiParserConfig = class (TObject)
  private
    FCompilerVersion: string;
    FCPU32: boolean;
    FCPU64: boolean;
    FFreePascal: boolean;

    FWIN32 : boolean;
    FWIN64 : boolean;
    FLINUX : boolean;
    FLINUX32 : boolean;
    FLINUX64 : boolean;
    FPOSIX : boolean;
    FPOSIX32 : boolean;
    FPOSIX64 : boolean;
    FCPUARM : boolean;
    FCPUARM32 : boolean;
    FCPUARM64 : boolean;
    FCPU386 : boolean;
    FCPUX86 : boolean;
    FCPUX64 : boolean;
    FMSWINDOWS : boolean;
    FMACOS : boolean;
    FMACOS32 : boolean;
    FMACOS64 : boolean;
    FIOS : boolean;
    FIOS32 : boolean;
    FIOS64 : boolean;
    FANDROID : boolean;
    FANDROID32 : boolean;
    FANDROID64 : boolean;
    FCONSOLE : boolean;
    FNATIVECODE : boolean;
    FCONDITIONALEXPRESSIONS : boolean;
    FUNICODE : boolean;
    FALIGN_STACK : boolean;
    FARM_NO_VFP_USE : boolean;
    FASSEMBLER : boolean;
    FAUTOREFCOUNT : boolean;
    FEXTERNALLINKER : boolean;
    FELF : boolean;
    FNEXTGEN : boolean;
    FPC_MAPPED_EXCEPTIONS : boolean;
    FPIC : boolean;
    FUNDERSCOREIMPORTNAME : boolean;
    FWEAKREF : boolean;
    FWEAKINSTREF : boolean;
    FWEAKINTFREF : boolean;

    FCustomDefines : TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadDefaults; // Set up the defines that are defined by the compiler based as compiling this code
    function GetDefines : TStringDynArray;

    procedure Configure(os : TDelphiParserOS; cpu : TDelphiParserCPUType);
    procedure ConfigureWin32;
    procedure ConfigureWin64;
    procedure ConfigureLinux64;
    procedure ConfigureMac64;
    procedure ConfigureMacM1;
    procedure ConfigureIOS;
    procedure ConfigureAndroid;


    property FreePascal : boolean read FFreePascal write FFreePascal;
    property CompilerVersion : string read FCompilerVersion write FCompilerVersion;

    property WIN32 : boolean read FWIN32 write FWIN32;
    property WIN64 : boolean read FWIN64 write FWIN64;
    property LINUX : boolean read FLINUX write FLINUX;
    property LINUX32 : boolean read FLINUX32 write FLINUX32;
    property LINUX64 : boolean read FLINUX64 write FLINUX64;
    property POSIX : boolean read FPOSIX write FPOSIX;
    property POSIX32 : boolean read FPOSIX32 write FPOSIX32;
    property POSIX64 : boolean read FPOSIX64 write FPOSIX64;
    property CPUARM : boolean read FCPUARM write FCPUARM;
    property CPUARM32 : boolean read FCPUARM32 write FCPUARM32;
    property CPUARM64 : boolean read FCPUARM64 write FCPUARM64;
    property CPU386 : boolean read FCPU386 write FCPU386;
    property CPUX86 : boolean read FCPUX86 write FCPUX86;
    property CPUX64 : boolean read FCPUX64 write FCPUX64;
    property CPU32 : boolean read FCPU32 write FCPU32;
    property CPU64 : boolean read FCPU64 write FCPU64;
    property MSWINDOWS : boolean read FMSWINDOWS write FMSWINDOWS;
    property MACOS : boolean read FMACOS write FMACOS;
    property MACOS32 : boolean read FMACOS32 write FMACOS32;
    property MACOS64 : boolean read FMACOS64 write FMACOS64;
    property IOS : boolean read FIOS write FIOS;
    property IOS32 : boolean read FIOS32 write FIOS32;
    property IOS64 : boolean read FIOS64 write FIOS64;
    property ANDROID : boolean read FANDROID write FANDROID;
    property ANDROID32 : boolean read FANDROID32 write FANDROID32;
    property ANDROID64 : boolean read FANDROID64 write FANDROID64;
    property CONSOLE : boolean read FCONSOLE write FCONSOLE;
    property NATIVECODE : boolean read FNATIVECODE write FNATIVECODE;
    property CONDITIONALEXPRESSIONS : boolean read FCONDITIONALEXPRESSIONS write FCONDITIONALEXPRESSIONS;
    property UNICODE : boolean read FUNICODE write FUNICODE;
    property ALIGN_STACK : boolean read FALIGN_STACK write FALIGN_STACK;
    property ARM_NO_VFP_USE : boolean read FARM_NO_VFP_USE write FARM_NO_VFP_USE;
    property ASSEMBLER : boolean read FASSEMBLER write FASSEMBLER;
    property AUTOREFCOUNT : boolean read FAUTOREFCOUNT write FAUTOREFCOUNT;
    property EXTERNALLINKER : boolean read FEXTERNALLINKER write FEXTERNALLINKER;
    property ELF : boolean read FELF write FELF;
    property NEXTGEN : boolean read FNEXTGEN write FNEXTGEN;
    property PC_MAPPED_EXCEPTIONS : boolean read FPC_MAPPED_EXCEPTIONS write FPC_MAPPED_EXCEPTIONS;
    property PIC : boolean read FPIC write FPIC;
    property UNDERSCOREIMPORTNAME : boolean read FUNDERSCOREIMPORTNAME write FUNDERSCOREIMPORTNAME;
    property WEAKREF : boolean read FWEAKREF write FWEAKREF;
    property WEAKINSTREF : boolean read FWEAKINSTREF write FWEAKINSTREF;
    property WEAKINTFREF : boolean read FWEAKINTFREF write FWEAKINTFREF;

    property CustomDefines : TStringList read FCustomDefines;
  end;

var
  DelphiParserConfig : TDelphiParserConfig;

implementation

function TDelphiParserConfig.GetDefines : TStringDynArray;
var
  ts : TStringList;
begin
  ts := TStringList.create;
  try
    ts.add('CODE_PARSER');
    if FFreePascal then
      ts.add('FPC');
    ts.add(CompilerVersion);

    if FWIN32 then ts.add('WIN32');
    if FWIN64 then ts.add('WIN64');
    if FLINUX then ts.add('LINUX');
    if FLINUX32 then ts.add('LINUX32');
    if FLINUX64 then ts.add('LINUX64');
    if FPOSIX then ts.add('POSIX');
    if FPOSIX32 then ts.add('POSIX32');
    if FPOSIX64 then ts.add('POSIX64');
    if FCPUARM then ts.add('CPUARM');
    if FCPUARM32 then ts.add('CPUARM32');
    if FCPUARM64 then ts.add('CPUARM64');
    if FCPU386 then ts.add('CPU386');
    if FCPUX86 then ts.add('CPUX86');
    if FCPUX64 then ts.add('CPUX64');
    if FCPU32 then ts.add('CPU32');
    if FCPU64 then ts.add('CPU64');
    if FCPU32 then ts.add('CPU32BITS');
    if FCPU64 then ts.add('CPU64BITS');
    if FMSWINDOWS then ts.add('MSWINDOWS');
    if FFreePascal then
    begin
      if FMACOS then ts.add('DARWIN');
      if FMACOS32 then ts.add('DARWIN32');
      if FMACOS64 then ts.add('DARWIN64');
    end
    else
    begin
      if FMACOS then ts.add('MACOS');
      if FMACOS32 then ts.add('MACOS32');
      if FMACOS64 then ts.add('MACOS64');
    end;
    if FIOS then ts.add('IOS');
    if FIOS32 then ts.add('IOS32');
    if FIOS64 then ts.add('IOS64');
    if FANDROID then ts.add('ANDROID');
    if FANDROID32 then ts.add('ANDROID32');
    if FANDROID64 then ts.add('ANDROID64');
    if FCONSOLE then ts.add('CONSOLE');
    if FNATIVECODE then ts.add('NATIVECODE');
    if FCONDITIONALEXPRESSIONS then ts.add('CONDITIONALEXPRESSIONS');
    if FUNICODE then ts.add('UNICODE');
    if FALIGN_STACK then ts.add('ALIGN_STACK');
    if FARM_NO_VFP_USE then ts.add('ARM_NO_VFP_USE');
    if FASSEMBLER then ts.add('ASSEMBLER');
    if FAUTOREFCOUNT then ts.add('AUTOREFCOUNT');
    if FEXTERNALLINKER then ts.add('EXTERNALLINKER');
    if FELF then ts.add('ELF');
    if FNEXTGEN then ts.add('NEXTGEN');
    if FPC_MAPPED_EXCEPTIONS then ts.add('PC_MAPPED_EXCEPTIONS');
    if FPIC then ts.add('PIC');
    if FUNDERSCOREIMPORTNAME then ts.add('UNDERSCOREIMPORTNAME');
    if FWEAKREF then ts.add('WEAKREF');
    if FWEAKINSTREF then ts.add('WEAKINSTREF');
    if FWEAKINTFREF then ts.add('WEAKINTFREF');

    ts.AddStrings(FCustomDefines);

    result := ts.ToStringArray;
  finally
    ts.free;
  end;
end;

constructor TDelphiParserConfig.Create;
begin
  inherited Create;
  FCustomDefines := TStringList.create;

end;

destructor TDelphiParserConfig.Destroy;
begin
  FCustomDefines.free;
  inherited Destroy;
end;

procedure TDelphiParserConfig.LoadDefaults;
begin
  FCustomDefines.clear;
  {$IFDEF FPC}
  FFreePascal := true;
  {$ELSE}
  FFreePascal := false;
  {$ENDIF}
  {$IFDEF VER90}
  CompilerVersion := 'VER90'; // 2
  {$ENDIF}
  {$IFDEF VER100}
  CompilerVersion := 'VER100'; // 3
  {$ENDIF}
  {$IFDEF VER120}
  CompilerVersion := 'VER120'; // 4
  {$ENDIF}
  {$IFDEF VER130}
  CompilerVersion := 'VER130'; // 5
  {$ENDIF}
  {$IFDEF VER140} // 6
  CompilerVersion := 'VER140';
  {$ENDIF}
  {$IFDEF VER150} // 7/7.1
  CompilerVersion := 'VER150';
  {$ENDIF}
  {$IFDEF VER160} // 8
  CompilerVersion := 'VER160';
  {$ENDIF}
  {$IFDEF VER170} // 2005
  CompilerVersion := 'VER170';
  {$ENDIF}
  {$IFDEF VER180} // 2007
  CompilerVersion := 'VER180';
  {$ENDIF}
  {$IFDEF VER185} // 2007
  CompilerVersion := 'VER185';
  {$ENDIF}
  {$IFDEF VER190} // 2007.NET
  CompilerVersion := 'VER190';
  {$ENDIF}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF COMPILERVERSION > 19.0}
    CompilerVersion := 'VER' + IntToStr(Round(10*CompilerVersion));
    {$IFEND}
  {$ENDIF}

{$IFDEF WIN32}
  FWIN32 := true;
{$ENDIF}
{$IFDEF WIN64}
  FWIN64 := true;
{$ENDIF}
{$IFDEF LINUX}
  FLINUX := true;
{$ENDIF}
{$IFDEF LINUX32}
  FLINUX32 := true;
{$ENDIF}
{$IFDEF LINUX64}
  FLINUX64 := true;
{$ENDIF}
{$IFDEF POSIX}
  FPOSIX := true;
{$ENDIF}
{$IFDEF POSIX32}
  FPOSIX32 := true;
{$ENDIF}
{$IFDEF POSIX64}
  FPOSIX64 := true;
{$ENDIF}
{$IFDEF CPUARM}
  FCPUARM := true;
{$ENDIF}
{$IFDEF CPUARM32}
  FCPUARM32 := true;
{$ENDIF}
{$IFDEF CPUARM64}
  FCPUARM64 := true;
{$ENDIF}
{$IFDEF CPU386}
  FCPU386 := true;
{$ENDIF}
{$IFDEF CPUX86}
  FCPUX86 := true;
{$ENDIF}
{$IFDEF CPUX64}
  FCPUX64 := true;
{$ENDIF}
{$IFDEF CPU32}
  FCPU32 := true;
{$ENDIF}
{$IFDEF CPU64}
  FCPU64 := true;
{$ENDIF}
{$IFDEF CPU32BITS}
  FCPU32BITS := true;
{$ENDIF}
{$IFDEF CPU64BITS}
  FCPU64BITS := true;
{$ENDIF}
{$IFDEF MSWINDOWS}
  FMSWINDOWS := true;
{$ENDIF}
{$IFDEF MACOS}
  FMACOS := true;
{$ENDIF}
{$IFDEF MACOS32}
  FMACOS32 := true;
{$ENDIF}
{$IFDEF MACOS64}
  FMACOS64 := true;
{$ENDIF}
{$IFDEF IOS}
  FIOS := true;
{$ENDIF}
{$IFDEF IOS32}
  FIOS32 := true;
{$ENDIF}
{$IFDEF IOS64}
  FIOS64 := true;
{$ENDIF}
{$IFDEF ANDROID}
  FANDROID := true;
{$ENDIF}
{$IFDEF ANDROID32}
  FANDROID32 := true;
{$ENDIF}
{$IFDEF ANDROID64}
  FANDROID64 := true;
{$ENDIF}
{$IFDEF CONSOLE}
  FCONSOLE := true;
{$ENDIF}
{$IFDEF NATIVECODE}
  FNATIVECODE := true;
{$ENDIF}
{$IFDEF CONDITIONALEXPRESSIONS}
  FCONDITIONALEXPRESSIONS := true;
{$ENDIF}
{$IFDEF UNICODE}
  FUNICODE := true;
{$ENDIF}
{$IFDEF ALIGN_STACK}
  FALIGN_STACK := true;
{$ENDIF}
{$IFDEF ARM_NO_VFP_USE}
  FARM_NO_VFP_USE := true;
{$ENDIF}
{$IFDEF ASSEMBLER}
  FASSEMBLER := true;
{$ENDIF}
{$IFDEF AUTOREFCOUNT}
  FAUTOREFCOUNT := true;
{$ENDIF}
{$IFDEF EXTERNALLINKER}
  FEXTERNALLINKER := true;
{$ENDIF}
{$IFDEF ELF}
  FELF := true;
{$ENDIF}
{$IFDEF NEXTGEN}
  FNEXTGEN := true;
{$ENDIF}
{$IFDEF PC_MAPPED_EXCEPTIONS}
  FPC_MAPPED_EXCEPTIONS := true;
{$ENDIF}
{$IFDEF PIC}
  FPIC := true;
{$ENDIF}
{$IFDEF UNDERSCOREIMPORTNAME}
  FUNDERSCOREIMPORTNAME := true;
{$ENDIF}
{$IFDEF WEAKREF}
  FWEAKREF := true;
{$ENDIF}
{$IFDEF WEAKINSTREF}
  FWEAKINSTREF := true;
{$ENDIF}
{$IFDEF WEAKINTFREF}
  FWEAKINTFREF := true;
{$ENDIF}
end;

procedure TDelphiParserConfig.Configure(os : TDelphiParserOS; cpu : TDelphiParserCPUType);
begin
  FCPU386 := false;
  FCPUX86 := false;
  FCPUX64 := false;
  FCPU32 := false;
  FCPU64 := false;
  FCPUARM := false;
  FCPUARM32 := false;
  FCPUARM64 := false;

  case cpu of
    ct32:
      begin
        FCPU386 := true;
        FCPUX86 := true;
        FCPU32 := true;
      end;
    ct64:
      begin
        FCPU386 := true;
        FCPUX64 := true;
        FCPU64 := true;
      end;
    ctARM32:
      begin
        FCPUARM := true;
        FCPUARM32 := true;
        FCPU32 := true;
      end;
    ctARM64:
      begin
        FCPUARM := true;
        FCPUARM64 := true;
        FCPU64 := true;
      end;
  end;


  FWIN32 := false;
  FWIN64 := false;
  FLINUX := false;
  FLINUX32 := false;
  FLINUX64 := false;
  FPOSIX := false;
  FPOSIX32 := false;
  FPOSIX64 := false;
  FCPUARM := false;
  FCPUARM32 := false;
  FCPUARM64 := false;
  FMSWINDOWS := false;
  FMACOS := false;
  FMACOS32 := false;
  FMACOS64 := false;
  FIOS := false;
  FIOS32 := false;
  FIOS64 := false;
  FANDROID := false;
  FANDROID32 := false;
  FANDROID64 := false;
  case os of
    osWindows :
      begin
        FMSWINDOWS := true;
        if cpu in [ct32, ctARM32] then FWIN32 := true else FWIN64 := true;
      end;
    osLinux :
      begin
        FLINUX := true;
        if cpu in [ct32, ctARM32] then FLINUX32 := true else FLINUX64 := true;
        FPOSIX := true;
        if cpu in [ct32, ctARM32] then FPOSIX32 := true else FPOSIX64 := true;
      end;
    osMac :
      begin
        FMACOS := true;
        if cpu in [ct32, ctARM32] then FMACOS32 := true else FMACOS64 := true;
        FPOSIX := true;
        if cpu in [ct32, ctARM32] then FPOSIX32 := true else FPOSIX64 := true;
      end;
    osIOS :
      begin
        FIOS := true;
        if cpu in [ct32, ctARM32] then FIOS32 := true else FIOS64 := true;
        FPOSIX := true;
        if cpu in [ct32, ctARM32] then FPOSIX32 := true else FPOSIX64 := true;
      end;
    osAndroid :
      begin
        FANDROID := true;
        if cpu in [ct32, ctARM32] then FANDROID32 := true else FANDROID64 := true;
      end;
  end;
end;

procedure TDelphiParserConfig.ConfigureWin32;
begin
  Configure(osWindows, ct32);
end;

procedure TDelphiParserConfig.ConfigureWin64;
begin
  Configure(osWindows, ct64);
end;

procedure TDelphiParserConfig.ConfigureLinux64;
begin
  Configure(osLinux, ct64);
end;

procedure TDelphiParserConfig.ConfigureMac64;
begin
  Configure(osMac, ct64);
end;

procedure TDelphiParserConfig.ConfigureMacM1;
begin
  Configure(osMac, ctARM64);
end;

procedure TDelphiParserConfig.ConfigureIOS;
begin
  Configure(osIOS, ct64);
end;

procedure TDelphiParserConfig.ConfigureAndroid;
begin
  Configure(osAndroid, ct64);
end;

initialization
  DelphiParserConfig := TDelphiParserConfig.create;
finalization
  DelphiParserConfig.Free;
end.

