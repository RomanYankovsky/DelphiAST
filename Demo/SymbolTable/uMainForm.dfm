object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'DelphiAST SymbolTable Demo'
  ClientHeight = 479
  ClientWidth = 754
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 251
    Width = 754
    Height = 3
    Cursor = crVSplit
    Align = alTop
    ExplicitWidth = 181
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 460
    Width = 754
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitTop = 370
    ExplicitWidth = 666
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 0
    Width = 754
    Height = 130
    Align = alTop
    Caption = 'Lookup Symbol'
    TabOrder = 1
    ExplicitTop = -6
    ExplicitWidth = 746
    object Label1: TLabel
      Left = 15
      Top = 20
      Width = 33
      Height = 13
      Caption = 'Scope:'
    end
    object Label2: TLabel
      Left = 15
      Top = 68
      Width = 68
      Height = 13
      Caption = 'Symbol Name:'
    end
    object Label3: TLabel
      Left = 446
      Top = 90
      Width = 120
      Height = 13
      Caption = '(for example, typename)'
    end
    object Label4: TLabel
      Left = 446
      Top = 44
      Width = 185
      Height = 13
      Caption = '(for example, unitname.functionname)'
    end
    object ScopeEdt: TEdit
      Left = 15
      Top = 41
      Width = 425
      Height = 21
      TabOrder = 0
    end
    object SymbolNameEdt: TEdit
      Left = 15
      Top = 87
      Width = 425
      Height = 21
      TabOrder = 1
    end
    object Lookup: TButton
      Left = 656
      Top = 85
      Width = 75
      Height = 25
      Caption = 'Lookup'
      TabOrder = 2
      OnClick = LookupClick
    end
  end
  object SymbolsBox: TListBox
    Left = 0
    Top = 130
    Width = 754
    Height = 121
    Align = alTop
    ItemHeight = 13
    TabOrder = 2
    OnClick = SymbolsBoxClick
    ExplicitTop = 136
    ExplicitWidth = 658
  end
  object DeclarationMemo: TMemo
    Left = 0
    Top = 254
    Width = 754
    Height = 206
    Align = alClient
    ScrollBars = ssBoth
    TabOrder = 3
    ExplicitTop = 252
    ExplicitWidth = 746
    ExplicitHeight = 178
  end
  object MainMenu: TMainMenu
    Left = 224
    Top = 96
    object OpenDelphiProject1: TMenuItem
      Caption = 'Open Delphi Project...'
      OnClick = OpenDelphiProject1Click
    end
  end
  object OpenDialog: TOpenDialog
    Filter = 'Delphi Project (*.dpr)|*.dpr'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 272
    Top = 96
  end
end
