unit LazHotKeyPlatform;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LazHotKeyType;

type
  THotKeyPlatform = class
  protected var
    FProcessId: Integer; // Avoiding FPC GetProcessID to avoid convertion from 64 to 32 bit integer.
    FFormHandle: HWND;
    FHotKeyList: THotKeyList;
    FEventOutput: Boolean;
    FOnHotKeyEvent: THotKeyEvent;
  protected
    function GetProcessId: Integer; virtual; abstract;
    procedure SetEventData; virtual; {abstract;}
    procedure SetEventOutput(const AValue: Boolean);
    procedure DoTriggerEvent(AHotKeyId: Word);
  protected
    // Readonly properties
  public
    constructor Create(const AFormHandle: HWND; AHotKeyList: THotKeyList);
    function GetNativeShortCut(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): TNativeShortCut; virtual; abstract;
    function GetShortCut(const ANativeShortCut: TNativeShortCut; const APlatformAdjusted: Boolean): TShortCut; virtual; abstract;
    function GetError: LongInt; virtual; abstract;
    procedure StartHandler; virtual;
    procedure StopHandler; virtual;
    function DoRegisterGlobalHotKey(const AHotKeyInfo: PTHotKeyInfo): Boolean; virtual; abstract;
    function DoUnregisterGlobalHotkey(const AHotKeyInfo: PTHotKeyInfo): Boolean; virtual; abstract;
  public
    property OnHotKeyEvent: THotKeyEvent read FOnHotKeyEvent write FOnHotKeyEvent;
  public
    property EventOutput: Boolean read FEventOutput write SetEventOutput;
    property FormHandle: HWND read FFormHandle;
    property HotKeyList: THotKeyList read FHotKeyList;
  end;

implementation

constructor THotKeyPlatform.Create(const AFormHandle: HWND; AHotKeyList: THotKeyList);
begin
  FEventOutput := True;
  FFormHandle := AFormHandle;
  FHotKeyList := AHotKeyList;
end;

procedure THotKeyPlatform.SetEventData;
begin
  //
end;

procedure THotKeyPlatform.SetEventOutput(const AValue: Boolean);
begin
  if AValue <> FEventOutput then
  begin
    FEventOutput := AValue;
    SetEventData;
  end;
end;

procedure THotKeyPlatform.StartHandler;
begin
  // Empty method, for case when there is no need for platform handler.
end;

procedure THotKeyPlatform.StopHandler;
begin
  // Empty method, for case when there is no need for platform handler.
end;

procedure THotKeyPlatform.DoTriggerEvent(AHotKeyId: Word);
var
  iHotKeyIndex: Integer;
begin

  if Assigned(FOnHotKeyEvent) then
  begin
    iHotKeyIndex := FHotKeyList.IndexById(AHotKeyId);
    if iHotKeyIndex >= 0 then
      FOnHotKeyEvent( FHotKeyList.List[iHotKeyIndex] );
  end;

end;

end.

