unit LazHotKeyType;

{$mode ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, fgl, LCLType, Menus;

  const
    MESSAGE_KEYPRESS_UNKNOWN = 201;
    MESSAGE_KEYPRESS_WIN32 = 202;
    MESSAGE_KEYPRESS_UNIX_X = 203;
    MESSAGE_KEYPRESS_DARWIN_CARBON = 204;
    MESSAGE_HOTKEY_VK = $2001;
    MESSAGE_HOTKEY_PLATFORMDATA = $2002;
    MOD_WIN_ALT = 1;
    MOD_WIN_CONTROL = 2;
    MOD_WIN_SHIFT = 4;
    MOD_WIN_SUPER = 8;

  type

    TNativeShortCut = type Low(DWord)..High(DWord);

    PTHotKeyInfo = ^THotKeyInfo;
    THotKeyInfo = packed record
      Id: Word;
      ShortCut: TShortCut;
      NativeShortCut: TNativeShortCut;
      PlatformAdjusted: Boolean;
      PlatformData: Pointer;
      class operator = (const AItemLeft, AItemRight: THotKeyInfo): Boolean;
      function GetShortCutKeyCode: Word;
      function GetShortCutShiftState: TShiftState;
      function GetNativeKeyCode: LongWord;
      function GetNativeModifiers: LongWord;
    end;

    THotKeyCustomList = specialize TFPGList<THotKeyInfo>;
    THotKeyList = class(THotKeyCustomList)
    public
      function IndexById(AId: Integer): Integer;
      function IndexByNativeShortCut(ANativeShortCut: TNativeShortCut): Integer;
      function IndexByShortCut(AShortCut: TShortCut): Integer;
    end;

    THotKeyEvent = procedure(AHotKeyInfo: PTHotKeyInfo) of object; // Review: Add Sender?
    THotKeyRegisterEvent = procedure(AHotKeyInfo: PTHotKeyInfo; ASuccess: Boolean; AErrorCode: LongInt) of object;

implementation

{$IfDef Unix}
{$IfDef Darwin}
uses
  LazHotKeyFunctionsDarwin;
{$Else}
uses
  LazHotKeyFunctionsUnix;
{$EndIf}
{$EndIf}
{$IfDef MSWindows}
uses
  LazHotKeyFunctionsWin;
{$EndIf}
{$IfDef Debug}
{$EndIf}

class operator THotKeyInfo.=(const AItemLeft, AItemRight: THotKeyInfo): Boolean;
begin
  Result := (AItemLeft.NativeShortCut = AItemRight.NativeShortCut);
end;

function THotKeyInfo.GetShortCutKeyCode: Word;
var
  LShiftState: TShiftState;
begin

  if ShortCut in [VK_UNKNOWN, VK_UNDEFINED] then
    Exit(Word(ShortCut));

  ShortCutToKey(ShortCut, Result, LShiftState);

end;

function THotKeyInfo.GetShortCutShiftState: TShiftState;
var
  wKeycode: Word;
begin

  Result := [];

  if ShortCut in [VK_UNKNOWN, VK_UNDEFINED] then
    Exit;

  ShortCutToKey(ShortCut, wKeycode, Result);

end;

function THotKeyInfo.GetNativeKeyCode: LongWord;
var
  wModifiers: LongWord;
begin

  NativeShortCutToKeyCode(NativeShortCut, Result, wModifiers);

end;

function THotKeyInfo.GetNativeModifiers: LongWord;
var
  wKeycode: LongWord;
begin

  NativeShortCutToKeyCode(NativeShortCut, wKeycode, Result);

end;

function THotKeyList.IndexById(AId: Integer): Integer;
begin

  Result := 0;
  while (Result < FCount) and ({%H-}Items[Result].Id <> AId) do
    Inc(Result);
  if Result = FCount then
    Result := -1;

end;

function THotKeyList.IndexByNativeShortCut(ANativeShortCut: TNativeShortCut): Integer;
begin

  Result := 0;
  while (Result < FCount) and ({%H-}Items[Result].NativeShortCut <> ANativeShortCut) do
    Inc(Result);
  if Result = FCount then
    Result := -1;

end;

function THotKeyList.IndexByShortCut(AShortCut: TShortCut): Integer;
begin

  Result := 0;
  while (Result < FCount) and ({%H-}Items[Result].ShortCut <> AShortCut) do
    Inc(Result);
  if Result = FCount then
    Result := -1;

end;


end.

