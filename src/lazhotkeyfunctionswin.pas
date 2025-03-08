unit LazHotKeyFunctionsWin;

{$mode ObjFPC}{$H+}


interface

uses
  Classes, SysUtils, Windows, LCLType, LCLIntf, Menus,
  LazHotKeyType, LazHotKeyFunctions;

  function KeyCodeToNativeShortCut(const AKeycode: UINT; const AModifiers: UINT): TNativeShortCut;
  procedure NativeShortCutToKeyCode(const AShortCut: TNativeShortCut; out AKeycode: UINT; out AModifiers: UINT);
  function ShortCutToNativeShortCut(const AShortCut: TShortCut): TNativeShortCut;
  function VKKeycodeAndModifiersToVKHotKey(const AKeycode, AWinModifiers: DWord): DWord;
  procedure VKHotKeyToVKKeycodeAndShiftState(const AVKHotKey: DWord; out AKeycode: Word; out AShiftState: TShiftState);
  procedure VKHotKeyToVKKeycodeAndWinModifiers(const AVKHotKey: DWord; out AKeycode: Word; out AWinModifiers: DWord);
  function ShiftStateToWinModifiers(const AShortCut: TShortCut): UINT; overload;
  function ShiftStateToWinModifiers(const AShiftState: TShiftState): DWord;
  function WinModifiersToShiftState(const AWinModifiers: DWord): TShiftState;

implementation

function KeyCodeToNativeShortCut(const AKeycode: UINT; const AModifiers: UINT): TNativeShortCut;
begin

  if (AKeycode and $FF00) <> 0 then
    Exit(0);
  Result := AModifiers shl 8;
  Inc(Result, AKeycode);

end;

procedure NativeShortCutToKeyCode(const AShortCut: TNativeShortCut; out AKeycode: UINT; out AModifiers: UINT);
begin

  AKeycode := AShortCut and $00FF;
  AModifiers := AShortCut shr 8;

end;

function ShortCutToNativeShortCut(const AShortCut: TShortCut): TNativeShortCut;
var
  iKeyCode: Word;
  LShiftState: TShiftState;
  iModifiers: UINT;
begin

  ShortCutToKey(AShortCut, iKeyCode, LShiftState);
  iModifiers := ShiftStateToWinModifiers(LShiftState);

  Result := KeyCodeToNativeShortCut(iKeyCode, iModifiers);

end;

function VKKeycodeAndModifiersToVKHotKey(const AKeycode, AWinModifiers: DWord): DWord;
begin

  if (AKeycode and $FFFF0000 <> 0) then
    Exit(0);
  if (AWinModifiers and $FFFFFFF0 <> 0) then
    Exit(0);

  Result := AKeycode shl 16;
  Inc(Result, AWinModifiers);

end;

procedure VKHotKeyToVKKeycodeAndShiftState(const AVKHotKey: DWord; out AKeycode: Word; out AShiftState: TShiftState);
begin

  AKeycode := LParamHiWord(AVKHotKey);
  AShiftState := WinModifiersToShiftState(LParamLoWord(AVKHotKey));

end;

procedure VKHotKeyToVKKeycodeAndWinModifiers(const AVKHotKey: DWord; out AKeycode: Word; out AWinModifiers: DWord);
begin

  AKeycode := LParamHiWord(AVKHotKey);
  AWinModifiers := LParamLoWord(AVKHotKey);

end;

function ShiftStateToWinModifiers(const AShortCut: TShortCut): UINT; overload;
var
  LShiftState: TShiftState;
  iKeyCode: Word;
begin

  ShortCutToKey(AShortCut, iKeyCode, LShiftState);
  Result := ShiftStateToWinModifiers(LShiftState);

end;

function ShiftStateToWinModifiers(const AShiftState: TShiftState): DWord;
begin

  Result := 0;

  // Alt, AltGr, Meta, Ctrl, Shift, Super (Win) and Hyper
  if (ssAlt in AShiftState) or (ssMeta in AShiftState) or (ssAltGr in AShiftState) then Result := Result or MOD_ALT;
  if (ssCtrl in AShiftState)  then Result := Result or MOD_CONTROL;
  if (ssShift in AShiftState) then Result := Result or MOD_SHIFT;
  if (ssSuper in AShiftState) or (ssHyper in AShiftState) then Result := Result or MOD_WIN;

end;

function WinModifiersToShiftState(const AWinModifiers: DWord): TShiftState;
begin

  Result := [];

  if AWinModifiers and MOD_ALT <> 0 then Result := Result + [ssAlt];
  if AWinModifiers and MOD_CONTROL <> 0 then Result := Result + [ssCtrl];
  if AWinModifiers and MOD_SHIFT <> 0 then Result := Result + [ssShift];
  if AWinModifiers and MOD_WIN <> 0 then Result := Result + [ssSuper];

end;

end.

