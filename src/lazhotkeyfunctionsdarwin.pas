unit LazHotKeyFunctionsDarwin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, Menus, MacOSAll,
  LazHotKeyType;

const
  kVK_HK_Power = $7F;

  function KeyCodeToNativeShortCut(const AKeycode: UInt32; const AModifiers: UInt32): TNativeShortCut;
  procedure NativeShortCutToKeyCode(const AShortCut: TNativeShortCut; out AKeycode: UInt32; out AModifiers: UInt32);
  function ShortCutToNativeShortCut(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): TNativeShortCut;
  function NativeShortCutToShortCut(const ANativeShortCut: TNativeShortCut; const APlatformAdjusted: Boolean): TShortCut;
  function VKKeycodeToMacKeycode(const AVKKeycode: Word): UInt32;
  function MacKeycodeToVKKeycode(const AMacKeycode: UInt32): Word;
  function ShiftStateToMacModifiers(const AShiftState: TShiftState; const APlatformAdjusted: Boolean{ = True}): UInt32; overload;
  function ShiftStateToMacModifiers(const AShortCut: TShortCut; const APlatformAdjusted: Boolean{ = True}): UInt32; overload;
  function MacModifiersToShiftState(const AModifiers: UInt32; const APlatformAdjusted: Boolean{ = True}): TShiftState;
  function MacModifiersToWinModifiers(const AModifiers: UInt32; const APlatformAdjusted: Boolean{ = True}): DWord;

implementation

function KeyCodeToNativeShortCut(const AKeycode: UInt32; const AModifiers: UInt32): TNativeShortCut;
begin

  if (AKeycode and $FF00) <> 0 then
    Exit(0);
  Result := AModifiers and $FF00;
  Inc(Result, AKeycode);

end;

procedure NativeShortCutToKeyCode(const AShortCut: TNativeShortCut; out AKeycode: UInt32; out AModifiers: UInt32);
begin

  AKeycode := AShortCut and $00FF;
  AModifiers := AShortCut and $FF00;

end;

function ShortCutToNativeShortCut(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): TNativeShortCut;
var
  iKeyCode: Word;
  LShiftState: TShiftState;
  iModifiers: UInt32;
begin

  ShortCutToKey(AShortCut, iKeyCode, LShiftState);
  iModifiers := ShiftStateToMacModifiers(LShiftState, APlatformAdjusted);
  iKeyCode := VKKeycodeToMacKeycode(iKeyCode);

  Result := KeyCodeToNativeShortCut(iKeyCode, iModifiers);

end;

function NativeShortCutToShortCut(const ANativeShortCut: TNativeShortCut; const APlatformAdjusted: Boolean): TShortCut;
var
  iMacKeyCode: UInt32;
  iMacModifiers: UInt32;
  wKeyCode: Word;
  LShiftState: TShiftState;
begin

  NativeShortCutToKeyCode(ANativeShortCut, iMacKeyCode, iMacModifiers);
  wKeyCode := MacKeycodeToVKKeycode(iMacKeyCode);
  LShiftState := MacModifiersToShiftState(iMacModifiers, APlatformAdjusted);

  Result := KeyToShortCut(wKeyCode, LShiftState);

end;

function VKKeycodeToMacKeycode(const AVKKeycode: Word): UInt32;
begin


  //https://gist.github.com/eegrok/949034

  case AVKKeycode of

    VK_BACK: Result := kVK_Delete;
    VK_TAB: Result := kVK_Tab;

    VK_RETURN: Result := kVK_Return;

    VK_SHIFT: Result := kVK_Shift;
    VK_CONTROL: Result := kVK_Control;
    VK_MENU: Result := kVK_Option;
    VK_PAUSE: Result := kVK_F15; // There is no Pause/Break. Instead it's F15.
    VK_CAPITAL: Result := kVK_CapsLock;

    VK_KANA: Result := kVK_JIS_Kana;
    VK_KANJI: Result := kVK_JIS_Kana;

    VK_ESCAPE: Result := kVK_Escape;
    VK_SPACE: Result := kVK_Space;
    VK_PRIOR: Result := kVK_PageUp;
    VK_NEXT: Result := kVK_PageDown;
    VK_END: Result := kVK_End;
    VK_HOME: Result := kVK_Home;
    VK_LEFT: Result := kVK_LeftArrow;
    VK_UP: Result := kVK_UpArrow;
    VK_RIGHT: Result := kVK_RightArrow;
    VK_DOWN: Result := kVK_DownArrow;

    VK_SNAPSHOT: Result := kVK_F13; // There is no Print Screen. Instead it's F13. Although sometimes Screenshot itself could be mapped to F11. But PrSc key is F13.
    VK_INSERT: Result := kVK_Help;
    VK_DELETE: Result := kVK_ForwardDelete;

    // Numbers and Letters
    VK_0: Result := kVK_ANSI_0;
    VK_1: Result := kVK_ANSI_1;
    VK_2: Result := kVK_ANSI_2;
    VK_3: Result := kVK_ANSI_3;
    VK_4: Result := kVK_ANSI_4;
    VK_5: Result := kVK_ANSI_5;
    VK_6: Result := kVK_ANSI_6;
    VK_7: Result := kVK_ANSI_7;
    VK_8: Result := kVK_ANSI_8;
    VK_9: Result := kVK_ANSI_9;
    VK_A: Result := kVK_ANSI_A;
    VK_B: Result := kVK_ANSI_B;
    VK_C: Result := kVK_ANSI_C;
    VK_D: Result := kVK_ANSI_D;
    VK_E: Result := kVK_ANSI_E;
    VK_F: Result := kVK_ANSI_F;
    VK_G: Result := kVK_ANSI_G;
    VK_H: Result := kVK_ANSI_H;
    VK_I: Result := kVK_ANSI_I;
    VK_J: Result := kVK_ANSI_J;
    VK_K: Result := kVK_ANSI_K;
    VK_L: Result := kVK_ANSI_L;
    VK_M: Result := kVK_ANSI_M;
    VK_N: Result := kVK_ANSI_N;
    VK_O: Result := kVK_ANSI_O;
    VK_P: Result := kVK_ANSI_P;
    VK_Q: Result := kVK_ANSI_Q;
    VK_R: Result := kVK_ANSI_R;
    VK_S: Result := kVK_ANSI_S;
    VK_T: Result := kVK_ANSI_T;
    VK_U: Result := kVK_ANSI_U;
    VK_V: Result := kVK_ANSI_V;
    VK_W: Result := kVK_ANSI_W;
    VK_X: Result := kVK_ANSI_X;
    VK_Y: Result := kVK_ANSI_Y;
    VK_Z: Result := kVK_ANSI_Z;

    VK_LWIN: Result := kVK_Command;
    VK_RWIN: Result := kVK_Command;

    VK_NUMPAD0: Result := kVK_ANSI_Keypad0;
    VK_NUMPAD1: Result := kVK_ANSI_Keypad1;
    VK_NUMPAD2: Result := kVK_ANSI_Keypad2;
    VK_NUMPAD3: Result := kVK_ANSI_Keypad3;
    VK_NUMPAD4: Result := kVK_ANSI_Keypad4;
    VK_NUMPAD5: Result := kVK_ANSI_Keypad5;
    VK_NUMPAD6: Result := kVK_ANSI_Keypad6;
    VK_NUMPAD7: Result := kVK_ANSI_Keypad7;
    VK_NUMPAD8: Result := kVK_ANSI_Keypad8;
    VK_NUMPAD9: Result := kVK_ANSI_Keypad9;
    VK_MULTIPLY: Result := kVK_ANSI_KeypadMultiply;
    VK_ADD: Result := kVK_ANSI_KeypadPlus;
    VK_SEPARATOR: Result := kVK_ANSI_KeypadEnter;
    VK_SUBTRACT: Result := kVK_ANSI_KeypadMinus;
    VK_DECIMAL: Result := kVK_ANSI_KeypadDecimal;
    VK_DIVIDE: Result := kVK_ANSI_KeypadDivide;

    // F21 to F24 are not defined in macOS
    VK_F1: Result := kVK_F1;
    VK_F2: Result := kVK_F2;
    VK_F3: Result := kVK_F3;
    VK_F4: Result := kVK_F4;
    VK_F5: Result := kVK_F5;
    VK_F6: Result := kVK_F6;
    VK_F7: Result := kVK_F7;
    VK_F8: Result := kVK_F8;
    VK_F9: Result := kVK_F9;
    VK_F10: Result := kVK_F10;
    VK_F11: Result := kVK_F11;
    VK_F12: Result := kVK_F12;
    VK_F13: Result := kVK_F13;
    VK_F14: Result := kVK_F14;
    VK_F15: Result := kVK_F15;
    VK_F16: Result := kVK_F16;
    VK_F17: Result := kVK_F17;
    VK_F18: Result := kVK_F18;
    VK_F19: Result := kVK_F19;
    VK_F20: Result := kVK_F20;

    VK_NUMLOCK: Result := kVK_ANSI_KeypadClear;
    VK_SCROLL: Result := kVK_F14; // There is no Scroll Lock. Instead it's F14.

    VK_LSHIFT: Result := kVK_Shift;
    VK_RSHIFT: Result := kVK_RightShift;
    VK_LCONTROL: Result := kVK_Control;
    VK_RCONTROL: Result := kVK_RightControl;
    VK_LMENU: Result := kVK_Option;
    VK_RMENU: Result := kVK_RightOption;

    VK_VOLUME_MUTE: Result := kVK_Mute;
    VK_VOLUME_DOWN: Result := kVK_VolumeDown;
    VK_VOLUME_UP: Result := kVK_VolumeUp;

    VK_OEM_1: Result := kVK_ANSI_Semicolon;
    VK_OEM_PLUS: Result := kVK_ANSI_Equal;
    VK_OEM_COMMA: Result := kVK_ANSI_Comma;
    VK_OEM_MINUS: Result := kVK_ANSI_Minus;
    VK_OEM_PERIOD: Result := kVK_ANSI_Period;
    VK_OEM_2: Result := kVK_ANSI_Slash;
    VK_OEM_3: Result := kVK_ANSI_Grave;
    VK_OEM_4: Result := kVK_ANSI_LeftBracket;
    VK_OEM_5: Result := kVK_ANSI_Backslash;
    VK_OEM_6: Result := kVK_ANSI_RightBracket;
    VK_OEM_7: Result := kVK_ANSI_Quote;
    VK_OEM_102: Result := kVK_ANSI_Backslash;

    VK_LCL_POWER: Result := kVK_HK_Power;

    else Result := 0;

  end;

end;

function MacKeycodeToVKKeycode(const AMacKeycode: UInt32): Word;
begin

  case AMacKeycode of

    kVK_Delete: Result := VK_BACK;
    kVK_Tab: Result := VK_TAB;
    kVK_Return: Result := VK_RETURN;

    // Shift, Control and Option means left buttons. For right there are separate codes. This is not related to modifiers.
    // kVK_Shift: Result := VK_SHIFT;
    // kVK_Control: Result := VK_CONTROL;
    // kVK_Option: Result := VK_MENU;

    kVK_F15: Result := VK_PAUSE;
    kVK_CapsLock: Result := VK_CAPITAL;
    kVK_JIS_Kana: Result := VK_KANA;
    // kVK_JIS_Kana: Result := VK_KANJI;
    kVK_Escape: Result := VK_ESCAPE;
    kVK_Space: Result := VK_SPACE;
    kVK_PageUp: Result := VK_PRIOR;
    kVK_PageDown: Result := VK_NEXT;
    kVK_End: Result := VK_END;
    kVK_Home: Result := VK_HOME;
    kVK_LeftArrow: Result := VK_LEFT;
    kVK_UpArrow: Result := VK_UP;
    kVK_RightArrow: Result := VK_RIGHT;
    kVK_DownArrow: Result := VK_DOWN;

    kVK_F13: Result := VK_SNAPSHOT;
    kVK_Help: Result := VK_INSERT;
    kVK_ForwardDelete: Result := VK_DELETE;

    kVK_ANSI_0: Result := VK_0;
    kVK_ANSI_1: Result := VK_1;
    kVK_ANSI_2: Result := VK_2;
    kVK_ANSI_3: Result := VK_3;
    kVK_ANSI_4: Result := VK_4;
    kVK_ANSI_5: Result := VK_5;
    kVK_ANSI_6: Result := VK_6;
    kVK_ANSI_7: Result := VK_7;
    kVK_ANSI_8: Result := VK_8;
    kVK_ANSI_9: Result := VK_9;

    kVK_ANSI_A: Result := VK_A;
    kVK_ANSI_B: Result := VK_B;
    kVK_ANSI_C: Result := VK_C;
    kVK_ANSI_D: Result := VK_D;
    kVK_ANSI_E: Result := VK_E;
    kVK_ANSI_F: Result := VK_F;
    kVK_ANSI_G: Result := VK_G;
    kVK_ANSI_H: Result := VK_H;
    kVK_ANSI_I: Result := VK_I;
    kVK_ANSI_J: Result := VK_J;
    kVK_ANSI_K: Result := VK_K;
    kVK_ANSI_L: Result := VK_L;
    kVK_ANSI_M: Result := VK_M;
    kVK_ANSI_N: Result := VK_N;
    kVK_ANSI_O: Result := VK_O;
    kVK_ANSI_P: Result := VK_P;
    kVK_ANSI_Q: Result := VK_Q;
    kVK_ANSI_R: Result := VK_R;
    kVK_ANSI_S: Result := VK_S;
    kVK_ANSI_T: Result := VK_T;
    kVK_ANSI_U: Result := VK_U;
    kVK_ANSI_V: Result := VK_V;
    kVK_ANSI_W: Result := VK_W;
    kVK_ANSI_X: Result := VK_X;
    kVK_ANSI_Y: Result := VK_Y;
    kVK_ANSI_Z: Result := VK_Z;
    kVK_Command: Result := VK_LWIN; // Mac has one code for both Command keys. But Windows has two codes for each. Defaulting to Left.
    //kVK_Command: Result := VK_RWIN;

    kVK_ANSI_Keypad0: Result := VK_NUMPAD0;
    kVK_ANSI_Keypad1: Result := VK_NUMPAD1;
    kVK_ANSI_Keypad2: Result := VK_NUMPAD2;
    kVK_ANSI_Keypad3: Result := VK_NUMPAD3;
    kVK_ANSI_Keypad4: Result := VK_NUMPAD4;
    kVK_ANSI_Keypad5: Result := VK_NUMPAD5;
    kVK_ANSI_Keypad6: Result := VK_NUMPAD6;
    kVK_ANSI_Keypad7: Result := VK_NUMPAD7;
    kVK_ANSI_Keypad8: Result := VK_NUMPAD8;
    kVK_ANSI_Keypad9: Result := VK_NUMPAD9;
    kVK_ANSI_KeypadMultiply: Result := VK_MULTIPLY;
    kVK_ANSI_KeypadPlus: Result := VK_ADD;
    kVK_ANSI_KeypadEnter: Result := VK_SEPARATOR;
    kVK_ANSI_KeypadMinus: Result := VK_SUBTRACT;
    kVK_ANSI_KeypadDecimal: Result := VK_DECIMAL;
    kVK_ANSI_KeypadDivide: Result := VK_DIVIDE;

    kVK_F1: Result := VK_F1;
    kVK_F2: Result := VK_F2;
    kVK_F3: Result := VK_F3;
    kVK_F4: Result := VK_F4;
    kVK_F5: Result := VK_F5;
    kVK_F6: Result := VK_F6;
    kVK_F7: Result := VK_F7;
    kVK_F8: Result := VK_F8;
    kVK_F9: Result := VK_F9;
    kVK_F10: Result := VK_F10;
    kVK_F11: Result := VK_F11;
    kVK_F12: Result := VK_F12;
    // F13, F14 and F15 are generally PrSc, Scroll Lock and Pause/Break on Mac.
    // kVK_F13: Result := VK_F13;
    // kVK_F14: Result := VK_F14;
    // kVK_F15: Result := VK_F15;
    kVK_F16: Result := VK_F16;
    kVK_F17: Result := VK_F17;
    kVK_F18: Result := VK_F18;
    kVK_F19: Result := VK_F19;
    kVK_F20: Result := VK_F20;

    kVK_ANSI_KeypadClear: Result := VK_NUMLOCK;
    kVK_F14: Result := VK_SCROLL;

    kVK_Shift: Result := VK_LSHIFT;
    kVK_RightShift: Result := VK_RSHIFT;
    kVK_Control: Result := VK_LCONTROL;
    kVK_RightControl: Result := VK_RCONTROL;
    kVK_Option: Result := VK_LMENU;
    kVK_RightOption: Result := VK_RMENU;
    kVK_Mute: Result := VK_VOLUME_MUTE;
    kVK_VolumeDown: Result := VK_VOLUME_DOWN;
    kVK_VolumeUp: Result := VK_VOLUME_UP;

    kVK_ANSI_Semicolon: Result := VK_OEM_1;
    kVK_ANSI_Equal: Result := VK_OEM_PLUS;
    kVK_ANSI_Comma: Result := VK_OEM_COMMA;
    kVK_ANSI_Minus: Result := VK_OEM_MINUS;
    kVK_ANSI_Period: Result := VK_OEM_PERIOD;
    kVK_ANSI_Slash: Result := VK_OEM_2;
    kVK_ANSI_Grave: Result := VK_OEM_3;

    kVK_ANSI_LeftBracket: Result := VK_OEM_4;
    kVK_ANSI_Backslash: Result := VK_OEM_5;
    kVK_ANSI_RightBracket: Result := VK_OEM_6;
    kVK_ANSI_Quote: Result := VK_OEM_7;
    // kVK_ANSI_Backslash: Result := VK_OEM_102;

    kVK_HK_Power : Result := VK_LCL_POWER; // This will work with LCL only.

  end;

end;

function ShiftStateToMacModifiers(const AShiftState: TShiftState; const APlatformAdjusted: Boolean {= True}): UInt32; overload;
begin

  Result := 0;

  // Alt, AltGr, Meta, Ctrl, Shift, Super (Win) and Hyper
  if (ssAlt in AShiftState) or (ssMeta in AShiftState) or (ssAltGr in AShiftState) then Result := optionKey;
  if (ssShift in AShiftState) then Result := Result or shiftKey;

  if APlatformAdjusted then
  begin
    if (ssCtrl in AShiftState)  then Result := Result or cmdKey;
    if (ssSuper in AShiftState) or (ssHyper in AShiftState) then Result := Result or controlKey;
  end
  else
  begin
    if (ssCtrl in AShiftState)  then Result := Result or controlKey;
    if (ssSuper in AShiftState) or (ssHyper in AShiftState) then Result := Result or cmdKey;
  end;

end;

function ShiftStateToMacModifiers(const AShortCut: TShortCut; const APlatformAdjusted: Boolean{ = True}): UInt32; overload;
var
  LShiftState: TShiftState;
  iKeyCode: Word;
begin

  ShortCutToKey(AShortCut, iKeyCode, LShiftState);
  Result := ShiftStateToMacModifiers(LShiftState, APlatformAdjusted);

end;


function MacModifiersToShiftState(const AModifiers: UInt32; const APlatformAdjusted: Boolean{ = True}): TShiftState;
begin

  Result := [];

  // Alt, AltGr, Meta, Ctrl, Shift, Super (Win) and Hyper
  // Caps, Num and Scroll are lock/toggle key, not modifiers, so we filter out them all

  if (AModifiers and optionKey <> 0) then Result := Result + [ssAlt]; // Defaulting to Alt. Could be Meta or AltGr as well.
  if (AModifiers and shiftKey <> 0) then Result := Result + [ssShift];

  if APlatformAdjusted then
  begin
    if (AModifiers and cmdKey <> 0) then Result := Result + [ssCtrl];
    if (AModifiers and controlKey <> 0) then Result := Result + [ssSuper]; // Defaulting to Super. Could be Hyper as well.
  end
  else
  begin
    if (AModifiers and controlKey <> 0) then Result := Result + [ssCtrl];
    if (AModifiers and cmdKey <> 0) then Result := Result + [ssSuper]; // Defaulting to Super. Could be Hyper as well.
  end;

end;

function MacModifiersToWinModifiers(const AModifiers: UInt32; const APlatformAdjusted: Boolean{ = True}): DWord;
begin

  Result := 0;

  // Alt, AltGr, Meta, Ctrl, Shift, Super (Win) and Hyper
  // Caps, Num and Scroll are lock/toggle key, not modifiers, so we filter out them all

  if (AModifiers and optionKey <> 0) then Result := Result or MOD_WIN_ALT; // Defaulting to Alt. Could be Meta or AltGr as well.
  if (AModifiers and shiftKey <> 0) then Result := Result or MOD_WIN_SHIFT;

  if APlatformAdjusted then
  begin
    if (AModifiers and cmdKey <> 0) then Result := Result or MOD_WIN_CONTROL;
    if (AModifiers and controlKey <> 0) then Result := Result or MOD_WIN_SUPER; // Defaulting to Super. Could be Hyper as well.
  end
  else
  begin
    if (AModifiers and controlKey <> 0) then Result := Result or MOD_WIN_CONTROL;
    if (AModifiers and cmdKey <> 0) then Result := Result or MOD_WIN_SUPER; // Defaulting to Super. Could be Hyper as well.
  end;

end;




end.


