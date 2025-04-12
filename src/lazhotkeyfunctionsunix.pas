unit LazHotKeyFunctionsUnix;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf, Menus,
  Unix, x, keysym, xf86keysym, xlib, xkblib, // What are hpkeysym, sunkeysym?
  LazHotKeyType;

  function KeyCodeToNativeShortCut(const AKeycode: TKeyCode; const AModifiers: cuint): TNativeShortCut;
  procedure NativeShortCutToKeyCode(const AShortCut: TNativeShortCut; out AKeycode: TKeyCode; out AModifiers: cuint); overload;
  procedure NativeShortCutToKeyCode(const AShortCut: TNativeShortCut; out AKeycode: LongWord; out AModifiers: cuint); overload;
  function ShortCutToNativeShortCut(const AShortCut: TShortCut; const AXDisplay: PDisplay): TNativeShortCut;
  function NativeShortCutToShortCut(const ANativeShortCut: TNativeShortCut; const AXDisplay: PDisplay): TShortCut;
  function XKeysymToNativeShortCut(const AKeysym: TKeySym; AModifiers: cuint; const AXDisplay: PDisplay): TNativeShortCut;
  function VKKeycodeToXKeysym(const AVKKeycode: Word): TKeySym; overload;
  function VKKeycodeToXKeysym(const AShortCut: TShortCut): TKeySym; overload;
  function XKeysymToVKKeycode(const AXKeysym: TKeySym): Word; overload;
  function XKeycodeToXKeysym(const AXKeycode: TKeyCode; const AXDisplay: PDisplay): TKeySym;
  function ShiftStateToXKeysymMask(const AShiftState: TShiftState; const {%H-}APlatformAdjusted: Boolean = True): cuint; overload;
  function ShiftStateToXKeysymMask(const AShortCut: TShortCut; const {%H-}APlatformAdjusted: Boolean = True): cuint; overload;
  function XKeysymMaskToShiftState(AXKeysymMask: cuint; const {%H-}APlatformAdjusted: Boolean = True): TShiftState;
  function XKeysymMaskToWinModifiers(AXKeysymMask: cuint; const {%H-}APlatformAdjusted: Boolean = True): DWord;

implementation

// xmodmap -pke or xmodmap -pk // X keycodes
// https://learn.microsoft.com/en-us/windows/win32/inputdev/virtual-key-codes // Virtual Keycodes

// https://unix.stackexchange.com/questions/21089/how-to-use-command-line-to-change-volume
// https://developer.mozilla.org/en-US/docs/Web/API/UI_Events/Keyboard_event_key_values

function KeyCodeToNativeShortCut(const AKeycode: TKeyCode; const AModifiers: cuint): TNativeShortCut;
begin

  if (AKeycode and $FF00) <> 0 then
    Exit(0);
  Result := AModifiers shl 8;
  Inc(Result, AKeycode);

end;

procedure NativeShortCutToKeyCode(const AShortCut: TNativeShortCut; out AKeycode: TKeyCode; out AModifiers: LongWord);
begin

  AKeycode := AShortCut and $00FF;
  AModifiers := AShortCut shr 8;

end;

procedure NativeShortCutToKeyCode(const AShortCut: TNativeShortCut; out AKeycode: LongWord; out AModifiers: cuint);
var
  LKeyCode: TKeyCode;
begin

  NativeShortCutToKeyCode(AShortCut, LKeyCode, AModifiers);
  AKeycode := LongWord(LKeyCode);

end;

function ShortCutToNativeShortCut(const AShortCut: TShortCut; const AXDisplay: PDisplay): TNativeShortCut;
var
  iKeyCode: Word;
  LShiftState: TShiftState;
  iXKeySym: TKeySym;
  iXKeyCode: TKeyCode;
  iXModifiers: LongWord;
begin

  ShortCutToKey(AShortCut, iKeyCode, LShiftState);
  iXKeySym := VKKeycodeToXKeysym(iKeyCode);
  iXKeyCode := XKeysymToKeycode(AXDisplay, iXKeySym);
  iXModifiers := ShiftStateToXKeysymMask(LShiftState);

  Result := KeyCodeToNativeShortCut(iXKeyCode, iXModifiers);

end;

function NativeShortCutToShortCut(const ANativeShortCut: TNativeShortCut; const AXDisplay: PDisplay): TShortCut;
var
  iKeyCode: Word;
  LShiftState: TShiftState;
  iXKeySym: TKeySym;
  iXKeyCode: TKeyCode;
  iXModifiers: LongWord;
begin

  NativeShortCutToKeyCode(ANativeShortCut, iXKeyCode, iXModifiers);
  iXKeySym := XKeycodeToXKeysym(iXKeyCode, AXDisplay);
  iKeyCode := XKeysymToVKKeycode(iXKeySym);
  LShiftState := XKeysymMaskToShiftState(iXModifiers);

  Result := KeyToShortCut(iKeyCode, LShiftState);

end;

function XKeysymToNativeShortCut(const AKeysym: TKeySym; AModifiers: cuint; const AXDisplay: PDisplay): TNativeShortCut;
var
  iXKeyCode: TKeyCode;
begin

  // Making sure no Caps Lock and Num Lock in Mask
  AModifiers := AModifiers and not (LockMask or Mod2Mask);

  iXKeyCode := XKeysymToKeycode(AXDisplay, AKeysym);
  Result := KeyCodeToNativeShortCut(iXKeyCode, AModifiers);

end;

function VKKeycodeToXKeysym(const AVKKeycode: Word): TKeySym; overload;
begin

  case AVKKeycode of

    // Mouse buttons
    //VK_LBUTTON
    //VK_RBUTTON
    //VK_MBUTTON
    //VK_XBUTTON1
    //VK_XBUTTON2

    VK_BACK: Result := XK_BackSpace;
    VK_TAB: Result := XK_Tab;
    VK_CLEAR: Result := XK_Clear;
    VK_RETURN: Result := XK_Return;

    // Modifiers. Use Masks. Separate buttons are down in the list.
    //VK_SHIFT := ShiftMask
    //VK_CONTROL := ControlMask
    //VK_MENU := Mod1Mask

    VK_PAUSE: Result := XK_Pause;
    VK_ESCAPE: Result := XK_Escape;

    // Lock and toggle keys
    VK_CAPITAL: Result := XK_Caps_Lock; // LockMask
    VK_KANA: Result := XK_Kana_Lock;
    //VK_HANGUL: Result := XK_Hangul; // We can't have duplicate. // So probably some optional Japanese and Korean mode in future.
    VK_JUNJA: Result := XK_Hangul_Jeonja;
    VK_KANJI: Result := XK_Kanji;
    //VK_HANJA: Result := XK_Hangul_Hanja;
    VK_CONVERT: Result := XK_Henkan;
    VK_NONCONVERT: Result := XK_Muhenkan;
    VK_MODECHANGE: Result := XK_Mode_switch;

    VK_SPACE: Result := XK_space;
    VK_PRIOR: Result := XK_Page_Up;
    VK_NEXT: Result := XK_Page_Down;
    VK_END: Result := XK_End;
    VK_HOME: Result := XK_Home;
    VK_LEFT: Result := XK_Left;
    VK_UP: Result := XK_Up;
    VK_RIGHT: Result := XK_Right;
    VK_DOWN: Result := XK_Down;
    VK_SELECT: Result := XK_Select;
    VK_PRINT: Result := XK_Print;
    VK_EXECUTE: Result := XK_Execute;
    VK_SNAPSHOT: Result := XK_Print;
    VK_INSERT: Result := XK_Insert;
    VK_DELETE: Result := XK_Delete;
    VK_HELP: Result := XK_Help;

    // Numbers and Letters
    VK_0: Result := XK_0;
    VK_1: Result := XK_1;
    VK_2: Result := XK_2;
    VK_3: Result := XK_3;
    VK_4: Result := XK_4;
    VK_5: Result := XK_5;
    VK_6: Result := XK_6;
    VK_7: Result := XK_7;
    VK_8: Result := XK_8;
    VK_9: Result := XK_9;
    VK_A: Result := XK_A;
    VK_B: Result := XK_B;
    VK_C: Result := XK_C;
    VK_D: Result := XK_D;
    VK_E: Result := XK_E;
    VK_F: Result := XK_F;
    VK_G: Result := XK_G;
    VK_H: Result := XK_H;
    VK_I: Result := XK_I;
    VK_J: Result := XK_J;
    VK_K: Result := XK_K;
    VK_L: Result := XK_L;
    VK_M: Result := XK_M;
    VK_N: Result := XK_N;
    VK_O: Result := XK_O;
    VK_P: Result := XK_P;
    VK_Q: Result := XK_Q;
    VK_R: Result := XK_R;
    VK_S: Result := XK_S;
    VK_T: Result := XK_T;
    VK_U: Result := XK_U;
    VK_V: Result := XK_V;
    VK_W: Result := XK_W;
    VK_X: Result := XK_X;
    VK_Y: Result := XK_Y;
    VK_Z: Result := XK_Z;

    // Modifiers
    VK_LWIN: Result := XK_Super_L; // Mod4Mask
    VK_RWIN: Result := XK_Super_R; // Mod4Mask
    VK_APPS: Result := XK_Menu;

    VK_NUMPAD0: Result := XK_KP_0;
    VK_NUMPAD1: Result := XK_KP_1;
    VK_NUMPAD2: Result := XK_KP_2;
    VK_NUMPAD3: Result := XK_KP_3;
    VK_NUMPAD4: Result := XK_KP_4;
    VK_NUMPAD5: Result := XK_KP_5;
    VK_NUMPAD6: Result := XK_KP_6;
    VK_NUMPAD7: Result := XK_KP_7;
    VK_NUMPAD8: Result := XK_KP_8;
    VK_NUMPAD9: Result := XK_KP_9;
    VK_MULTIPLY: Result := XK_KP_Multiply;
    VK_ADD: Result := XK_KP_Add;
    VK_SEPARATOR: Result := XK_KP_Separator;
    VK_SUBTRACT: Result := XK_KP_Subtract;
    VK_DECIMAL: Result := XK_KP_Decimal;
    VK_DIVIDE: Result := XK_KP_Divide;
    VK_F1: Result := XK_F1;
    VK_F2: Result := XK_F2;
    VK_F3: Result := XK_F3;
    VK_F4: Result := XK_F4;
    VK_F5: Result := XK_F5;
    VK_F6: Result := XK_F6;
    VK_F7: Result := XK_F7;
    VK_F8: Result := XK_F8;
    VK_F9: Result := XK_F9;
    VK_F10: Result := XK_F10;
    VK_F11: Result := XK_F11;
    VK_F12: Result := XK_F12;
    VK_F13: Result := XK_F13;
    VK_F14: Result := XK_F14;
    VK_F15: Result := XK_F15;
    VK_F16: Result := XK_F16;
    VK_F17: Result := XK_F17; // Do not use VK_LWIN + VK_F17
    VK_F18: Result := XK_F18;
    VK_F19: Result := XK_F19;
    VK_F20: Result := XK_F20;
    VK_F21: Result := XK_F21;
    VK_F22: Result := XK_F22;
    VK_F23: Result := XK_F23;
    VK_F24: Result := XK_F24;

    // Lock keys
    VK_NUMLOCK: Result := XK_Num_Lock; // Mod2Mask
    VK_SCROLL: Result := XK_Scroll_Lock; // Mod3Mask

    // Modifiers
    VK_LSHIFT: Result := XK_Shift_L; // ShiftMask
    VK_RSHIFT: Result := XK_Shift_R; // ShiftMask
    VK_LCONTROL: Result := XK_Control_L; // ControlMask
    VK_RCONTROL: Result := XK_Control_R; // ControlMask
    VK_LMENU: Result := XK_Alt_L; // Mod1Mask
    VK_RMENU: Result := XK_Alt_R; // Mod1Mask for Alt or Mod5Mask for AltGr

    // Media
    VK_BROWSER_BACK: Result := XF86XK_Back;
    VK_BROWSER_FORWARD: Result := XF86XK_Forward;
    VK_BROWSER_REFRESH: Result := XF86XK_Refresh;
    VK_BROWSER_STOP: Result := XF86XK_Stop;
    VK_BROWSER_SEARCH: Result := XF86XK_Search;
    VK_BROWSER_FAVORITES: Result := XF86XK_Favorites;
    VK_BROWSER_HOME: Result := XF86XK_HomePage;
    VK_VOLUME_MUTE: Result := XF86XK_AudioMute;
    VK_VOLUME_DOWN: Result := XF86XK_AudioLowerVolume;
    VK_VOLUME_UP: Result := XF86XK_AudioRaiseVolume;
    VK_MEDIA_NEXT_TRACK: Result := XF86XK_AudioNext;
    VK_MEDIA_PREV_TRACK: Result := XF86XK_AudioPrev;
    VK_MEDIA_STOP: Result := XF86XK_AudioStop;
    VK_MEDIA_PLAY_PAUSE: Result := XF86XK_AudioPlay;
    VK_LAUNCH_MAIL: Result := XF86XK_Mail;
    VK_LAUNCH_MEDIA_SELECT: Result := XF86XK_Eject;
    VK_LAUNCH_APP1: Result := XF86XK_Launch1;
    VK_LAUNCH_APP2: Result := XF86XK_Launch2;

    // Grammatics
    VK_OEM_1: Result := XK_semicolon;
    VK_OEM_PLUS: Result := XK_plus;
    VK_OEM_COMMA: Result := XK_comma;
    VK_OEM_MINUS: Result := XK_minus;
    VK_OEM_PERIOD: Result := XK_period;
    VK_OEM_2: Result := XK_slash;
    VK_OEM_3: Result := XK_asciitilde;
    VK_OEM_4: Result := XK_bracketleft;
    VK_OEM_5: Result := XK_backslash;
    VK_OEM_6: Result := XK_bracketright;
    VK_OEM_7: Result := XK_quoteleft;
    VK_OEM_8: Result := XK_quoteright;
    VK_OEM_102: Result := XK_backslash;

    // Console
    VK_ATTN: Result := XK_3270_Attn;
    VK_CRSEL: Result := XK_3270_CursorSelect;
    VK_EXSEL: Result := XK_3270_ExSelect;
    VK_EREOF: Result := XK_3270_EraseEOF;
    VK_PLAY: Result := XK_3270_Play;
    VK_ZOOM: Result := XF86XK_ZoomIn; // XF86XK_ZoomOut;
    VK_PA1: Result := XK_3270_PA1;
    VK_OEM_CLEAR: Result := XK_Clear;

    // Power
    VK_SLEEP: Result := XF86XK_Sleep;
    VK_LCL_POWER: Result := XF86XK_PowerOff;

    else Result := 0;

  end;

end;

function XKeysymToVKKeycode(const AXKeysym: TKeySym): Word; overload;
begin

  case AXKeysym of

    XK_BackSpace: Result := VK_BACK;
    XK_Tab: Result := VK_TAB;

    XK_Clear: Result := VK_CLEAR;
    XK_Return: Result := VK_RETURN;

    XK_Pause: Result := VK_PAUSE;
    XK_Caps_Lock: Result := VK_CAPITAL;
    XK_Kana_Lock: Result := VK_KANA;
    XK_Hangul: Result := VK_HANGUL;
    XK_Hangul_Jeonja: Result := VK_JUNJA;
    XK_Hangul_Hanja: Result := VK_HANJA;
    XK_Kanji: Result := VK_KANJI;
    XK_Escape: Result := VK_ESCAPE;
    XK_Henkan: Result := VK_CONVERT;
    XK_Muhenkan: Result := VK_NONCONVERT;
    XK_Mode_switch: Result := VK_MODECHANGE;

    XK_space: Result := VK_SPACE;
    XK_Page_Up: Result := VK_PRIOR;
    XK_Page_Down: Result := VK_NEXT;
    XK_End: Result := VK_END;
    XK_Home: Result := VK_HOME;
    XK_Left: Result := VK_LEFT;
    XK_Up: Result := VK_UP;
    XK_Right: Result := VK_RIGHT;
    XK_Down: Result := VK_DOWN;
    XK_Select: Result := VK_SELECT;
    XK_Print: Result := VK_SNAPSHOT;
    XK_Execute: Result := VK_EXECUTE;
    XK_Insert: Result := VK_INSERT;
    XK_Delete: Result := VK_DELETE;
    XK_Help: Result := VK_HELP;

    XK_0: Result := VK_0;
    XK_1: Result := VK_1;
    XK_2: Result := VK_2;
    XK_3: Result := VK_3;
    XK_4: Result := VK_4;
    XK_5: Result := VK_5;
    XK_6: Result := VK_6;
    XK_7: Result := VK_7;
    XK_8: Result := VK_8;
    XK_9: Result := VK_9;

    XK_A: Result := VK_A;
    XK_B: Result := VK_B;
    XK_C: Result := VK_C;
    XK_D: Result := VK_D;
    XK_E: Result := VK_E;
    XK_F: Result := VK_F;
    XK_G: Result := VK_G;
    XK_H: Result := VK_H;
    XK_I: Result := VK_I;
    XK_J: Result := VK_J;
    XK_K: Result := VK_K;
    XK_L: Result := VK_L;
    XK_M: Result := VK_M;
    XK_N: Result := VK_N;
    XK_O: Result := VK_O;
    XK_P: Result := VK_P;
    XK_Q: Result := VK_Q;
    XK_R: Result := VK_R;
    XK_S: Result := VK_S;
    XK_T: Result := VK_T;
    XK_U: Result := VK_U;
    XK_V: Result := VK_V;
    XK_W: Result := VK_W;
    XK_X: Result := VK_X;
    XK_Y: Result := VK_Y;
    XK_Z: Result := VK_Z;

    XK_Super_L: Result := VK_LWIN;
    XK_Super_R: Result := VK_RWIN;
    XK_Menu: Result := VK_APPS;

    XK_KP_0: Result := VK_NUMPAD0;
    XK_KP_1: Result := VK_NUMPAD1;
    XK_KP_2: Result := VK_NUMPAD2;
    XK_KP_3: Result := VK_NUMPAD3;
    XK_KP_4: Result := VK_NUMPAD4;
    XK_KP_5: Result := VK_NUMPAD5;
    XK_KP_6: Result := VK_NUMPAD6;
    XK_KP_7: Result := VK_NUMPAD7;
    XK_KP_8: Result := VK_NUMPAD8;
    XK_KP_9: Result := VK_NUMPAD9;

    XK_KP_Multiply: Result := VK_MULTIPLY;
    XK_KP_Add: Result := VK_ADD;
    XK_KP_Separator: Result := VK_SEPARATOR;
    XK_KP_Subtract: Result := VK_SUBTRACT;
    XK_KP_Decimal: Result := VK_DECIMAL;
    XK_KP_Divide: Result := VK_DIVIDE;

    XK_F1: Result := VK_F1;
    XK_F2: Result := VK_F2;
    XK_F3: Result := VK_F3;
    XK_F4: Result := VK_F4;
    XK_F5: Result := VK_F5;
    XK_F6: Result := VK_F6;
    XK_F7: Result := VK_F7;
    XK_F8: Result := VK_F8;
    XK_F9: Result := VK_F9;
    XK_F10: Result := VK_F10;
    XK_F11: Result := VK_F11;
    XK_F12: Result := VK_F12;
    XK_F13: Result := VK_F13;
    XK_F14: Result := VK_F14;
    XK_F15: Result := VK_F15;
    XK_F16: Result := VK_F16;
    XK_F17: Result := VK_F17;
    XK_F18: Result := VK_F18;
    XK_F19: Result := VK_F19;
    XK_F20: Result := VK_F20;
    XK_F21: Result := VK_F21;
    XK_F22: Result := VK_F22;
    XK_F23: Result := VK_F23;
    XK_F24: Result := VK_F24;

    XK_Num_Lock: Result := VK_NUMLOCK;
    XK_Scroll_Lock: Result := VK_SCROLL;

    XK_Shift_L: Result := VK_LSHIFT;
    XK_Shift_R: Result := VK_RSHIFT;
    XK_Control_L: Result := VK_LCONTROL;
    XK_Control_R: Result := VK_RCONTROL;
    XK_Alt_L: Result := VK_LMENU;
    XK_Alt_R: Result := VK_RMENU;
    XF86XK_Back: Result := VK_BROWSER_BACK;
    XF86XK_Forward: Result := VK_BROWSER_FORWARD;
    XF86XK_Refresh: Result := VK_BROWSER_REFRESH;
    XF86XK_Stop: Result := VK_BROWSER_STOP;
    XF86XK_Search: Result := VK_BROWSER_SEARCH;
    XF86XK_Favorites: Result := VK_BROWSER_FAVORITES;
    XF86XK_HomePage: Result := VK_BROWSER_HOME;
    XF86XK_AudioMute: Result := VK_VOLUME_MUTE;
    XF86XK_AudioLowerVolume: Result := VK_VOLUME_DOWN;
    XF86XK_AudioRaiseVolume: Result := VK_VOLUME_UP;
    XF86XK_AudioNext: Result := VK_MEDIA_NEXT_TRACK;
    XF86XK_AudioPrev: Result := VK_MEDIA_PREV_TRACK;
    XF86XK_AudioStop: Result := VK_MEDIA_STOP;
    XF86XK_AudioPlay: Result := VK_MEDIA_PLAY_PAUSE;
    XF86XK_Mail: Result := VK_LAUNCH_MAIL;
    XF86XK_Eject: Result := VK_LAUNCH_MEDIA_SELECT;
    XF86XK_Launch1: Result := VK_LAUNCH_APP1;
    XF86XK_Launch2: Result := VK_LAUNCH_APP2;


    XK_semicolon: Result := VK_OEM_1;
    XK_plus: Result := VK_OEM_PLUS;
    XK_comma: Result := VK_OEM_COMMA;
    XK_minus: Result := VK_OEM_MINUS;
    XK_period: Result := VK_OEM_PERIOD;
    XK_slash: Result := VK_OEM_2;
    XK_asciitilde: Result := VK_OEM_3;
    XK_bracketleft: Result := VK_OEM_4;
    XK_backslash: Result := VK_OEM_5;
    XK_bracketright: Result := VK_OEM_6;
    XK_quoteleft: Result := VK_OEM_7;
    XK_quoteright: Result := VK_OEM_8;

    // Better not to use this section. For IME process event data directly.
    XK_Eisu_toggle: Result := 240; // VK_OEM_ATTN;
    XK_Katakana: Result := 241; // VK_OEM_FINNISH;
    XK_Hiragana: Result := 242; // VK_OEM_COPY;
    XK_Hankaku: Result := 243; // VK_OEM_AUTO;
    XK_Zenkaku: Result := 244; // VK_OEM_ENLW;
    XK_Romaji: Result := 245; // VK_OEM_BACKTAB ;
    //XK_Kana_Lock: Result := VK_ATTN;

    XK_3270_Attn: Result := VK_ATTN;
    XK_3270_CursorSelect: Result := VK_CRSEL;
    XK_3270_ExSelect: Result := VK_EXSEL;
    XK_3270_EraseEOF: Result := VK_EREOF;
    XK_3270_Play: Result := VK_PLAY;
    XF86XK_ZoomIn: Result := VK_ZOOM;

    XK_3270_PA1: Result := VK_PA1;
    //XK_Clear: Result := VK_OEM_CLEAR;

    XF86XK_Sleep: Result := VK_SLEEP;
    XF86XK_PowerOff: Result := VK_LCL_POWER;

    else Result := VK_UNKNOWN;

  end;

end;

function VKKeycodeToXKeysym(const AShortCut: TShortCut): TKeySym; overload;
var
  LShiftState: TShiftState;
  iKeyCode: Word;
begin

  ShortCutToKey(AShortCut, iKeyCode, LShiftState);
  Result := VKKeycodeToXKeysym(iKeyCode);

end;

function XKeycodeToXKeysym(const AXKeycode: TKeyCode; const AXDisplay: PDisplay): TKeySym;
begin

  Result := XkbKeycodeToKeysym(AXDisplay, AXKeycode, 0, 0);

end;

function ShiftStateToXKeysymMask(const AShiftState: TShiftState; const APlatformAdjusted: Boolean = True): cuint; overload;
begin

  Result := 0;

  // Alt, AltGr, Meta, Ctrl, Shift, Super (Win) and Hyper. LCL ssMeta is Super/Win.
  // Caps, Num and Scroll are lock/toggle key, not modifiers, so we filter out them all
  if (ssAlt in AShiftState) then Result := Result or Mod1Mask;
  if (ssAltGr in AShiftState) then Result := Result or Mod5Mask;
  if (ssCtrl in AShiftState)  then Result := Result or ControlMask;
  if (ssShift in AShiftState) then Result := Result or ShiftMask;
  if (ssMeta in AShiftState) or (ssSuper in AShiftState) or (ssHyper in AShiftState) then Result := Result or Mod4Mask;

end;

function ShiftStateToXKeysymMask(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): cuint; overload;
var
  LShiftState: TShiftState;
  iKeyCode: Word;
begin

  ShortCutToKey(AShortCut, iKeyCode, LShiftState);
  Result := ShiftStateToXKeysymMask(LShiftState);

end;

function XKeysymMaskToShiftState(AXKeysymMask: cuint; const APlatformAdjusted: Boolean = True): TShiftState;
begin

  Result := [];

  // Alt, AltGr, Meta, Ctrl, Shift, Super (Win) and Hyper. LCL ssMeta is Super/Win.
  // Caps, Num and Scroll are lock/toggle key, not modifiers, so we filter out them all

  // Filtering out Caps Lock and Num Lock
  AXKeysymMask := AXKeysymMask and not (LockMask or Mod2Mask);

  if AXKeysymMask and Mod1Mask <> 0 then Result := Result + [ssAlt]; // Defaulting to Alt. Could be Meta as well.
  if AXKeysymMask and Mod5Mask <> 0 then Result := Result + [ssAltGr];
  if AXKeysymMask and ControlMask <> 0 then Result := Result + [ssCtrl];
  if AXKeysymMask and ShiftMask <> 0 then Result := Result + [ssShift];
  if AXKeysymMask and Mod4Mask <> 0 then Result := Result + [ssMeta];  // Defaulting to ssMeta,  Could be Hyper or Super as well.

end;

function XKeysymMaskToWinModifiers(AXKeysymMask: cuint; const APlatformAdjusted: Boolean = True): DWord;
begin

  Result := 0;

  // Alt, AltGr, Meta, Ctrl, Shift, Super (Win) and Hyper
  // Caps, Num and Scroll are lock/toggle key, not modifiers, so we filter out them all

  // Filtering out Caps Lock and Num Lock
  AXKeysymMask := AXKeysymMask and not (LockMask or Mod2Mask);

  if (AXKeysymMask and Mod1Mask <> 0) or (AXKeysymMask and Mod5Mask <> 0) then Result := Result or MOD_WIN_ALT; // There are no AltGr and Meta in VK Modifiers.
  if (AXKeysymMask and ControlMask <> 0) then Result := Result or MOD_WIN_CONTROL;
  if (AXKeysymMask and ShiftMask <> 0) then Result := Result or MOD_WIN_SHIFT;
  if (AXKeysymMask and Mod4Mask <> 0) then Result := Result or MOD_WIN_SUPER;  // There is no Hyper in VK Modifiers.

end;

end.



