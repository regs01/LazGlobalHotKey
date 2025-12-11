unit LazHotKeyPlatformWin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLType, LCLIntf,
  Windows, CommCtrl, ctypes, LMessages,
  LazHotKeyType, LazHotKeyFunctionsWin, LazHotKeyPlatform;

type

  THotKeyPlatformWindows = class (THotKeyPlatform)
  protected var
    FError: DWORD;
    FHandle: HWND;
    FWineNotice: Boolean;
  protected
    function GetProcessId: Integer; override;
    procedure TriggerEvent(AHotKeyId: Integer);
  public
    constructor Create(const AFormHandle: HWND; AHotKeyList: THotKeyList);
    destructor Destroy; override;
    function GetError: Longint; override;
    function GetNativeShortCut(const AShortCut: TShortCut; const {%H-}APlatformAdjusted: Boolean = True): TNativeShortCut; override;
    function GetNativeShortCut(const AKeyCode, AModifiers: UINT): TNativeShortCut;
    function GetShortCut(const ANativeShortCut: TNativeShortCut; const {%H-}APlatformAdjusted: Boolean): TShortCut; override;
    procedure StartHandler; override;
    procedure StopHandler; override;
    function DoRegisterGlobalHotKey(const AHotKeyInfo: PTHotKeyInfo): Boolean; override;
    function DoUnregisterGlobalHotkey(const AHotKeyInfo: PTHotKeyInfo): Boolean; override;
  public
    property Error: DWORD read FError;
    property WineNotice: Boolean read FWineNotice write FWineNotice;
  end;

implementation

function SubclassProc(hwnd: HWND; uMsg: cUINT; AwParam: WPARAM; AlParam: LPARAM; uISubClass: UINT_PTR; dwRefData: DWORD_PTR): LRESULT; stdcall;
var
  LMsg: PMSG;
begin

  // https://learn.microsoft.com/windows/win32/inputdev/wm-hotkey

  Result := 0;

  if uMsg = WM_HOTKEY then
  begin

    try

      if dwRefData = 0 then
        raise EArgumentNilException.Create('dwRefData is nil.') ;

      if THotKeyPlatformWindows(dwRefData).EventOutput then
        THotKeyPlatformWindows(dwRefData).TriggerEvent(Integer(AwParam))
      else
      begin

        New(LMsg);
        LMsg^.hwnd := hwnd;
        LMsg^.message := uMsg;
        LMsg^.wParam := AwParam;
        LMsg^.lParam := AlParam;
        LMsg^.time := Windows.GetTickCount;

        try
          PostMessage(THotKeyPlatformWindows(dwRefData).FormHandle, SC_HOTKEY, WPARAM(MESSAGE_HOTKEY_PLATFORMDATA), LPARAM(LMsg));
        except
          Dispose(LMsg);
          raise;
        end;

      end;

      Exit(1);

    except
      Exit;
    end;

  end
  else
  begin
    Result := DefSubclassProc(hwnd, uMsg, AwParam, AlParam);
  end;

end;

procedure THotKeyPlatformWindows.TriggerEvent(AHotKeyId: Integer);
begin

  DoTriggerEvent(AHotKeyId);

end;

constructor THotKeyPlatformWindows.Create(const AFormHandle: HWND; AHotKeyList: THotKeyList);
begin

  inherited;

  FError := 0;

end;

destructor THotKeyPlatformWindows.Destroy;
begin

  inherited;

end;

function THotKeyPlatformWindows.GetProcessId: Integer;
begin

  Result := Windows.GetCurrentProcessId;

end;

function THotKeyPlatformWindows.GetError: Longint;
begin

  Result := FError;

end;

function THotKeyPlatformWindows.GetNativeShortCut(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): TNativeShortCut;
begin

  Result := ShortCutToNativeShortCut(AShortCut);

end;

function THotKeyPlatformWindows.GetNativeShortCut(const AKeyCode, AModifiers: UINT): TNativeShortCut;
begin

  Result := KeyCodeToNativeShortCut(AKeyCode, AModifiers);

end;

function THotKeyPlatformWindows.GetShortCut(const ANativeShortCut: TNativeShortCut; const {%H-}APlatformAdjusted: Boolean): TShortCut;
begin

  Result := NativeShortCutToShortCut(ANativeShortCut);

end;

procedure THotKeyPlatformWindows.StartHandler;
begin

  SetWindowSubclass(FFormHandle, @SubclassProc, 0, DWORD_PTR(Self));

end;

procedure THotKeyPlatformWindows.StopHandler;
begin

  RemoveWindowSubclass(FFormHandle, @SubclassProc, 0);

end;

function THotKeyPlatformWindows.DoRegisterGlobalHotKey(const AHotKeyInfo: PTHotKeyInfo): Boolean;
var
  iKeyCode: UINT;
  iModifiers: UINT;
begin

  if FWineNotice then
  if SysUtils.GetEnvironmentVariable('WINELOADER') <> '' then
    MessageBox(0, 'RegisterHotKey is not yet fully implemented in WINE. Might not work globally and work only if window is focused.',
      'Note', MB_OK+MB_ICONEXCLAMATION);

  Result := False;

  if AHotKeyInfo = Nil then
    Exit;

  NativeShortCutToKeyCode(AHotKeyInfo^.NativeShortCut, iKeyCode, iModifiers);

  Result := RegisterHotKey(FFormHandle, AHotKeyInfo^.Id, iModifiers, iKeyCode);

  if not Result then
    FError := Windows.GetLastError;

end;

function THotKeyPlatformWindows.DoUnregisterGlobalHotkey(const AHotKeyInfo: PTHotKeyInfo): Boolean;
begin

  Result := False;

  if AHotKeyInfo = Nil then
    Exit;

  Result := UnregisterHotKey(FFormHandle, AHotKeyInfo^.Id);

  if not Result then
    FError := Windows.GetLastError;

end;

end.