unit LazHotKey;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, LCLType, LCLClasses, process, Controls,
  LazHotKeyType, LazHotKeyPlatform,
  // https://wiki.freepascal.org/Platform_defines
  {$IfDef Unix}
  {$IfDef Darwin}
  MacOSAll, LazHotKeyPlatformDarwin,
  {$Else}
  Unix, X, LazHotKeyPlatformUnix,
  {$EndIf}
  {$EndIf}
  {$IfDef MSWindows}
  Windows, LazHotKeyPlatformWin,
  {$EndIf}
  {$IfDef Debug}
  {$EndIf}
  Forms, Dialogs, fgl, Menus,
  ComponentEditors, PropEdits;

  type
    TCustomGlobalHotKey = class(TLCLComponent)
    protected var
      FActive: Boolean;
      FEventOutput: Boolean;
      FFormHandle: HWND;
      FHotKeyList: THotKeyList;
      FIncrementalCounter: Word;
      FUnixInterval: Word;
      FWineNotice: Boolean;
      FPlatform: THotKeyPlatform;
      FOnHotKeyEvent: THotKeyEvent;
      FOnRegisterEvent: THotKeyRegisterEvent;
      FOnUnregisterEvent: THotKeyRegisterEvent;
    protected
      procedure SetActive(const AValue: Boolean);
      procedure SetEventOutput(const AValue: Boolean);
      procedure SetOnHotKeyEvent(Value: THotKeyEvent);
      procedure SetUnixInterval(const AValue: Word);
      procedure SetWineNotice(const AValue: Boolean);
      function IsActive: Boolean;
      procedure StartHandler;
      procedure StopHandler;
      function RegisterGlobalHotkey(AHotKeyInfo: PTHotKeyInfo): Boolean;
      function UnregisterGlobalHotkey(AHotKeyInfo: PTHotKeyInfo): Boolean;
      function AddGlobalHotkey(var AHotKeyInfo: THotKeyInfo): Boolean;
      function AddGlobalHotkey(const ANativeShortCut: TNativeShortCut; const APlatformAdjusted: Boolean = False): Boolean;
      function RemoveGlobalHotkey(const ANativeShortCut: TNativeShortCut): Boolean;
      function ActivateAllGloablHotkeys: Boolean;
      function UnactivateAllGloablHotkeys: Boolean;
    protected
      procedure DefineProperties(Filer: TFiler); override;
      procedure ReadHotKeyList(Reader: TReader);
      procedure WriteHotKeyList(Writer: TWriter);
    public
      constructor Create(AOwner: TComponent); override;
      constructor Create(const AFormHandle: HWND);
      destructor Destroy; override;
      procedure Start;
      procedure Stop;
      function AddGlobalHotkey(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): Boolean;
      function RemoveGlobalHotkey(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): Boolean;
      function RemoveAllGloablHotkeys: Boolean;
      {$IfDef Unix}
      {$IfDef Darwin}
      function AddGlobalHotkeyDarwin(const AKeyCode: UInt32; const AModifiers: UInt32): Boolean;
      function RemoveGlobalHotkeyDarwin(const AKeyCode: UInt32; const AModifiers: UInt32): Boolean;
      {$Else}
      function AddGlobalHotkeyUnix(const AKeysym: TKeySym; const AMask: cint): Boolean;
      function RemoveGlobalHotkeyUnix(const AKeysym: TKeySym; const AMask: cint): Boolean;
      {$EndIf}
      {$EndIf}
      {$IfDef MSWindows}
      function AddGlobalHotkeyWindows(const AKeyCode: UINT; const AModifiers: UINT): Boolean;
      function RemoveGlobalHotkeyWindows(const AKeyCode: UINT; const AModifiers: UINT): Boolean;
      {$EndIf}
    public
      property OnHotKey: THotKeyEvent read FOnHotKeyEvent write SetOnHotKeyEvent;
      property OnRegister: THotKeyRegisterEvent read FOnRegisterEvent write FOnRegisterEvent;
      property OnUnRegister: THotKeyRegisterEvent read FOnUnregisterEvent write FOnUnregisterEvent;
    public
      property Active: Boolean read FActive write SetActive default True;
      property EventOutput: Boolean read FEventOutput write SetEventOutput default True;
      property HotKeyList: THotKeyList read FHotKeyList stored True;
      property Platform: THotKeyPlatform read FPlatform;
      property UnixInterval: Word read FUnixInterval write SetUnixInterval default 100;
      property WineNotice: Boolean read FWineNotice write SetWineNotice default True;
    end;

    TGlobalHotKey = class(TCustomGlobalHotKey)
    published
      property OnHotKey;
      property OnRegister;
      property OnUnRegister;
    published
      property Active;
      property EventOutput;
      property UnixInterval;
      property WineNotice;
    end;

     TGlobalHotKeyEditor = class(TComponentEditor)
     public
       procedure ExecuteVerb(Index: Integer); override;
       function GetVerbCount: Integer; override;
       function GetVerb(Index: Integer): string; override;
     end;

    procedure Register;

implementation

{$R ../lazglobalhotkey.res}

uses
  FormHotKeyListEditor;

procedure Register;
begin
  RegisterComponents('GlobalHotKey', [TGlobalHotKey]);
  RegisterComponentEditor(TGlobalHotKey, TGlobalHotKeyEditor);
end;

constructor TCustomGlobalHotKey.Create(AOwner: TComponent);
var
  LForm: TCustomForm;
begin

  inherited Create(AOwner);

  LForm := GetParentForm(TControl(AOwner));

  if LForm = nil then
    raise EArgumentNilException.Create('Form is nil.');

  Create(LForm.Handle);

end;

constructor TCustomGlobalHotKey.Create(const AFormHandle: HWND);
begin

  FIncrementalCounter := 0;
  FHotKeyList := THotKeyList.Create;
  FFormHandle := AFormHandle;

  {$IfDef Unix}
  {$IfDef Darwin}
  FPlatform := THotKeyPlatformDarwin.Create(FFormHandle, FHotKeyList);
  {$Else}
  FPlatform := THotKeyPlatformUnix.Create(FFormHandle, FHotKeyList);
  SetUnixInterval(FUnixInterval);
  {$EndIf}
  {$EndIf}

  {$IfDef MSWindows}
  FPlatform := THotKeyPlatformWindows.Create(FFormHandle, FHotKeyList);
  SetWineNotice(FWineNotice);
  {$EndIf}

  EventOutput := True;
  Active := True;
  UnixInterval := 100;
  WineNotice := True;

end;

destructor TCustomGlobalHotKey.Destroy;
begin

  try
    RemoveAllGloablHotkeys;
  finally
    FHotKeyList.Free;
    FPlatform.Free;
  end;

end;

procedure TCustomGlobalHotKey.SetOnHotKeyEvent(Value: THotKeyEvent);
begin

  FOnHotKeyEvent := Value;
  FPlatform.OnHotKeyEvent := Value;

end;

procedure TCustomGlobalHotKey.SetActive(const AValue: Boolean);
begin

  if FActive <> AValue then
  begin

    FActive := AValue;
    if FActive then
      Start
    else
      Stop;

  end;

end;

procedure TCustomGlobalHotKey.SetEventOutput(const AValue: Boolean);
begin

  if AValue <> FEventOutput then
  begin
    FEventOutput := AValue;
    FPlatform.EventOutput := FEventOutput;
  end;

end;

procedure TCustomGlobalHotKey.SetUnixInterval(const AValue: Word);
begin

  if FUnixInterval <> AValue then
    FUnixInterval := AValue;

  {$IfDef Unix}
  {$IfNDef Darwin}
  THotKeyPlatformUnix(FPlatform).UnixInterval := AValue;
  {$EndIf}
  {$EndIf}

end;

procedure TCustomGlobalHotKey.SetWineNotice(const AValue: Boolean);
begin

  if FWineNotice <> AValue then
    FWineNotice := AValue;

  {$IfDef MSWindows}
  THotKeyPlatformWindows(FPlatform).WineNotice := AValue;
  {$EndIf}

end;

function TCustomGlobalHotKey.IsActive: Boolean;
begin

  Result := False;
  if not (csDesigning in ComponentState) then
    Result := FActive;

end;

procedure TCustomGlobalHotKey.Start;
begin

  ActivateAllGloablHotkeys;

end;

procedure TCustomGlobalHotKey.Stop;
begin

  UnactivateAllGloablHotkeys;

end;

procedure TCustomGlobalHotKey.StartHandler;
begin

  if not IsActive then
    Exit;

  if FHotKeyList.Count > 0 then
    FPlatform.StartHandler;

end;

procedure TCustomGlobalHotKey.StopHandler;
begin

  if not IsActive then
    Exit;

  FPlatform.StopHandler;

end;

function TCustomGlobalHotKey.RegisterGlobalHotkey(AHotKeyInfo: PTHotKeyInfo): Boolean;
begin

  if IsActive then
  begin
    Result := FPlatform.DoRegisterGlobalHotKey(AHotKeyInfo);
    if Assigned(FOnRegisterEvent) then
      FOnRegisterEvent(AHotKeyInfo, Result, FPlatform.GetError);
  end
  else
    Result := True;

end;

function TCustomGlobalHotKey.UnregisterGlobalHotkey(AHotKeyInfo: PTHotKeyInfo): Boolean;
begin

  if IsActive then
  begin
    Result := FPlatform.DoUnregisterGlobalHotkey(AHotKeyInfo);
    if Assigned(FOnUnregisterEvent) then
      FOnUnregisterEvent(AHotKeyInfo, Result, FPlatform.GetError);
  end
  else
    Result := True;

end;

function TCustomGlobalHotKey.AddGlobalHotkey(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): Boolean;
var
  iHotKeyIndex: Integer;
  LNativeShortCut: TNativeShortCut;
  LHotKeyInfo: THotKeyInfo;
begin

  iHotKeyIndex := FHotKeyList.IndexByShortCut(AShortCut);
  if iHotKeyIndex > -1 then
    Exit(True);

  LNativeShortCut := FPlatform.GetNativeShortCut(AShortCut, APlatformAdjusted);

  LHotKeyInfo := Default(THotKeyInfo);
  LHotKeyInfo.ShortCut := AShortCut;
  LHotKeyInfo.NativeShortCut := LNativeShortCut;
  LHotKeyInfo.PlatformAdjusted := APlatformAdjusted;

  Result := AddGlobalHotkey(LHotKeyInfo);

end;

function TCustomGlobalHotKey.AddGlobalHotkey(var AHotKeyInfo: THotKeyInfo): Boolean;
begin

  StopHandler;

  if FIncrementalCounter = High(Word)-1 then
    Exit(False);
  Inc(FIncrementalCounter);

  AHotKeyInfo.Id := FIncrementalCounter;

  Result := RegisterGlobalHotkey(@AHotKeyInfo);

  if Result then
    {%H-}FHotKeyList.Add(AHotKeyInfo);

  StartHandler;

end;

function TCustomGlobalHotKey.AddGlobalHotkey(const ANativeShortCut: TNativeShortCut; const APlatformAdjusted: Boolean = False): Boolean;
var
  iHotKeyIndex: Integer;
  LHotKeyInfo: THotKeyInfo;
begin

  iHotKeyIndex := FHotKeyList.IndexByNativeShortCut(ANativeShortCut);
  if iHotKeyIndex > -1 then
    Exit(True);

  LHotKeyInfo := Default(THotKeyInfo);
  LHotKeyInfo.ShortCut := VK_UNDEFINED; // TNativeShortCut support far more than just TShortCut
  LHotKeyInfo.NativeShortCut := ANativeShortCut;
  LHotKeyInfo.PlatformAdjusted := APlatformAdjusted; // Defaulting to False, as TNativeShortCut is already platform-specific.

  Result := AddGlobalHotkey(LHotKeyInfo);

end;

function TCustomGlobalHotKey.RemoveGlobalHotkey(const AShortCut: TShortCut; const APlatformAdjusted: Boolean = True): Boolean;
var
  LNativeShortCut: TNativeShortCut;
begin

  LNativeShortCut := FPlatform.GetNativeShortCut(AShortCut, APlatformAdjusted);
  Result := RemoveGlobalHotkey(FPlatform.GetNativeShortCut(LNativeShortCut));

end;

function TCustomGlobalHotKey.RemoveGlobalHotkey(const ANativeShortCut: TNativeShortCut): Boolean;
var
  iHotKeyIndex: Integer;
  LpHotKeyInfo: PTHotKeyInfo;
begin

  iHotKeyIndex := FHotKeyList.IndexByNativeShortCut(ANativeShortCut);
  if iHotKeyIndex = -1 then
    Exit;

  StopHandler;

  LpHotKeyInfo := FHotKeyList.List[iHotKeyIndex];

  Result := UnregisterGlobalHotkey(LpHotKeyInfo);

  if Result then
    FHotKeyList.Delete(iHotKeyIndex);

  StartHandler;

end;

function TCustomGlobalHotKey.ActivateAllGloablHotkeys: Boolean;
var
  iHotKeyIndex: Integer;
  LpHotKeyInfo: PTHotKeyInfo;
  ResultValue: Boolean;
begin

  Result := False;

  StopHandler;

  for iHotKeyIndex := FHotKeyList.Count-1 downto 0 do
  begin
    LpHotKeyInfo := FHotKeyList.List[iHotKeyIndex];
    ResultValue := RegisterGlobalHotkey(LpHotKeyInfo);
    Result := Result and ResultValue;
  end;

  StartHandler;

end;

function TCustomGlobalHotKey.UnactivateAllGloablHotkeys: Boolean;
var
  iHotKeyIndex: Integer;
  LpHotKeyInfo: PTHotKeyInfo;
  ResultValue: Boolean;
begin

  Result := False;

  StopHandler;

  for iHotKeyIndex := FHotKeyList.Count-1 downto 0 do
  begin
    LpHotKeyInfo := FHotKeyList.List[iHotKeyIndex];
    ResultValue := UnregisterGlobalHotkey(LpHotKeyInfo);
    Result := Result and ResultValue;
  end;

end;

function TCustomGlobalHotKey.RemoveAllGloablHotkeys: Boolean;
var
  iHotKeyIndex: Integer;
  LpHotKeyInfo: PTHotKeyInfo;
  ResultValue: Boolean;
begin

  Result := False;

  StopHandler;

  for iHotKeyIndex := FHotKeyList.Count-1 downto 0 do
  begin
      LpHotKeyInfo := FHotKeyList.List[iHotKeyIndex];
    ResultValue := UnregisterGlobalHotkey(LpHotKeyInfo);
    if ResultValue then
      FHotKeyList.Delete(iHotKeyIndex);

    Result := Result and ResultValue;

  end;

end;

{$IfDef Unix}
{$IfNDef Darwin}
function TCustomGlobalHotKey.AddGlobalHotkeyUnix(const AKeysym: TKeySym; const AMask: cint): Boolean;
begin
  Result := AddGlobalHotkey( THotKeyPlatformUnix(FPlatform).GetNativeShortCut(AKeysym, AMask) );
end;

function TCustomGlobalHotKey.RemoveGlobalHotkeyUnix(const AKeysym: TKeySym; const AMask: cint): Boolean;
begin
  Result := RemoveGlobalHotkey( THotKeyPlatformUnix(FPlatform).GetNativeShortCut(AKeysym, AMask) );
end;
{$EndIf}
{$EndIf}

{$IfDef MSWindows}
function TCustomGlobalHotKey.AddGlobalHotkeyWindows(const AKeyCode: UINT; const AModifiers: UINT): Boolean;
begin
  Result := AddGlobalHotkey( THotKeyPlatformWindows(FPlatform).GetNativeShortCut(AKeyCode, AModifiers) );
end;

function TCustomGlobalHotKey.RemoveGlobalHotkeyWindows(const AKeyCode: UINT; const AModifiers: UINT): Boolean;
begin
  Result := RemoveGlobalHotkey( THotKeyPlatformWindows(FPlatform).GetNativeShortCut(AKeyCode, AModifiers) );
end;
{$EndIf}

{$IfDef Darwin}
function TCustomGlobalHotKey.AddGlobalHotkeyDarwin(const AKeyCode: UInt32; const AModifiers: UInt32): Boolean;
begin
  Result := AddGlobalHotkey( THotKeyPlatformDarwin(FPlatform).GetNativeShortCut(AKeyCode, AModifiers) );
end;

function TCustomGlobalHotKey.RemoveGlobalHotkeyDarwin(const AKeyCode: UInt32; const AModifiers: UInt32): Boolean;
begin
  Result := RemoveGlobalHotkey( THotKeyPlatformDarwin(FPlatform).GetNativeShortCut(AKeyCode, AModifiers));
end;
{$EndIf}

procedure TCustomGlobalHotKey.DefineProperties(Filer: TFiler);
begin

  inherited;
  Filer.DefineProperty('HotKeyList', @ReadHotKeyList,  @WriteHotKeyList, FHotKeyList.Count > 0);

end;

procedure TCustomGlobalHotKey.ReadHotKeyList(Reader: TReader);
var
  sFieldName: String;
  LShortCut: TShortCut;
  bPlatformAdjusted: Boolean;
begin

  FHotKeyList.Clear;

  Reader.ReadListBegin;

  while not Reader.EndOfList do
  begin

    //LHotKeyItem := Default(THotKeyInfo);
    LShortCut := VK_UNKNOWN;
    bPlatformAdjusted := True;

    Reader.ReadListBegin;
    while not Reader.EndOfList do
    begin
      if Reader.ReadValue = vaString then
      begin
        // Reader.ReadString bugs for whatever reason. EReadError.
        // Reader.ReadVariant returns only first letter padded with zeros.
        // So bypassing it straight to Driver.
        sFieldName := Reader.Driver.ReadStr;
        case sFieldName of
          'ShortCut': LShortCut := Reader.ReadInteger;
          'PlatformAdjusted': bPlatformAdjusted := Reader.ReadBoolean;
          //'Id': LHotKeyItem.Id := Reader.ReadInteger;
          //'NativeShortCut': LHotKeyItem.NativeShortCut := Reader.ReadInteger;
          //'PlatformAdjusted': LHotKeyItem.PlatformAdjusted := Reader.ReadBoolean;
        end;
      end;
    end;

    Reader.ReadListEnd;

    //HotKeyList.Add(LHotKeyItem);
    if LShortCut <> VK_UNKNOWN then
      Self.AddGlobalHotkey(LShortCut, bPlatformAdjusted);

  end;
  Reader.ReadListEnd;

  FIncrementalCounter := HotKeyList.Count;

end;

procedure TCustomGlobalHotKey.WriteHotKeyList(Writer: TWriter);
var
  iHotKeyIndex: Integer;
begin

  Writer.WriteListBegin;
  for iHotKeyIndex := 0 to HotKeyList.Count - 1 do
  begin

    // Saving VK ShortCut, as X11 is complex and keycodes may vary for different language.
    with HotKeyList{%H-}.Items[iHotKeyIndex] do
    begin
      Writer.WriteListBegin;
      Writer.WriteString('Id');
      Writer.WriteInteger(Id);
      Writer.WriteString('ShortCut');
      Writer.WriteInteger(ShortCut);
      Writer.WriteString('PlatformAdjusted');
      Writer.WriteBoolean(PlatformAdjusted);
      Writer.WriteListEnd;
    end;

  end;
  Writer.WriteListEnd;

end;

 procedure TGlobalHotKeyEditor.ExecuteVerb(Index: Integer);
 var
   LFormHotKeyListEditor: TfrmHotKeyListEditor;
 begin

   if Index = 0 then
   begin

     // review: non-modal hotkey list editor?
     LFormHotKeyListEditor := TfrmHotKeyListEditor.Create(TGlobalHotKey(Self.Component));

     try
       LFormHotKeyListEditor.ShowModal;
       if LFormHotKeyListEditor.Modified then
         Self.Modified;
     finally
       LFormHotKeyListEditor.Free;
     end;

   end;

 end;

 function TGlobalHotKeyEditor.GetVerbCount: Integer;
 begin
   Result := 1; // Number of verbs (actions) available
 end;

 function TGlobalHotKeyEditor.GetVerb(Index: Integer): string;
 begin
   if Index = 0 then
     Result := 'Manage HotKeys...';
 end;

end.


