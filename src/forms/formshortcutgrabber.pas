unit FormShortCutGrabber;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, LCLType,
  LCLProc, Menus, ExtCtrls, PopupNotifier;

type

  { TfrmShortCutGrabber }

  TfrmShortCutGrabber = class(TForm)
    btnGrab: TButton;
    btnCancel: TButton;
    btnUngrab: TButton;
    chbShift: TCheckBox;
    chbControl: TCheckBox;
    chbSuper: TCheckBox;
    chbAlt: TCheckBox;
    cbKeyGrab: TComboBox;
    lblShortCutPreview: TLabel;
    FHintWindow: THintWindow;
    procedure btnGrabClick(Sender: TObject);
    procedure cbKeyGrabChange(Sender: TObject);
    procedure cbKeyGrabKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckBoxStateChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  protected var
    FShortCut: TShortCut;
    FPreviewShortCut: TShortCut;
    FUngrabButton: Boolean;
  protected
    procedure ListKeys;
    function GetShortCut: TShortCut;
    procedure OnKeyDown(var Key: Word; const Shift: TShiftState);
    procedure SetCheckBoxes(const AShiftState: TShiftState);
    procedure SetKeyComboBox(var AKeyCode: Word);
    procedure SetShortCut;
    procedure SetPreviewShortCut;
  private

  public
    property ShortCut: TShortCut read FShortCut write FShortCut;
    property UngrabButton: Boolean read FUngrabButton write FUngrabButton;
  end;

var
  frmShortCutGrabber: TfrmShortCutGrabber;

implementation

{$R *.lfm}

{ TfrmShortCutGrabber }

procedure TfrmShortCutGrabber.FormCreate(Sender: TObject);
begin

  FShortCut := VK_UNDEFINED;
  FPreviewShortCut := VK_UNDEFINED;

  FHintWindow := THintWindow.Create(Self);
  with FHintWindow do
  begin
    AutoHide := True;
    HideInterval := 2200;
    Hint := cbKeyGrab.Hint;
    HintRect := CalcHintRect(0, Hint, nil);
  end;

  ListKeys;

end;

procedure TfrmShortCutGrabber.FormDestroy(Sender: TObject);
begin

  FHintWindow.Free;

end;

procedure TfrmShortCutGrabber.FormShow(Sender: TObject);
begin

  btnUngrab.Visible := FUngrabButton;
  if FShortCut <> 0 then
    SetShortCut;

end;

procedure TfrmShortCutGrabber.ListKeys;

  procedure AddKey(AKeyCode: Word);
  begin
    cbKeyGrab.AddItem(ShortCutToTextRaw(KeyToShortCut(AKeyCode, [])), TObject(PtrUInt(AKeyCode)));
  end;

begin

  //AddKey(VK_BACK);
  AddKey(VK_TAB);
  AddKey(VK_RETURN);

  AddKey(VK_PAUSE);

  AddKey(VK_ESCAPE);
  AddKey(VK_SPACE);

  AddKey(VK_PRIOR); // (PgUp)
  AddKey(VK_NEXT); // (PgDown)
  AddKey(VK_END);
  AddKey(VK_HOME);

  AddKey(VK_LEFT);
  AddKey(VK_UP);
  AddKey(VK_RIGHT);
  AddKey(VK_DOWN);

  AddKey(VK_SNAPSHOT);

  AddKey(VK_INSERT);
  AddKey(VK_DELETE);

  AddKey(VK_0);
  AddKey(VK_1);
  AddKey(VK_2);
  AddKey(VK_3);
  AddKey(VK_4);
  AddKey(VK_5);
  AddKey(VK_6);
  AddKey(VK_7);
  AddKey(VK_8);
  AddKey(VK_9);

  AddKey(VK_A);
  AddKey(VK_B);
  AddKey(VK_C);
  AddKey(VK_D);
  AddKey(VK_E);
  AddKey(VK_F);
  AddKey(VK_G);
  AddKey(VK_H);
  AddKey(VK_I);
  AddKey(VK_J);
  AddKey(VK_K);
  AddKey(VK_L);
  AddKey(VK_M);
  AddKey(VK_N);
  AddKey(VK_O);
  AddKey(VK_P);
  AddKey(VK_Q);
  AddKey(VK_R);
  AddKey(VK_S);
  AddKey(VK_T);
  AddKey(VK_U);
  AddKey(VK_V);
  AddKey(VK_W);
  AddKey(VK_X);
  AddKey(VK_Y);
  AddKey(VK_Z);

  AddKey(VK_APPS); // (Menu)

  AddKey(VK_NUMPAD0);
  AddKey(VK_NUMPAD1);
  AddKey(VK_NUMPAD2);
  AddKey(VK_NUMPAD3);
  AddKey(VK_NUMPAD4);
  AddKey(VK_NUMPAD5);
  AddKey(VK_NUMPAD6);
  AddKey(VK_NUMPAD7);
  AddKey(VK_NUMPAD8);
  AddKey(VK_NUMPAD9);
  AddKey(VK_MULTIPLY);
  AddKey(VK_ADD);
  AddKey(VK_SUBTRACT);
  AddKey(VK_DECIMAL);
  AddKey(VK_DIVIDE);

  AddKey(VK_F1);
  AddKey(VK_F2);
  AddKey(VK_F3);
  AddKey(VK_F4);
  AddKey(VK_F5);
  AddKey(VK_F6);
  AddKey(VK_F7);
  AddKey(VK_F8);
  AddKey(VK_F9);
  AddKey(VK_F10);
  AddKey(VK_F11);
  AddKey(VK_F12);
  AddKey(VK_F13);
  AddKey(VK_F14);
  AddKey(VK_F15);
  AddKey(VK_F16);
  AddKey(VK_F17);
  AddKey(VK_F18);
  AddKey(VK_F19);
  AddKey(VK_F20);
  AddKey(VK_F21);
  AddKey(VK_F22);
  AddKey(VK_F23);
  AddKey(VK_F24);

  AddKey(VK_VOLUME_MUTE);
  AddKey(VK_VOLUME_DOWN);
  AddKey(VK_VOLUME_UP);

  AddKey(VK_OEM_1); // (Semicolon ;)
  AddKey(VK_OEM_PLUS); // (Plus +)
  AddKey(VK_OEM_COMMA); // (Comma ,)
  AddKey(VK_OEM_MINUS); // (Minus -)
  AddKey(VK_OEM_PERIOD); // (Period .)

  AddKey(VK_OEM_2); // (Slash /)
  AddKey(VK_OEM_3); // (Tilde ~)
  AddKey(VK_OEM_4); // (Open bracket [)
  AddKey(VK_OEM_5); // (Back slash \)
  AddKey(VK_OEM_6); // (Close bracket ])
  AddKey(VK_OEM_7); // (Quote ")

end;

function TfrmShortCutGrabber.GetShortCut: TShortCut;
var
  LShiftState: TShiftState;
  Key: Word;
begin

  LShiftState := [];
  Key := VK_UNDEFINED;

  if chbShift.Checked then
    Include(LShiftState, ssShift);
  if chbControl.Checked then
    Include(LShiftState, ssCtrl);
  if chbSuper.Checked then
    Include(LShiftState, ssMeta);
  if chbAlt.Checked then
    Include(LShiftState, ssAlt);

  if cbKeyGrab.ItemIndex > 0 then
    Key := PtrUInt(cbKeyGrab.Items.Objects[cbKeyGrab.ItemIndex]);

  Result := KeyToShortCut(Key, LShiftState);

  FPreviewShortCut := Result;

end;

procedure TfrmShortCutGrabber.OnKeyDown(var Key: Word; const Shift: TShiftState);
begin

  SetCheckBoxes(Shift);
  SetKeyComboBox(Key);

  if (FPreviewShortCut = VK_ESCAPE) and (Key = VK_ESCAPE) then
    ModalResult := mrCancel;

  Key := 0;

  SetPreviewShortCut;

end;

procedure TfrmShortCutGrabber.SetCheckBoxes(const AShiftState: TShiftState);
begin

  chbShift.Checked := (TShiftStateEnum.ssShift in AShiftState);
  chbControl.Checked := (TShiftStateEnum.ssCtrl in AShiftState);
  chbSuper.Checked := (TShiftStateEnum.ssMeta in AShiftState);
  chbAlt.Checked := (TShiftStateEnum.ssAlt in AShiftState);

end;

procedure TfrmShortCutGrabber.SetKeyComboBox(var AKeyCode: Word);
begin

  cbKeyGrab.ItemIndex := cbKeyGrab.Items.IndexOfObject(TObject(PtrUInt(AKeyCode)));

end;

procedure TfrmShortCutGrabber.SetShortCut;
var
  wKeyCode: Word;
  LShiftState: TShiftState;
begin

  ShortCutToKey(FShortCut, wKeyCode, LShiftState);

  SetCheckBoxes(LShiftState);
  SetKeyComboBox(wKeyCode);

end;

procedure TfrmShortCutGrabber.SetPreviewShortCut;
begin

  lblShortCutPreview.Caption := ShortCutToTextRaw(GetShortCut);
  lblShortCutPreview.Font.Color := clDefault;

end;

procedure TfrmShortCutGrabber.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  OnKeyDown(Key, Shift);

end;

procedure TfrmShortCutGrabber.cbKeyGrabKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  OnKeyDown(Key, Shift);

end;

procedure TfrmShortCutGrabber.btnGrabClick(Sender: TObject);
begin

  if cbKeyGrab.ItemIndex > -1 then
  begin

    FShortCut := GetShortCut;
    ModalResult := mrOK;

  end
  else
  begin

    ModalResult := mrNone;

    with cbKeyGrab.ClientToScreen(cbKeyGrab.ClientRect) do
    begin
    FHintWindow.ActivateHint(
      Rect(
        Left, Top - FHintWindow.HintRect.Height - Scale96ToForm(4),
        Right - (Width-FHintWindow.HintRect.Width), Top - Scale96ToForm(4)
      ),
      FHintWindow.Hint
    );
    end;

  end;

end;

procedure TfrmShortCutGrabber.cbKeyGrabChange(Sender: TObject);
begin

  SetPreviewShortCut;

end;

procedure TfrmShortCutGrabber.CheckBoxStateChange(Sender: TObject);
begin

  SetPreviewShortCut;

end;

end.

