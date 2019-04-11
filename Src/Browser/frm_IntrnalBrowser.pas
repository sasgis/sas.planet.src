{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit frm_IntrnalBrowser;

interface

uses
  Windows,
  Messages,
  Forms,
  Classes,
  Controls,
  OleCtrls,
  SysUtils,
  UITypes,
  EwbCore,
  EmbeddedWB,
  SHDocVw_EWB,
  i_Listener,
  u_CommonFormAndFrameParents,
  i_WindowPositionConfig,
  i_DownloadRequest,
  i_LanguageManager,
  i_InternalDomainUrlHandler,
  i_ProxySettings;

type
  TfrmIntrnalBrowser = class(TFormWitghLanguageManager)
    procedure OnEmbeddedWBAuthenticate(
      Sender: TCustomEmbeddedWB;
      var hwnd: HWND;
      var szUserName, szPassWord: WideString;
      var Rezult: HRESULT
    );
    procedure OnEmbeddedWBKeyDown(
      Sender: TObject;
      var Key: Word;
      ScanCode: Word;
      Shift: TShiftState
    );
    procedure OnEmbeddedWBBeforeNavigate2(
      ASender: TObject;
      const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant;
      var Cancel: WordBool
    );
    procedure OnEmbeddedWBTitleChange(
      ASender: TObject;
      const Text: WideString
    );
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    Procedure FormMove(Var Msg: TWMMove); Message WM_MOVE;
  private
    FEmbeddedWB: TEmbeddedWB;
    FCurrentCaption: string;
    FProxyConfig: IProxyConfig;
    FConfig: IWindowPositionConfig;
    FInternalDomainUrlHandler: IInternalDomainUrlHandler;
    FConfigListener: IListener;
    procedure OnConfigChange;
    procedure SetGoodCaption(const ACaption: String);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AConfig: IWindowPositionConfig;
      const AProxyConfig: IProxyConfig;
      const AInternalDomainUrlHandler: IInternalDomainUrlHandler
    ); reintroduce;

    procedure Navigate(const ACaption, AUrl: string);
    procedure NavigateByRequest(const ACaption: string; const ARequest: IDownloadRequest);
  end;

implementation

uses
  Variants,
  Dialogs,
  u_HtmlToHintTextConverterStuped,
  u_ListenerByEvent,
  u_ResStrings;

const
  CEmptyPage = 'about:blank';

{$R *.dfm}

{ TfrmIntrnalBrowser }

constructor TfrmIntrnalBrowser.Create(
  const ALanguageManager: ILanguageManager;
  const AConfig: IWindowPositionConfig;
  const AProxyConfig: IProxyConfig;
  const AInternalDomainUrlHandler: IInternalDomainUrlHandler
);
begin
  inherited Create(ALanguageManager);
  FConfig := AConfig;
  FProxyConfig := AProxyConfig;
  FInternalDomainUrlHandler := AInternalDomainUrlHandler;
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);

  FEmbeddedWB := TEmbeddedWB.Create(Self);
  FEmbeddedWB.Name := 'IntrnalBrowserEmbeddedWB';
  FEmbeddedWB.Parent := Self;
  FEmbeddedWB.Left := 0;
  FEmbeddedWB.Top := 0;
  FEmbeddedWB.Align := alClient;
  FEmbeddedWB.Silent := False;
  FEmbeddedWB.OnTitleChange := OnEmbeddedWBTitleChange;
  FEmbeddedWB.OnBeforeNavigate2 := OnEmbeddedWBBeforeNavigate2;
  FEmbeddedWB.DisableCtrlShortcuts := 'N';
  FEmbeddedWB.UserInterfaceOptions := [EnablesFormsAutoComplete, EnableThemes];
  FEmbeddedWB.OnAuthenticate := OnEmbeddedWBAuthenticate;
  FEmbeddedWB.About := '';
  FEmbeddedWB.PrintOptions.HTMLHeader.Clear;
  FEmbeddedWB.PrintOptions.HTMLHeader.Add('<HTML></HTML>');
  FEmbeddedWB.PrintOptions.Orientation := poPortrait;
  FEmbeddedWB.OnKeyDown := OnEmbeddedWBKeyDown;
end;

procedure TfrmIntrnalBrowser.FormDestroy(Sender: TObject);
begin
  if FConfig <> nil then begin
    if FConfigListener <> nil then begin
      FConfig.ChangeNotifier.Remove(FConfigListener);
      FConfigListener := nil;
    end;
  end;
end;

procedure TfrmIntrnalBrowser.OnEmbeddedWBAuthenticate(
  Sender: TCustomEmbeddedWB;
  var hwnd: HWND;
  var szUserName, szPassWord: WideString;
  var Rezult: HRESULT
);
var
  VProxyConfig: IProxyConfigStatic;
  VUseLogin: Boolean;
begin
  VProxyConfig := FProxyConfig.GetStatic;
  VUseLogin := (not VProxyConfig.UseIESettings) and VProxyConfig.UseProxy and VProxyConfig.UseLogin;
  if VUseLogin then begin
    szUserName := VProxyConfig.Login;
    szPassWord := VProxyConfig.Password;
  end;
end;

procedure TfrmIntrnalBrowser.OnEmbeddedWBBeforeNavigate2(
  ASender: TObject;
  const pDisp: IDispatch;
  var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant;
  var Cancel: WordBool
);
var
  VUrl: string;
begin
  if Cancel then begin
    Exit;
  end;

  try
    VUrl := LowerCase(URL);
    if VUrl = CEmptyPage then begin
      Exit;
    end;

    if Assigned(FInternalDomainUrlHandler) and FInternalDomainUrlHandler.Process(VUrl) then begin
      Cancel := True;
      Exit;
    end;
  except
    on E: Exception do begin
      Cancel := True;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmIntrnalBrowser.FormClose(
  Sender: TObject;
  var Action: TCloseAction
);
begin
  FEmbeddedWB.Stop;
end;

procedure TfrmIntrnalBrowser.FormCreate(Sender: TObject);
begin
  if IsRectEmpty(FConfig.BoundsRect) then begin
    FConfig.SetWindowPosition(Self.BoundsRect);
  end;
  FEmbeddedWB.Navigate(CEmptyPage);
end;

procedure TfrmIntrnalBrowser.Navigate(const ACaption, AUrl: string);
begin
  FEmbeddedWB.HTMLCode.Text := SAS_STR_WiteLoad;
  SetGoodCaption(ACaption);
  Show;
  FEmbeddedWB.Navigate(AUrl);
end;

procedure TfrmIntrnalBrowser.NavigateByRequest(
  const ACaption: string;
  const ARequest: IDownloadRequest
);
var
  VPostData, VHeaders: OleVariant;
  VFlags: OleVariant;
  VTargetFrameName: OleVariant;
  VPostRequest: IDownloadPostRequest;
  VSafeArray: PVarArray;
begin
  FEmbeddedWB.HTMLCode.Text := SAS_STR_WiteLoad;
  SetGoodCaption(ACaption);
  Show;

  VPostData := EmptyParam;
  if Supports(ARequest, IDownloadPostRequest, VPostRequest) then begin
    VPostData := VarArrayCreate([0, VPostRequest.PostData.Size - 1], varByte);
    VSafeArray := VarArrayAsPSafeArray(VPostData);
    Move(VPostRequest.PostData.Buffer^, VSafeArray.Data^, VPostRequest.PostData.Size);
  end;
  VHeaders := ARequest.RequestHeader;
  VFlags := EmptyParam;
  VTargetFrameName := EmptyParam;
  FEmbeddedWB.Navigate(ARequest.Url, VFlags, VTargetFrameName, VPostData, VHeaders);
end;

procedure TfrmIntrnalBrowser.OnConfigChange;
var
  VRect: TRect;
begin
  VRect := FConfig.BoundsRect;
  if not EqualRect(BoundsRect, VRect) then begin
    BoundsRect := VRect;
  end;
end;

procedure TfrmIntrnalBrowser.SetGoodCaption(const ACaption: String);
var
  VCaption: String;
begin
  VCaption := ACaption;
  if VCaption <> '' then begin
    VCaption := StringReplace(ACaption, #13#10, ', ', [rfReplaceAll]);
    VCaption := StupedHtmlToTextConverter(VCaption);
  end;
  FCurrentCaption := VCaption;
  Self.Caption := VCaption;
end;

procedure TfrmIntrnalBrowser.OnEmbeddedWBKeyDown(
  Sender: TObject;
  var Key: Word;
  ScanCode: Word;
  Shift: TShiftState
);
begin
  case Key of
    VK_ESCAPE: begin
      Close;
    end;
  end;
end;

procedure TfrmIntrnalBrowser.OnEmbeddedWBTitleChange(
  ASender: TObject;
  const Text: WideString
);
begin
  if FCurrentCaption = '' then begin
    Self.Caption := Text;
  end;
end;

procedure TfrmIntrnalBrowser.FormHide(Sender: TObject);
begin
  FEmbeddedWB.Navigate(CEmptyPage);
  Self.OnResize := nil;
  FConfig.ChangeNotifier.Remove(FConfigListener);
end;

procedure TfrmIntrnalBrowser.FormMove(var Msg: TWMMove);
begin
  Inherited;
  if Assigned(Self.OnResize) then begin
    Self.OnResize(Self);
  end;
end;

procedure TfrmIntrnalBrowser.FormResize(Sender: TObject);
begin
  if Self.WindowState = wsNormal then begin
    FConfig.SetWindowPosition(BoundsRect);
  end;
end;

procedure TfrmIntrnalBrowser.FormShow(Sender: TObject);
begin
  FConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;
  Self.OnResize := FormResize;
end;

end.
