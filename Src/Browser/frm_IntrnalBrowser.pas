{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
  EwbCore,
  EmbeddedWB,
  SHDocVw_EWB,
  GR32_Image,
  i_Listener,
  u_CommonFormAndFrameParents,
  i_WindowPositionConfig,
  i_ContentTypeManager,
  i_DownloadRequest,
  i_LanguageManager,
  i_InterfaceListStatic,
  i_ProxySettings;

type
  TfrmIntrnalBrowser = class(TFormWitghLanguageManager)
    imgViewImage: TImgView32;
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
    procedure imgViewImageClick(Sender: TObject);
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
    FContentTypeManager: IContentTypeManager;
    FInternalDomainUrlHandlerList: IInterfaceListStatic;

    FConfigListener: IListener;
    procedure OnConfigChange;
    procedure SetGoodCaption(const ACaption: String);
    function OpenLocalImage(const AFileName: string): Boolean;
    procedure ResetImageView(const AForImage: Boolean);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AConfig: IWindowPositionConfig;
      const AProxyConfig: IProxyConfig;
      const AContentTypeManager: IContentTypeManager;
      const AInternalDomainUrlHandlerList: IInterfaceListStatic
    ); reintroduce;

    procedure Navigate(const ACaption, AUrl: string);
    procedure NavigateByRequest(const ACaption: string; const ARequest: IDownloadRequest);
  end;

implementation

uses
  Variants,
  Dialogs,
  c_InternalBrowser,
  i_InternalDomainUrlHandler,
  u_HtmlToHintTextConverterStuped,
  u_ListenerByEvent,
  u_ResStrings;

{$R *.dfm}

{ TfrmIntrnalBrowser }

constructor TfrmIntrnalBrowser.Create(
  const ALanguageManager: ILanguageManager;
  const AConfig: IWindowPositionConfig;
  const AProxyConfig: IProxyConfig;
  const AContentTypeManager: IContentTypeManager;
  const AInternalDomainUrlHandlerList: IInterfaceListStatic
);
begin
  inherited Create(ALanguageManager);
  FConfig := AConfig;
  FContentTypeManager := AContentTypeManager;
  FProxyConfig := AProxyConfig;
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FInternalDomainUrlHandlerList := AInternalDomainUrlHandlerList;

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
  I: Integer;
  VUrl: string;
  VUrlHandler: IInternalDomainUrlHandler;
begin
  if Cancel then begin
    Exit;
  end;

  try
    VUrl := URL;

    if Assigned(FInternalDomainUrlHandlerList) and (Pos(CSASInternalURLPrefix, VUrl) > 0) then begin
      for I := 0 to FInternalDomainUrlHandlerList.Count - 1 do begin
        VUrlHandler := IInternalDomainUrlHandler(FInternalDomainUrlHandlerList.Items[I]);
        if Assigned(VUrlHandler) and VUrlHandler.Process(VUrl) then begin
          Cancel := True;
          Exit;
        end;
      end;
    end;

    // check file exists and known image type
    if System.Pos(':', VUrl) > 0 then begin
      Exit;
    end;

    // check file exists
    if FileExists(VUrl) then begin
      if Assigned(FContentTypeManager.GetBitmapLoaderByFileName(VUrl)) then begin
        if OpenLocalImage(VUrl) then begin
        // image opened
          Cancel := TRUE;
        end;
      end;
    end;
  except
    on E: Exception do begin
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

  FEmbeddedWB.Navigate('about:blank');
end;

procedure TfrmIntrnalBrowser.imgViewImageClick(Sender: TObject);
begin
  ResetImageView(FALSE);
end;

procedure TfrmIntrnalBrowser.Navigate(const ACaption, AUrl: string);
begin
  FEmbeddedWB.HTMLCode.Text := SAS_STR_WiteLoad;
  SetGoodCaption(ACaption);
  ResetImageView(FALSE);
  show;
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
  ResetImageView(FALSE);
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

function TfrmIntrnalBrowser.OpenLocalImage(const AFileName: string): Boolean;
begin
  Result := FALSE;
  ResetImageView(TRUE);
  try
    imgViewImage.Bitmap.LoadFromFile(AFileName);
    Inc(Result);
  except
  end;
end;

procedure TfrmIntrnalBrowser.ResetImageView(const AForImage: Boolean);
begin
  imgViewImage.Bitmap.Clear;
  imgViewImage.Visible := AForImage;
  FEmbeddedWB.Visible := (not AForImage);
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
    VK_BACK: begin
      if imgViewImage.Visible then begin
        ResetImageView(FALSE);
      end;
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
  FEmbeddedWB.Navigate('about:blank');
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
