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
  i_LanguageManager,
  i_ProxySettings;

type
  TfrmIntrnalBrowser = class(TFormWitghLanguageManager)
    EmbeddedWB1: TEmbeddedWB;
    imgViewImage: TImgView32;
    procedure FormDestroy(Sender: TObject);
    procedure EmbeddedWB1Authenticate(
      Sender: TCustomEmbeddedWB;
      var hwnd: HWND;
      var szUserName, szPassWord: WideString;
      var Rezult: HRESULT
    );
    procedure FormClose(
      Sender: TObject;
      var Action: TCloseAction
    );
    procedure EmbeddedWB1KeyDown(
      Sender: TObject;
      var Key: Word;
      ScanCode: Word;
      Shift: TShiftState
    );
    procedure FormCreate(Sender: TObject);
    procedure EmbeddedWB1BeforeNavigate2(
      ASender: TObject;
      const pDisp: IDispatch;
      var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant;
      var Cancel: WordBool
    );
    procedure EmbeddedWB1TitleChange(
      ASender: TObject;
      const Text: WideString
    );
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imgViewImageClick(Sender: TObject);
    Procedure FormMove(Var Msg: TWMMove); Message WM_MOVE;
  private
    FCurrentCaption: string;
    FProxyConfig: IProxyConfig;
    FConfig: IWindowPositionConfig;
    FContentTypeManager: IContentTypeManager;

    FConfigListener: IListener;
    procedure OnConfigChange;
    procedure SetGoodCaption(const ACaption: String);
    function OpenLocalImage(const AFileName: WideString): Boolean;
    procedure ResetImageView(const AForImage: Boolean);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AConfig: IWindowPositionConfig;
      const AProxyConfig: IProxyConfig;
      const AContentTypeManager: IContentTypeManager
    ); reintroduce;

    procedure Navigate(const ACaption, AUrl: string);
    procedure NavigatePost(const ACaption, AUrl, AReferer, APostData: string);
  end;

implementation

uses
  Variants,
  u_HtmlToHintTextConverterStuped,
  u_ListenerByEvent,
  u_ResStrings;

{$R *.dfm}

{ TfrmIntrnalBrowser }

constructor TfrmIntrnalBrowser.Create(
  const ALanguageManager: ILanguageManager;
  const AConfig: IWindowPositionConfig;
  const AProxyConfig: IProxyConfig;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create(ALanguageManager);
  FConfig := AConfig;
  FContentTypeManager := AContentTypeManager;
  FProxyConfig := AProxyConfig;

  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
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

procedure TfrmIntrnalBrowser.EmbeddedWB1Authenticate(
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

procedure TfrmIntrnalBrowser.EmbeddedWB1BeforeNavigate2(
  ASender: TObject;
  const pDisp: IDispatch;
  var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant;
  var Cancel: WordBool
);
var
  VURL: WideString;
begin
  if Cancel then begin
    Exit;
  end;
  try
    VURL := URL;

    // check file exists and known image type
    if System.Pos(':', VURL) > 0 then begin
      Exit;
    end;

    // check file exists
    if FileExists(VURL) then begin
      if Assigned(FContentTypeManager.GetBitmapLoaderByFileName(VURL)) then begin
        if OpenLocalImage(VURL) then begin
        // image opened
          Cancel := TRUE;
        end;
      end;
    end;
  except
  end;
end;

procedure TfrmIntrnalBrowser.FormClose(
  Sender: TObject;
  var Action: TCloseAction
);
begin
  EmbeddedWB1.Stop;
end;

procedure TfrmIntrnalBrowser.FormCreate(Sender: TObject);
begin
  if IsRectEmpty(FConfig.BoundsRect) then begin
    FConfig.SetWindowPosition(Self.BoundsRect);
  end;

  EmbeddedWB1.Navigate('about:blank');
end;

procedure TfrmIntrnalBrowser.imgViewImageClick(Sender: TObject);
begin
  ResetImageView(FALSE);
end;

procedure TfrmIntrnalBrowser.Navigate(const ACaption, AUrl: string);
begin
  EmbeddedWB1.HTMLCode.Text := SAS_STR_WiteLoad;
  SetGoodCaption(ACaption);
  ResetImageView(FALSE);
  show;
  EmbeddedWB1.Navigate(AUrl);
end;

procedure TfrmIntrnalBrowser.NavigatePost(const ACaption, AUrl, AReferer, APostData: string);
var
  VPostData, VHeaders: OleVariant;
  VFlags: OleVariant;
  VTargetFrameName: OleVariant;
  i: Integer;
begin
  EmbeddedWB1.HTMLCode.Text := SAS_STR_WiteLoad;
  SetGoodCaption(ACaption);
  ResetImageView(FALSE);
  show;

  VPostData := VarArrayCreate([0, Length(APostData) - 1], varByte);
  for i := 1 to Length(APostData) do begin
    VPostData[i - 1] := Ord(APostData[i]);
  end;

  VHeaders := 'Referer: ' + AReferer + #$D#$A +
    'Content-Type: application/x-www-form-urlencoded';

  VFlags := EmptyParam;
  VTargetFrameName := EmptyParam;
  EmbeddedWB1.Navigate(AUrl, VFlags, VTargetFrameName, VPostData, VHeaders);
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

function TfrmIntrnalBrowser.OpenLocalImage(const AFileName: WideString): Boolean;
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
  EmbeddedWB1.Visible := (not AForImage);
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

procedure TfrmIntrnalBrowser.EmbeddedWB1KeyDown(
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

procedure TfrmIntrnalBrowser.EmbeddedWB1TitleChange(
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
