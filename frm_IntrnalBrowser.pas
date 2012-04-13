{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
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
  u_CommonFormAndFrameParents,
  u_BaseTileDownloaderThread,
  i_MapAttachmentsInfo,
  i_ContentTypeManager,
  i_LanguageManager,
  i_ProxySettings;

const
  WM_PARSER_THREAD_FINISHED = WM_USER + $200;

type
  TfrmIntrnalBrowser = class(TFormWitghLanguageManager)
    EmbeddedWB1: TEmbeddedWB;
    imgViewImage: TImgView32;
    procedure EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB;
      var hwnd: HWND; var szUserName, szPassWord: WideString;
      var Rezult: HRESULT);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EmbeddedWB1KeyDown(Sender: TObject; var Key: Word;
      ScanCode: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure EmbeddedWB1BeforeNavigate2(ASender: TObject;
      const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
      Headers: OleVariant; var Cancel: WordBool);
    procedure imgViewImageClick(Sender: TObject);
  private
    FParserThread: TBaseTileDownloaderThread;
    procedure KillParserThread;
    procedure WMPARSER_THREAD_FINISHED(var m: TMessage); message WM_PARSER_THREAD_FINISHED;
  private
    FProxyConfig: IProxyConfig;
    FContentTypeManager: IContentTypeManager;
    procedure SetGoodCaption(const ACaption: String);
    function OpenLocalImage(const AFilename: WideString): Boolean;
    procedure ResetImageView(const AForImage: Boolean);
  public
    constructor Create(ALanguageManager: ILanguageManager; AProxyConfig: IProxyConfig; AContentTypeManager: IContentTypeManager); reintroduce;

    procedure showmessage(const ACaption, AText: string);
    procedure Navigate(const ACaption, AUrl: string);

    procedure ShowHTMLDescrWithParser(const ACaption, AText: string;
                                      const AParserProc: TMapAttachmentsInfoParserProc);
  end;

implementation

uses
  u_HtmlToHintTextConverterStuped,
  u_ResStrings;

{$R *.dfm}

type
  TMapAttachmentsInfoParserThread = class(TBaseTileDownloaderThread)
  private
    FParserSourceText: String;
    FParserProc: TMapAttachmentsInfoParserProc;
    FForm: TfrmIntrnalBrowser;
  protected
    procedure Execute; override;
  end;

{ TfrmIntrnalBrowser }
 
constructor TfrmIntrnalBrowser.Create(ALanguageManager: ILanguageManager;
  AProxyConfig: IProxyConfig; AContentTypeManager: IContentTypeManager);
begin
  inherited Create(ALanguageManager);
  FContentTypeManager := AContentTypeManager;
  FProxyConfig := AProxyConfig;
end;

procedure TfrmIntrnalBrowser.EmbeddedWB1Authenticate(Sender: TCustomEmbeddedWB; var hwnd: HWND; var szUserName, szPassWord: WideString; var Rezult: HRESULT);
var
  VProxyConfig: IProxyConfigStatic;
  VUseLogin: Boolean;
begin
  VProxyConfig := FProxyConfig.GetStatic;
  VUselogin := (not VProxyConfig.UseIESettings) and VProxyConfig.UseProxy and VProxyConfig.UseLogin;
  if VUselogin then begin
    szUserName := VProxyConfig.Login;
    szPassWord := VProxyConfig.Password;
  end;
end;

procedure TfrmIntrnalBrowser.EmbeddedWB1BeforeNavigate2(ASender: TObject;
  const pDisp: IDispatch; var URL, Flags, TargetFrameName, PostData,
  Headers: OleVariant; var Cancel: WordBool);
var
  VURL, VExt: WideString;
begin
  if Cancel then
    Exit;
  try
    VURL := URL;

    // check file exists and known image type
    if System.Pos(':',VURL)>0 then
      Exit;

    // check file exists
    if FileExists(VURL) then begin
      VExt := ExtractFileExt(VURL);
      if FContentTypeManager.GetIsBitmapExt(VExt) then
      if OpenLocalImage(VURL) then begin
        // image opened
        Cancel := TRUE;
      end;
    end;
  except
  end;
end;

procedure TfrmIntrnalBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  EmbeddedWB1.Stop;
  KillParserThread;
end;

procedure TfrmIntrnalBrowser.FormCreate(Sender: TObject);
begin
  EmbeddedWB1.Navigate('about:blank');
end;

procedure TfrmIntrnalBrowser.imgViewImageClick(Sender: TObject);
begin
  ResetImageView(FALSE);
end;

procedure TfrmIntrnalBrowser.KillParserThread;
begin
  if (FParserThread<>nil) then begin
    FParserThread.Terminate;
    FreeAndNil(FParserThread);
  end;
end;

procedure TfrmIntrnalBrowser.Navigate(const ACaption, AUrl: string);
begin
  EmbeddedWB1.HTMLCode.Text:=SAS_STR_WiteLoad;
  SetGoodCaption(ACaption);
  ResetImageView(FALSE);
  show;
  EmbeddedWB1.Navigate(AUrl);
end;

function TfrmIntrnalBrowser.OpenLocalImage(const AFilename: WideString): Boolean;
begin
  Result := FALSE;
  ResetImageView(TRUE);
  try
    imgViewImage.Bitmap.LoadFromFile(AFilename);
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
var VCaption: String;
begin
  VCaption := StringReplace(ACaption,#13#10,', ',[rfReplaceAll]);
  Caption := StupedHtmlToTextConverter(VCaption);
end;

procedure TfrmIntrnalBrowser.ShowHTMLDescrWithParser(const ACaption, AText: string;
                                                     const AParserProc: TMapAttachmentsInfoParserProc);
var
  VText: String;
  VOnlyCheckAllowRunImmediately: Boolean;
  VError: Boolean;
begin
  // kill prev call
  KillParserThread;

  EmbeddedWB1.GoAboutBlank;
  Application.ProcessMessages; // sometimes it shows empty window without this line (only for first run)

  VError:=FALSE;

  if Assigned(AParserProc) then begin
    // if in cache - show immediately
    VOnlyCheckAllowRunImmediately := TRUE;
    try
      VText := AText;
      AParserProc(Self, VText, VOnlyCheckAllowRunImmediately);
      if (not VOnlyCheckAllowRunImmediately) then begin
        // cannot wait for download - show stub
        VText := SAS_STR_WiteLoad;
      end;
    except
      // on except
      on E: Exception do begin
        VText := E.ClassName + ': ' + E.Message;
        VError := TRUE;
      end;
    end;
  end else
    VText := AText;

  EmbeddedWB1.HTMLCode.Text := VText;
  Application.ProcessMessages;
  SetGoodCaption(ACaption);
  ResetImageView(FALSE);
  Self.Show;

  // parse after show in thread
  if (not VError) then
  if Assigned(AParserProc) then begin
    FParserThread := TMapAttachmentsInfoParserThread.Create(TRUE);
    with TMapAttachmentsInfoParserThread(FParserThread) do begin
      FParserSourceText := AText;
      FParserProc := AParserProc;
      FForm := Self;
    end;
    FParserThread.FreeOnTerminate:=FALSE;
    FParserThread.Resume;
  end;
end;

procedure TfrmIntrnalBrowser.showmessage(const ACaption,AText: string);
begin
  EmbeddedWB1.GoAboutBlank;
  Application.ProcessMessages; // sometimes it shows empty window without this line (only for first run)
  EmbeddedWB1.HTMLCode.Text:=AText;
  SetGoodCaption(ACaption);
  ResetImageView(FALSE);
  show;
end;

procedure TfrmIntrnalBrowser.WMPARSER_THREAD_FINISHED(var m: TMessage);
begin
 if (FParserThread<>nil) then begin
    if (not FParserThread.Terminated) then
      EmbeddedWB1.HTMLCode.Text := TMapAttachmentsInfoParserThread(FParserThread).FParserSourceText;
    KillParserThread;
  end;
end;

procedure TfrmIntrnalBrowser.EmbeddedWB1KeyDown(Sender: TObject; var Key: Word;
  ScanCode: Word; Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE: begin
      Close;
    end;
    VK_BACK: begin
      if imgViewImage.Visible then
        ResetImageView(FALSE);
    end;
  end;
end;

{ TMapAttachmentsInfoParserThread }

procedure TMapAttachmentsInfoParserThread.Execute;
var VOnlyCheckAllowRunImmediately: Boolean;
begin
  //inherited;

  // download and parse
  if (Self<>nil) and (not Self.Terminated) then
  try
    // full download
    VOnlyCheckAllowRunImmediately := FALSE;
    FParserProc(Self, FParserSourceText, VOnlyCheckAllowRunImmediately);
  except
    on E: Exception do
      FParserSourceText := E.Classname + ': ' + E.Message;
  end;

  // notify form (show page)
  if (Self<>nil) and (not Self.Terminated) then
    PostMessage(FForm.Handle, WM_PARSER_THREAD_FINISHED, 0, 0);
end;

end.
