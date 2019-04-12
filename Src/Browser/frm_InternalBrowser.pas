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

unit frm_InternalBrowser;

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
  i_Listener,
  i_WindowPositionConfig,
  i_DownloadRequest,
  i_LanguageManager,
  i_InternalDomainUrlHandler,
  i_InetConfig,
  u_InternalBrowserImplByIE,
  u_CommonFormAndFrameParents;

type
  TfrmInternalBrowser = class(TFormWitghLanguageManager)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    Procedure FormMove(Var Msg: TWMMove); Message WM_MOVE;
  private
    FBrowser: TInternalBrowserImplByIE;
    FCurrentCaption: string;
    FConfig: IWindowPositionConfig;
    FConfigListener: IListener;

    procedure OnBrowserKeyDown(
      Sender: TObject;
      var Key: Word;
      ScanCode: Word;
      Shift: TShiftState
    );
    procedure OnBrowserTitleChange(ASender: TObject; const Text: string);
    procedure OnConfigChange;
    procedure SetGoodCaption(const ACaption: String);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AConfig: IWindowPositionConfig;
      const AInetConfig: IInetConfig;
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

{$R *.dfm}

{ TfrmInternalBrowser }

constructor TfrmInternalBrowser.Create(
  const ALanguageManager: ILanguageManager;
  const AConfig: IWindowPositionConfig;
  const AInetConfig: IInetConfig;
  const AInternalDomainUrlHandler: IInternalDomainUrlHandler
);
begin
  Assert(AInetConfig <> nil);
  Assert(AInternalDomainUrlHandler <> nil);

  inherited Create(ALanguageManager);

  FConfig := AConfig;
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);

  FBrowser :=
    TInternalBrowserImplByIE.Create(
      Self,
      False,
      AInetConfig.ProxyConfig,
      AInternalDomainUrlHandler,
      AInetConfig.UserAgentString,
      OnBrowserKeyDown,
      OnBrowserTitleChange
    );
end;

procedure TfrmInternalBrowser.FormDestroy(Sender: TObject);
begin
  if FConfig <> nil then begin
    if FConfigListener <> nil then begin
      FConfig.ChangeNotifier.Remove(FConfigListener);
      FConfigListener := nil;
    end;
  end;
  FreeAndNil(FBrowser);
end;

procedure TfrmInternalBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FBrowser.Stop;
end;

procedure TfrmInternalBrowser.FormCreate(Sender: TObject);
begin
  if IsRectEmpty(FConfig.BoundsRect) then begin
    FConfig.SetWindowPosition(Self.BoundsRect);
  end;
  FBrowser.AssignEmptyDocument;
end;

procedure TfrmInternalBrowser.Navigate(const ACaption, AUrl: string);
begin
  FBrowser.SetHtmlText(SAS_STR_WiteLoad);
  SetGoodCaption(ACaption);
  Show;
  FBrowser.Navigate(AUrl);
end;

procedure TfrmInternalBrowser.NavigateByRequest(const ACaption: string; const ARequest: IDownloadRequest);
begin
  FBrowser.SetHtmlText(SAS_STR_WiteLoad);
  SetGoodCaption(ACaption);
  Show;
  FBrowser.Navigate(ARequest);
end;

procedure TfrmInternalBrowser.OnConfigChange;
var
  VRect: TRect;
begin
  VRect := FConfig.BoundsRect;
  if not EqualRect(BoundsRect, VRect) then begin
    BoundsRect := VRect;
  end;
end;

procedure TfrmInternalBrowser.SetGoodCaption(const ACaption: String);
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

procedure TfrmInternalBrowser.OnBrowserKeyDown(
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

procedure TfrmInternalBrowser.OnBrowserTitleChange(ASender: TObject; const Text: string);
begin
  if FCurrentCaption = '' then begin
    Self.Caption := Text;
  end;
end;

procedure TfrmInternalBrowser.FormHide(Sender: TObject);
begin
  FBrowser.AssignEmptyDocument;
  Self.OnResize := nil;
  FConfig.ChangeNotifier.Remove(FConfigListener);
end;

procedure TfrmInternalBrowser.FormMove(var Msg: TWMMove);
begin
  inherited;
  if Assigned(Self.OnResize) then begin
    Self.OnResize(Self);
  end;
end;

procedure TfrmInternalBrowser.FormResize(Sender: TObject);
begin
  if Self.WindowState = wsNormal then begin
    FConfig.SetWindowPosition(BoundsRect);
  end;
end;

procedure TfrmInternalBrowser.FormShow(Sender: TObject);
begin
  FConfig.ChangeNotifier.Add(FConfigListener);
  OnConfigChange;
  Self.OnResize := FormResize;
end;

end.
