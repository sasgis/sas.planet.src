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

unit frm_StartLogo;

interface

uses
  Forms,
  Classes,
  Controls,
  ExtCtrls,
  StdCtrls,
  GR32_Image,
  i_LanguageManager,
  i_ContentTypeManager,
  i_ConfigDataProvider,
  i_StartUpLogoConfig,
  i_Listener,
  i_NotifierOperation,
  u_CommonFormAndFrameParents;

type
  TfrmStartLogo = class(TFormWitghLanguageManager)
    tmrLogo: TTimer;
    imgLogo: TImage32;
    lblVersion: TLabel;
    lblWebSite: TLabel;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure tmrLogoTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure imgLogoClick(Sender: TObject);
  private
    FReadyToHide: Boolean;
    FContentTypeManager: IContentTypeManager;
    FConfigData: IConfigDataProvider;
    FAppStartedNotifier: INotifierOneOperation;
    FAppStartedListener: IListener;
    procedure OnAppStarted;
  private
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppStartedNotifier: INotifierOneOperation;
      const AContentTypeManager: IContentTypeManager;
      const AConfigData: IConfigDataProvider
    ); reintroduce;
  public
    destructor Destroy; override;

    class procedure ShowLogo(
      const ALanguageManager: ILanguageManager;
      const AAppStartedNotifier: INotifierOneOperation;
      const AContentTypeManager: IContentTypeManager;
      const AConfigData: IConfigDataProvider;
      const AConfig: IStartUpLogoConfig
    );
  end;


implementation

uses
  Types,
  c_SasVersion,
  i_Bitmap32Static,
  u_ConfigProviderHelpers,
  u_ListenerByEvent,
  u_BitmapFunc;

var
  frmStartLogo: TfrmStartLogo;

{$R *.dfm}

constructor TfrmStartLogo.Create(
  const ALanguageManager: ILanguageManager;
  const AAppStartedNotifier: INotifierOneOperation;
  const AContentTypeManager: IContentTypeManager;
  const AConfigData: IConfigDataProvider
);
begin
  inherited Create(ALanguageManager);
  FAppStartedNotifier := AAppStartedNotifier;
  FContentTypeManager := AContentTypeManager;
  FConfigData := AConfigData;
  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppStartedNotifier.Add(FAppStartedListener);
end;

destructor TfrmStartLogo.Destroy;
begin
  if Assigned(FAppStartedNotifier) and Assigned(FAppStartedListener) then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedNotifier := nil;
    FAppStartedListener := nil;
  end;

  frmStartLogo := nil;
  inherited;
end;

procedure TfrmStartLogo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TfrmStartLogo.FormShow(Sender: TObject);
var
  VBitmapSize: TPoint;
  VBitmapStatic: IBitmap32Static;
begin
  VBitmapStatic := ReadBitmapByFileRef(FConfigData, 'sas:\Resource\LOGOI.jpg', FContentTypeManager, nil);
  if VBitmapStatic <> nil then begin
    AssignStaticToBitmap32(imgLogo.Bitmap, VBitmapStatic);
  end;
  VBitmapSize.X := imgLogo.Bitmap.Width;
  VBitmapSize.Y := imgLogo.Bitmap.Height;

  if VBitmapSize.X < 100 then begin
    VBitmapSize.X := 480;
  end;
  if VBitmapSize.Y < 100 then begin
    VBitmapSize.Y := 276;
  end;
  imgLogo.Bitmap.SetSize(VBitmapSize.X, VBitmapSize.Y);
  lblVersion.Caption:='v '+SASVersion;
  FReadyToHide := False;
  if FAppStartedNotifier.IsExecuted then begin
    OnAppStarted;
  end;
end;

procedure TfrmStartLogo.tmrLogoTimer(Sender: TObject);
begin
  tmrLogo.Enabled:=false;
  Self.Close;
end;

procedure TfrmStartLogo.imgLogoClick(Sender: TObject);
begin
  if FReadyToHide then begin
    tmrLogo.Enabled := false;
    Self.Close;
  end;
end;

procedure TfrmStartLogo.OnAppStarted;
begin
  FReadyToHide := True;
  tmrLogo.Enabled := True;
end;

class procedure TfrmStartLogo.ShowLogo(
  const ALanguageManager: ILanguageManager;
  const AAppStartedNotifier: INotifierOneOperation;
  const AContentTypeManager: IContentTypeManager;
  const AConfigData: IConfigDataProvider;
  const AConfig: IStartUpLogoConfig
);
begin
  if AConfig.IsShowLogo then begin
    TfrmStartLogo.Create(
      ALanguageManager,
      AAppStartedNotifier,
      AContentTypeManager,
      AConfigData
    ).Show;
    Application.ProcessMessages;
  end;
end;

end.
