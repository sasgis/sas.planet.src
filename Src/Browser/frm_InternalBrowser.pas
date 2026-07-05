{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit frm_InternalBrowser;

interface

uses
  Windows,
  Messages,
  Forms,
  Classes,
  Controls,
  SysUtils,
  UITypes,
  i_Listener,
  i_WindowPositionConfig,
  i_DownloadRequest,
  i_LanguageManager,
  i_InternalBrowserFactory,
  u_InternalBrowserImpl,
  u_InternalBrowserByImpl,
  u_CommonFormAndFrameParents;

type
  TfrmInternalBrowser = class(TFormWitghLanguageManager)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    Procedure FormMove(var Msg: TWMMove); message WM_MOVE;
  private
    FBrowser: TInternalBrowserByImpl;
    FCurrentCaption: string;
    FConfig: IWindowPositionConfig;
    FConfigListener: IListener;

    procedure SetGoodCaption(const ACaption: string);

    procedure OnBrowserKeyDown(Sender: TObject; const AKey: Word; var AHandled: Boolean);
    procedure OnBrowserTitleChange(Sender: TObject; const AText: string);

    procedure OnConfigChange;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AConfig: IWindowPositionConfig;
      const AInternalBrowserFactory: IInternalBrowserFactory
    ); reintroduce;

    procedure Navigate(const ACaption, AUrl: string);
    procedure NavigateByRequest(const ACaption: string; const ARequest: IDownloadRequest);
  end;

implementation

uses
  u_HtmlToHintTextConverterStuped,
  u_ListenerByEvent,
  u_ResStrings;

{$R *.dfm}

{ TfrmInternalBrowser }

constructor TfrmInternalBrowser.Create(
  const ALanguageManager: ILanguageManager;
  const AConfig: IWindowPositionConfig;
  const AInternalBrowserFactory: IInternalBrowserFactory
);
begin
  inherited Create(ALanguageManager);

  FConfig := AConfig;
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);

  FBrowser :=
    TInternalBrowserByImpl.Create(
      Self,
      False, // IsInvisible
      Self.OnBrowserKeyDown,
      Self.OnBrowserTitleChange,
      AInternalBrowserFactory
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

procedure TfrmInternalBrowser.SetGoodCaption(const ACaption: string);
var
  VCaption: string;
begin
  VCaption := ACaption;
  if VCaption <> '' then begin
    VCaption := StringReplace(ACaption, #13#10, ', ', [rfReplaceAll]);
    VCaption := StupedHtmlToTextConverter(VCaption);
  end;
  FCurrentCaption := VCaption;
  Self.Caption := VCaption;
end;

procedure TfrmInternalBrowser.OnBrowserKeyDown(Sender: TObject; const AKey: Word; var AHandled: Boolean);
begin
  if AKey = VK_ESCAPE then begin
    AHandled := True;
    Close;
  end;
end;

procedure TfrmInternalBrowser.OnBrowserTitleChange(Sender: TObject; const AText: string);
begin
  if FCurrentCaption = '' then begin
    Self.Caption := AText;
  end;
end;

procedure TfrmInternalBrowser.FormHide(Sender: TObject);
begin
  Self.OnResize := nil;
  FConfig.ChangeNotifier.Remove(FConfigListener);

  FBrowser.SetVisible(False);
  FBrowser.AssignEmptyDocument;
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

  FBrowser.SetVisible(True);
end;

end.
