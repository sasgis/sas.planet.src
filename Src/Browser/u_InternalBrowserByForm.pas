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

unit u_InternalBrowserByForm;

interface

uses
  i_Listener,
  i_NotifierOperation,
  i_DownloadRequest,
  i_LanguageManager,
  i_InternalBrowser,
  i_InternalBrowserFactory,
  i_InternalBrowserLastContent,
  i_WindowPositionConfig,
  u_BaseInterfacedObject,
  frm_InternalBrowser;

type
  TInternalBrowserByForm = class(TBaseInterfacedObject, IInternalBrowser)
  private
    FLanguageManager: ILanguageManager;
    FConfig: IWindowPositionConfig;
    FContent: IInternalBrowserLastContent;
    FInternalBrowserFactory: IInternalBrowserFactory;
    FfrmInternalBrowser: TfrmInternalBrowser;
    FAppStartedListener: IListener;
    FAppStartedNotifier: INotifierOneOperation;
  private
    procedure SafeCreateInternal;
    procedure SafeCreateInternalAsync;
  private
    { IInternalBrowser }
    procedure ShowMessage(const ACaption, AText: string);
    procedure Navigate(const ACaption, AUrl: string);
    procedure NavigateByRequest(const ACaption: string; const ARequest: IDownloadRequest);
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AContent: IInternalBrowserLastContent;
      const AConfig: IWindowPositionConfig;
      const AInternalBrowserFactory: IInternalBrowserFactory;
      const AAppStartedNotifier: INotifierOneOperation;
      const APreInitBrowserEngine: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  SysUtils,
  c_InternalBrowser,
  u_ListenerByEvent;

{ TInternalBrowserByForm }

constructor TInternalBrowserByForm.Create(
  const ALanguageManager: ILanguageManager;
  const AContent: IInternalBrowserLastContent;
  const AConfig: IWindowPositionConfig;
  const AInternalBrowserFactory: IInternalBrowserFactory;
  const AAppStartedNotifier: INotifierOneOperation;
  const APreInitBrowserEngine: Boolean
);
begin
  inherited Create;

  FLanguageManager := ALanguageManager;
  FContent := AContent;
  FConfig := AConfig;
  FInternalBrowserFactory := AInternalBrowserFactory;
  FAppStartedNotifier := AAppStartedNotifier;

  if Assigned(FAppStartedNotifier) and APreInitBrowserEngine then begin
    FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.SafeCreateInternalAsync);
    FAppStartedNotifier.Add(FAppStartedListener);
  end;
end;

destructor TInternalBrowserByForm.Destroy;
begin
  if Assigned(FAppStartedNotifier) and Assigned(FAppStartedListener) then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedListener := nil;
  end;
  FreeAndNil(FfrmInternalBrowser);
  inherited;
end;

procedure TInternalBrowserByForm.ShowMessage(const ACaption, AText: string);
begin
  SafeCreateInternal;
  FContent.Content := AText;
  FfrmInternalBrowser.Navigate(ACaption, CShowMessageInternalURL);
end;

procedure TInternalBrowserByForm.Navigate(const ACaption, AUrl: string);
begin
  SafeCreateInternal;
  FfrmInternalBrowser.Navigate(ACaption, AUrl);
end;

procedure TInternalBrowserByForm.NavigateByRequest(
  const ACaption: string;
  const ARequest: IDownloadRequest
);
begin
  SafeCreateInternal;
  FfrmInternalBrowser.NavigateByRequest(ACaption, ARequest);
end;

procedure TInternalBrowserByForm.SafeCreateInternal;
begin
  if FfrmInternalBrowser = nil then begin
    FfrmInternalBrowser :=
      TfrmInternalBrowser.Create(
        FLanguageManager,
        FConfig,
        FInternalBrowserFactory
      );
  end;
end;

procedure TInternalBrowserByForm.SafeCreateInternalAsync;
begin
  TThread.CreateAnonymousThread(
    procedure
    begin
      TThread.Synchronize(nil, SafeCreateInternal);
    end
  ).Start;
end;

end.
