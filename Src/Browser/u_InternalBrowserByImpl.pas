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

unit u_InternalBrowserByImpl;

interface

uses
  Controls,
  i_Listener,
  i_DownloadRequest,
  i_InternalBrowserFactory,
  u_InternalBrowserImpl;

type
  TInternalBrowserByImpl = class
  private
    FParent: TWinControl;
    FIsInvisible: Boolean;
    FOnKeyDown: TOnKeyDown;
    FOnTitleChange: TOnTitleChange;
    FInternalBrowserFactory: IInternalBrowserFactory;

    FOnBeforeImplChangeListener: IListener;
    FOnAfterImplChangeListener: IListener;

    FImpl: TInternalBrowserImpl;

    FImplRestore: Boolean;
    FImplVisible: Boolean;
    FImplAddress: string;

    procedure DoCreateImpl;
    procedure DoReleaseImpl;
  public
    function Initialize: Boolean; inline;
    procedure AssignEmptyDocument; inline;
    procedure Navigate(const AUrl: string); overload; inline;
    procedure Navigate(const ARequest: IDownloadRequest); overload; inline;
    function NavigateWait(const AUrl: string; const ATimeOut: Cardinal): Boolean; inline;
    procedure SetHtmlText(const AText: string); inline;
    procedure Stop; inline;
    procedure SetVisible(const AIsVisible: Boolean); inline;
  public
    constructor Create(
      const AParent: TWinControl;
      const AIsInvisible: Boolean;
      const AOnKeyDown: TOnKeyDown;
      const AOnTitleChange: TOnTitleChange;
      const AInternalBrowserFactory: IInternalBrowserFactory
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ListenerByEvent;

{ TInternalBrowserByImpl }

constructor TInternalBrowserByImpl.Create(
  const AParent: TWinControl;
  const AIsInvisible: Boolean;
  const AOnKeyDown: TOnKeyDown;
  const AOnTitleChange: TOnTitleChange;
  const AInternalBrowserFactory: IInternalBrowserFactory
);
begin
  Assert(AInternalBrowserFactory <> nil);

  inherited Create;

  FParent := AParent;
  FIsInvisible := AIsInvisible;
  FOnKeyDown := AOnKeyDown;
  FOnTitleChange := AOnTitleChange;
  FInternalBrowserFactory := AInternalBrowserFactory;

  DoCreateImpl;

  FOnBeforeImplChangeListener := TNotifyNoMmgEventListener.Create(Self.DoReleaseImpl);
  FInternalBrowserFactory.BeforeChangeNotifier.Add(FOnBeforeImplChangeListener);

  FOnAfterImplChangeListener := TNotifyNoMmgEventListener.Create(Self.DoCreateImpl);
  FInternalBrowserFactory.AfterChangeNotifier.Add(FOnAfterImplChangeListener);
end;

destructor TInternalBrowserByImpl.Destroy;
begin
  if Assigned(FInternalBrowserFactory) and Assigned(FOnBeforeImplChangeListener) then begin
    FInternalBrowserFactory.BeforeChangeNotifier.Remove(FOnBeforeImplChangeListener);
    FOnBeforeImplChangeListener := nil;
  end;

  if Assigned(FInternalBrowserFactory) and Assigned(FOnAfterImplChangeListener) then begin
    FInternalBrowserFactory.BeforeChangeNotifier.Remove(FOnAfterImplChangeListener);
    FOnAfterImplChangeListener := nil;
  end;

  DoReleaseImpl;

  FInternalBrowserFactory := nil;
  inherited Destroy;
end;

procedure TInternalBrowserByImpl.DoCreateImpl;
begin
  FImpl :=
    FInternalBrowserFactory.CreateBrowserImpl(
      FParent,
      FIsInvisible,
      FOnKeyDown,
      FOnTitleChange
    );

  if FImplRestore and FImplVisible and FImpl.Initialize then begin
    FImpl.SetVisible(FImplVisible);
    FImpl.Navigate(FImplAddress);
  end;
end;

procedure TInternalBrowserByImpl.DoReleaseImpl;
begin
  if (FImpl <> nil) and not FIsInvisible then begin
    FImplRestore := True;
    FImplVisible := FImpl.GetVisible;
    FImplAddress := FImpl.GetCurrentAddress;
  end else begin
    FImplRestore := False;
  end;

  FreeAndNil(FImpl);
end;

function TInternalBrowserByImpl.Initialize: Boolean;
begin
  if FImpl <> nil then begin
    Result := FImpl.Initialize;
  end else begin
    Result := False;
  end;
end;

procedure TInternalBrowserByImpl.AssignEmptyDocument;
begin
  if FImpl <> nil then
    FImpl.AssignEmptyDocument;
end;

procedure TInternalBrowserByImpl.Navigate(const AUrl: string);
begin
  if FImpl <> nil then
    FImpl.Navigate(AUrl);
end;

procedure TInternalBrowserByImpl.Navigate(const ARequest: IDownloadRequest);
begin
  if FImpl <> nil then
    FImpl.Navigate(ARequest);
end;

function TInternalBrowserByImpl.NavigateWait(const AUrl: string; const ATimeOut: Cardinal): Boolean;
begin
  if FImpl <> nil then begin
    Result := FImpl.NavigateWait(AUrl, ATimeOut);
  end else begin
    Result := False;
  end;
end;

procedure TInternalBrowserByImpl.SetHtmlText(const AText: string);
begin
  if FImpl <> nil then
    FImpl.SetHtmlText(AText);
end;

procedure TInternalBrowserByImpl.SetVisible(const AIsVisible: Boolean);
begin
  if FImpl <> nil then
    FImpl.SetVisible(AIsVisible);
end;

procedure TInternalBrowserByImpl.Stop;
begin
  if FImpl <> nil then
    FImpl.Stop;
end;

end.
