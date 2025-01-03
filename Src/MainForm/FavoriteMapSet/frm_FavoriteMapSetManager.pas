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

unit frm_FavoriteMapSetManager;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  fr_FavoriteMapSetManager,
  frm_FavoriteMapSetEditor,
  i_MapTypeSet,
  i_Listener,
  i_LanguageManager,
  i_FavoriteMapSetConfig,
  i_FavoriteMapSetHelper,
  i_CoordToStringConverter,
  u_CommonFormAndFrameParents;

type
  TfrmFavoriteMapSetManager = class(TFormWitghLanguageManager)
    pnlBottomButtons: TPanel;
    btnClose: TButton;
    pnlMapSets: TPanel;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
  private
    FfrFavoriteMapSetManager: TfrFavoriteMapSetManager;
    FFavoriteMapSetConfig: IFavoriteMapSetConfig;
    FConfigChangeListener: IListener;
    procedure OnConfigChange;
    procedure RestoreState;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapsSet: IMapTypeSet;
      const ACoordToStringConverter: ICoordToStringConverterChangeable;
      const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
      const AFavoriteMapSetHelper: IFavoriteMapSetHelper;
      const AFavoriteMapSetEditor: TfrmFavoriteMapSetEditor
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

{$R *.dfm}

{ TfrmFavoriteMapSetManager }

constructor TfrmFavoriteMapSetManager.Create(
  const ALanguageManager: ILanguageManager;
  const AMapsSet: IMapTypeSet;
  const ACoordToStringConverter: ICoordToStringConverterChangeable;
  const AFavoriteMapSetConfig: IFavoriteMapSetConfig;
  const AFavoriteMapSetHelper: IFavoriteMapSetHelper;
  const AFavoriteMapSetEditor: TfrmFavoriteMapSetEditor
);
begin
  inherited Create(ALanguageManager);

  FFavoriteMapSetConfig := AFavoriteMapSetConfig;

  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FFavoriteMapSetConfig.ChangeNotifier.Add(FConfigChangeListener);

  FfrFavoriteMapSetManager :=
    TfrFavoriteMapSetManager.Create(
      ALanguageManager,
      AMapsSet,
      ACoordToStringConverter,
      AFavoriteMapSetConfig,
      AFavoriteMapSetHelper,
      AFavoriteMapSetEditor
    );
  FfrFavoriteMapSetManager.Parent := pnlMapSets;

  FPropertyState := CreateComponentPropertyState(
    Self, [], [], True, False, False, True
  );
  FPropertyState.Include(Self.Name, ['Top', 'Left']);

  RestoreState;
end;

destructor TfrmFavoriteMapSetManager.Destroy;
begin
  if FFavoriteMapSetConfig <> nil then begin
    FFavoriteMapSetConfig.ChangeNotifier.Remove(FConfigChangeListener);
    FFavoriteMapSetConfig := nil;
  end;
  FreeAndNil(FfrFavoriteMapSetManager);
  inherited Destroy;
end;

procedure TfrmFavoriteMapSetManager.RestoreState;
var
  VRect: TRect;
begin
  FPropertyState.Restore;

  if (Self.Top <> 0) and (Self.Left <> 0) then begin
    VRect := Self.BoundsRect;
    UpdateRectByMonitors(VRect);
    if not EqualRect(VRect, Self.BoundsRect) then begin
      Self.BoundsRect := VRect;
    end;
    Self.Position := poDesigned;
  end else begin
    Self.Position := poMainFormCenter;
  end;
end;

procedure TfrmFavoriteMapSetManager.FormHide(Sender: TObject);
begin
  FfrFavoriteMapSetManager.Hide;
end;

procedure TfrmFavoriteMapSetManager.FormShow(Sender: TObject);
begin
  FfrFavoriteMapSetManager.Init;
  FfrFavoriteMapSetManager.Show;
end;

procedure TfrmFavoriteMapSetManager.OnConfigChange;
begin
  if Self.Visible then begin
    FfrFavoriteMapSetManager.Init;
  end;
end;

procedure TfrmFavoriteMapSetManager.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
