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

unit fr_ProjectionSelect;

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
  i_Listener,
  i_Notifier,
  i_ProjectionSet,
  i_ProjectionSetList,
  i_ViewProjectionConfig,
  i_LanguageManager,
  u_CommonFormAndFrameParents, StdCtrls;

type
  TfrProjectionSelect = class(TFrame)
    lblProjection: TLabel;
    cbbProjection: TComboBox;
    procedure cbbProjectionChange(Sender: TObject);
  private
    FItems: array of record
      Caption: string;
      ProjSet: IProjectionSet;
    end;
    FSelected: Integer;
    FProjectionSetList: IProjectionSetList;
    FViewProjectionConfig: IViewProjectionConfig;
    FConfigChangeListener: IListener;
    FConfigChangeNotifier: INotifier;
    FOnProjectionChange: TNotifyEvent;
  private
    function BuilProjCaption(
      const AProj: IProjectionSet;
      const ACaptionPrefix: string
    ): string;
    function GetProjIndexByEPSG(
      const AEPSG: Integer
    ): Integer;
    procedure SetProjection(
      const AProj: IProjectionSet;
      const AIsMapProj: Boolean
    );
    procedure ValidateSelected;
    procedure RefreshStrings;
    procedure OnConfigChange;
  protected
    procedure RefreshTranslation; override;
  public
    procedure SetMapProjection(const AProj: IProjectionSet);
    procedure SetLayerProjection(const AProj: IProjectionSet);
    function GetSelectedProjection: IProjectionSet;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AProjectionSetList: IProjectionSetList;
      const AViewProjectionConfig: IViewProjectionConfig
    ); reintroduce;
    destructor Destroy; override;
    procedure Show(AParent: TWinControl);
    property OnProjectionChange: TNotifyEvent read FOnProjectionChange write FOnProjectionChange;
  end;

implementation

uses
  u_ListenerByEvent;

resourcestring
  rsProjectionOfMap = 'Projection of map';
  rsProjectionOfLayer = 'Projection of layer';

{$R *.dfm}

{ TfrProjectionSelect }

constructor TfrProjectionSelect.Create(
  const ALanguageManager: ILanguageManager;
  const AProjectionSetList: IProjectionSetList;
  const AViewProjectionConfig: IViewProjectionConfig
);
var
  I: Integer;
begin
  inherited Create(ALanguageManager);

  FProjectionSetList := AProjectionSetList;
  FViewProjectionConfig := AViewProjectionConfig;

  FConfigChangeNotifier := FViewProjectionConfig.ChangeNotifier;
  if FConfigChangeNotifier <> nil then begin
    FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
    FConfigChangeNotifier.Add(FConfigChangeListener);
  end;

  SetLength(FItems, FProjectionSetList.Count + 2);

  FItems[0].ProjSet := nil; // reserved for Map projection
  FItems[1].ProjSet := nil; // reserved for Layer projection

  for I := 2 to Length(FItems) - 1 do begin
    FItems[I].Caption := FProjectionSetList.Captions[I - 2];
    FItems[I].ProjSet := FProjectionSetList.Items[I - 2];
  end;

  FSelected := -1;
  cbbProjection.Items.Clear;
end;

destructor TfrProjectionSelect.Destroy;
begin
  if (FConfigChangeNotifier <> nil) and (FConfigChangeListener <> nil) then begin
    FConfigChangeNotifier.Remove(FConfigChangeListener);
    FConfigChangeNotifier := nil;
  end;
  inherited Destroy;
end;

procedure TfrProjectionSelect.Show(AParent: TWinControl);
begin
  Parent := AParent;
  RefreshStrings;
end;

procedure TfrProjectionSelect.OnConfigChange;
var
  VEPSG: Integer;
begin
  VEPSG := FViewProjectionConfig.EPSG;
  if VEPSG <> 0 then begin
    FSelected := GetProjIndexByEPSG(VEPSG) + 2;
  end;
  ValidateSelected;
  RefreshStrings;
end;

procedure TfrProjectionSelect.ValidateSelected;
begin
  if (FItems[0].ProjSet = nil) and (FItems[1].ProjSet = nil) then begin
    FSelected := -1;
    Exit;
  end;

  if (FSelected = 0) and (FItems[0].ProjSet = nil) then begin
    Inc(FSelected);
  end;

  if (FSelected = 1) and (FItems[1].ProjSet = nil) then begin
    Dec(FSelected);
  end;
end;

function TfrProjectionSelect.GetProjIndexByEPSG(const AEPSG: Integer): Integer;
var
  I: Integer;
  VItem: IProjectionSet;
begin
  Result := -1;
  for I := 0 to FProjectionSetList.Count - 1 do begin
    VItem := FProjectionSetList.Items[I];
    if VItem.Zooms[0].ProjectionType.ProjectionEPSG = AEPSG then begin
      Result := I;
      Break;
    end;
  end;
end;

function TfrProjectionSelect.GetSelectedProjection: IProjectionSet;
begin
  if FSelected <> -1 then begin
    Result := FItems[FSelected].ProjSet;
  end else begin
    Result := nil;
  end;
end;

procedure TfrProjectionSelect.SetProjection(
  const AProj: IProjectionSet;
  const AIsMapProj: Boolean
);
var
  I: Integer;
  VEPSG: Integer;
  VCaption: string;
begin
  if AIsMapProj then begin
    I := 0;
    VCaption := rsProjectionOfMap;
  end else begin
    I := 1;
    VCaption := rsProjectionOfLayer;
  end;

  VEPSG := FViewProjectionConfig.EPSG;

  if FSelected = -1 then begin
    if VEPSG = 0 then begin
      FSelected := I;
    end else begin
      FSelected := GetProjIndexByEPSG(VEPSG) + 2;
    end;
  end;

  FItems[I].ProjSet := AProj;
  FItems[I].Caption := BuilProjCaption(AProj, VCaption);

  ValidateSelected;

  RefreshStrings;
end;

procedure TfrProjectionSelect.SetMapProjection(const AProj: IProjectionSet);
begin
  SetProjection(AProj, True);
end;

procedure TfrProjectionSelect.SetLayerProjection(const AProj: IProjectionSet);
begin
  SetProjection(AProj, False);
end;

function TfrProjectionSelect.BuilProjCaption(
  const AProj: IProjectionSet;
  const ACaptionPrefix: string
): string;
var
  I: Integer;
begin
  Result := '';

  if AProj = nil then begin
    Exit;
  end;

  for I := 0 to FProjectionSetList.Count - 1 do begin
    if FProjectionSetList.Items[I].IsSame(AProj) then begin
      Result := ACaptionPrefix + ' - ' + FProjectionSetList.Captions[I];
      Break;
    end;
  end;

  if Result = '' then begin
    Result := ACaptionPrefix + ' - ' + '<UNKNOWN>';
  end;
end;

procedure TfrProjectionSelect.RefreshStrings;
var
  I: Integer;
  VItemIndex: Integer;
begin
  cbbProjection.Items.Clear;

  if (FItems[0].ProjSet = nil) and (FItems[1].ProjSet = nil) then begin
    Exit;
  end;

  VItemIndex := -1;
  for I := 0 to Length(FItems) - 1 do begin
    if FItems[I].ProjSet <> nil then begin
      cbbProjection.Items.Add(FItems[I].Caption);
      if FSelected = I then begin
        VItemIndex := cbbProjection.Items.Count - 1;
      end;
    end;
  end;

  cbbProjection.ItemIndex := VItemIndex;
end;

procedure TfrProjectionSelect.cbbProjectionChange(Sender: TObject);
var
  VItemIndex: Integer;
begin
  VItemIndex := cbbProjection.ItemIndex;

  if (FItems[0].ProjSet = nil) or (FItems[1].ProjSet = nil) then begin
    if VItemIndex > 0 then begin
      FSelected := VItemIndex + 1;
    end;
  end else begin
    FSelected := VItemIndex;
  end;

  ValidateSelected;

  if Assigned(FOnProjectionChange) then begin
    FOnProjectionChange(Self);
  end;
end;

procedure TfrProjectionSelect.RefreshTranslation;
begin
  inherited RefreshTranslation;

  FItems[0].Caption := BuilProjCaption(FItems[0].ProjSet, rsProjectionOfMap);
  FItems[1].Caption := BuilProjCaption(FItems[1].ProjSet, rsProjectionOfLayer);

  RefreshStrings;
end;

end.
