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

unit fr_TilesGenPrev;

interface

uses
  Types,
  SysUtils,
  Classes,
  Controls,
  Forms,
  StdCtrls,
  CheckLst,
  ExtCtrls,
  t_CommonTypes,
  i_LanguageManager,
  i_ImageResamplerFactory,
  i_MapType,
  i_ImageResamplerConfig,
  i_GeometryLonLat,
  i_RegionProcessParamsFrame,
  u_CommonFormAndFrameParents,
  fr_MapSelect;

type
  IRegionProcessParamsFrameTilesGenPrev = interface(IRegionProcessParamsFrameBase)
    ['{34B156A8-D8DD-4EFF-AF55-70C93C3ADE17}']
    function GetIsReplace: Boolean;
    property IsReplace: Boolean read GetIsReplace;

    function GetIsSaveFullOnly: Boolean;
    property IsSaveFullOnly: Boolean read GetIsSaveFullOnly;

    function GetIsUseTilesFromPrevZoom: Boolean;
    property IsUseTilesFromPrevZoom: Boolean read GetIsUseTilesFromPrevZoom;

    function GetIsCreateAllFromFirstZoom: Boolean;
    property IsCreateAllFromFirstZoom: Boolean read GetIsCreateAllFromFirstZoom;

    function GetResampler: IImageResamplerFactory;
    property Resampler: IImageResamplerFactory read GetResampler;
  end;

type
  TfrTilesGenPrev = class(
      TFrame,
      IRegionProcessParamsFrameBase,
      IRegionProcessParamsFrameOneMap,
      IRegionProcessParamsFrameZoomArray,
      IRegionProcessParamsFrameTilesGenPrev
    )
    pnlBottom: TPanel;
    pnlRight: TPanel;
    pnlCenter: TPanel;
    lblStat: TLabel;
    pnlTop: TPanel;
    cbbFromZoom: TComboBox;
    lblFromZoom: TLabel;
    chkAllZooms: TCheckBox;
    lblZooms: TLabel;
    chklstZooms: TCheckListBox;
    cbbResampler: TComboBox;
    lblResampler: TLabel;
    chkReplace: TCheckBox;
    chkSaveFullOnly: TCheckBox;
    chkFromPrevZoom: TCheckBox;
    chkUsePrevTiles: TCheckBox;
    Bevel1: TBevel;
    pnlMapSelect: TPanel;
    pnlZoom: TPanel;
    pnlFrame: TPanel;
    lblMapCaption: TLabel;
    procedure cbbFromZoomChange(Sender: TObject);
    procedure chkAllZoomsClick(Sender: TObject);
    procedure chkFromPrevZoomClick(Sender: TObject);
    procedure chklstZoomsClickCheck(Sender: TObject);
    procedure chkReplaceClick(Sender: TObject);
  private
    FImageResamplerFactoryList: IImageResamplerFactoryList;
    FImageResamplerConfig: IImageResamplerConfig;
    FfrMapSelect: TfrMapSelect;
  protected
    procedure OnShow(const AIsFirstTime: Boolean); override;
  private
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  private
    function GetMapType: IMapType;
    function GetZoomArray: TByteDynArray;
    function GetAllowGenPrev(const AMapType: IMapType): boolean;
    function GetIsReplace: Boolean;
    function GetIsSaveFullOnly: Boolean;
    function GetIsUseTilesFromPrevZoom: Boolean;
    function GetIsCreateAllFromFirstZoom: Boolean;
    function GetResampler: IImageResamplerFactory;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AImageResamplerFactoryList: IImageResamplerFactoryList;
      const AImageResamplerConfig: IImageResamplerConfig
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  gnugettext,
  u_Dialogs;

{$R *.dfm}

const
  CZommDeltaMax = 8;

constructor TfrTilesGenPrev.Create(
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AImageResamplerFactoryList: IImageResamplerFactoryList;
  const AImageResamplerConfig: IImageResamplerConfig
);
begin
  TP_Ignore(Self, 'cbbResampler.Items');
  TP_Ignore(Self, 'cbbResampler.Text');
  inherited Create(ALanguageManager);
  FImageResamplerFactoryList := AImageResamplerFactoryList;
  FImageResamplerConfig := AImageResamplerConfig;
  FfrMapSelect :=
    AMapSelectFrameBuilder.Build(
      mfAll, // show maps and layers
      False,  // add -NO- to combobox
      False,  // show disabled map
      GetAllowGenPrev
    );
  FPropertyState := CreateComponentPropertyState(
    Self, [pnlMapSelect, pnlRight], [], True, False, True, True
  )
end;

destructor TfrTilesGenPrev.Destroy;
begin
  FreeAndNil(FfrMapSelect);
  inherited;
end;

function TfrTilesGenPrev.GetAllowGenPrev(const AMapType: IMapType): boolean;
begin
  Result :=
    AMapType.IsBitmapTiles and
    AMapType.TileStorage.State.GetStatic.AddAccess;
end;

procedure TfrTilesGenPrev.cbbFromZoomChange(Sender: TObject);
var
  I: integer;
begin
  chklstZooms.Items.Clear;
  for I := cbbFromZoom.ItemIndex + 1 downto 1 do begin
    chklstZooms.Items.Add(inttostr(I));
  end;
  chklstZoomsClickCheck(nil);
  chklstZooms.Repaint;
end;

procedure TfrTilesGenPrev.chkAllZoomsClick(Sender: TObject);
var
  I: integer;
begin
  if chkAllZooms.State <> cbGrayed then begin
    for I := 0 to chklstZooms.Count - 1 do begin
      if chklstZooms.ItemEnabled[I] or chkFromPrevZoom.Checked then begin
        if chkFromPrevZoom.Checked then begin
          chklstZooms.ItemEnabled[I] := true;
        end;
        chklstZooms.Checked[I] := chkAllZooms.Checked;
      end;
    end;
  end;
end;

procedure TfrTilesGenPrev.chkFromPrevZoomClick(Sender: TObject);
begin
  chklstZoomsClickCheck(nil);
end;

procedure TfrTilesGenPrev.chklstZoomsClickCheck(Sender: TObject);
var
  I: Integer;
  VLastCheckedZoom: Integer;
  VZoom: Integer;
  VSourceZoom: Integer;
  VAllChecked: Boolean;
  VAllUnChecked: Boolean;
begin
  if chkFromPrevZoom.Checked then begin
    VSourceZoom := cbbFromZoom.ItemIndex + 1;
    VLastCheckedZoom := VSourceZoom;
    I := 0;
    while I < chklstZooms.Items.Count do begin
      VZoom := VSourceZoom - I - 1;
      if VLastCheckedZoom - VZoom > CZommDeltaMax then begin
        Break;
      end else begin
        chklstZooms.ItemEnabled[I] := True;
        if chklstZooms.Checked[I] then begin
          VLastCheckedZoom := VZoom;
        end;
      end;
      Inc(I);
    end;
    while I < chklstZooms.Items.Count do begin
      chklstZooms.ItemEnabled[I] := False;
      Inc(I);
    end;
  end else begin
    for I := CZommDeltaMax to chklstZooms.Items.Count - 1 do begin
      chklstZooms.ItemEnabled[I] := false;
    end;
  end;
  VAllChecked := True;
  VAllUnChecked := True;
  for I := 0 to chklstZooms.Items.Count - 1 do begin
    if chklstZooms.ItemEnabled[I] then begin
      if chklstZooms.Checked[I] then begin
        VAllUnChecked := False;
      end else begin
        VAllChecked := False;
      end;
    end;
  end;
  if VAllChecked then begin
    chkAllZooms.State := cbChecked;
  end else if VAllUnChecked then begin
    chkAllZooms.State := cbUnchecked;
  end else begin
    chkAllZooms.State := cbGrayed;
  end;
end;

procedure TfrTilesGenPrev.chkReplaceClick(Sender: TObject);
begin
  chkUsePrevTiles.Enabled := chkReplace.Checked;
end;

function TfrTilesGenPrev.GetIsCreateAllFromFirstZoom: Boolean;
begin
  Result := not chkFromPrevZoom.Checked;
end;

function TfrTilesGenPrev.GetIsReplace: Boolean;
begin
  Result := chkReplace.Checked;
end;

function TfrTilesGenPrev.GetIsSaveFullOnly: Boolean;
begin
  Result := chkSaveFullOnly.Checked;
end;

function TfrTilesGenPrev.GetIsUseTilesFromPrevZoom: Boolean;
begin
  Result := chkUsePrevTiles.Checked;
end;

function TfrTilesGenPrev.GetMapType: IMapType;
begin
  Result := FfrMapSelect.GetSelectedMapType;
end;

function TfrTilesGenPrev.GetResampler: IImageResamplerFactory;
begin
  try
    if cbbResampler.ItemIndex >= 0 then begin
      Result := FImageResamplerFactoryList.Items[cbbResampler.ItemIndex];
    end else begin
      Result := FImageResamplerFactoryList.Items[0];
    end;
  except
    Result := FImageResamplerFactoryList.Items[0];
  end;
end;

function TfrTilesGenPrev.GetZoomArray: TByteDynArray;
var
  I: Integer;
  VCount: Integer;
  VSourceZoom: Byte;
begin
  Result := nil;
  VCount := 1;
  SetLength(Result, VCount);
  VSourceZoom := cbbFromZoom.ItemIndex + 1;
  Result[0] := VSourceZoom;
  if VSourceZoom > 0 then begin
    for I := 0 to VSourceZoom - 1 do begin
      if chklstZooms.ItemEnabled[I] then begin
        if chklstZooms.Checked[I] then begin
          SetLength(Result, VCount + 1);
          Result[VCount] := VSourceZoom - 1 - I;
          Inc(VCount);
        end;
      end;
    end;
  end;
end;

procedure TfrTilesGenPrev.OnShow(const AIsFirstTime: Boolean);
var
  I: Integer;
begin
  if AIsFirstTime then begin
    with cbbFromZoom.Items do begin
      BeginUpdate;
      Clear;
      for I := 2 to 24 do begin
        Add(IntToStr(I));
      end;
      EndUpdate;
    end;

    with cbbResampler.Items do begin
      BeginUpdate;
      Clear;
      for I := 0 to FImageResamplerFactoryList.Count - 1 do begin
        Add(FImageResamplerFactoryList.Captions[I]);
      end;
      EndUpdate;
    end;
    cbbResampler.DropDownCount := cbbResampler.Items.Count;
  end;

  inherited; // restore state

  if AIsFirstTime then begin
    if cbbResampler.ItemIndex < 0 then begin
      cbbResampler.ItemIndex := FImageResamplerFactoryList.GetIndexByGUID(FImageResamplerConfig.ActiveGUID);
    end;
  end;
end;

procedure TfrTilesGenPrev.Init(
  const AZoom: byte;
  const APolygon: IGeometryLonLatPolygon
);
begin
  if AZoom > 0 then begin
    cbbFromZoom.ItemIndex := AZoom - 1;
  end else begin
    cbbFromZoom.ItemIndex := 0;
  end;
  cbbFromZoomChange(cbbFromZoom);

  FfrMapSelect.Show(pnlFrame);
end;

function TfrTilesGenPrev.Validate: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to chklstZooms.Count - 1 do begin
    if chklstZooms.ItemEnabled[I] then begin
      if chklstZooms.Checked[I] then begin
        Result := True;
        Break;
      end;
    end;
  end;
  if not Result then begin
    ShowErrorMessage(_('Please select at least one zoom'));
  end else begin
    Result := FfrMapSelect.GetSelectedMapType <> nil;
    if not Result then begin
      ShowErrorMessage(_('Please select a map'));
    end;
  end;
end;

end.
