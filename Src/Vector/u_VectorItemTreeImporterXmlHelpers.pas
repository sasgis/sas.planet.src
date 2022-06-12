{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_VectorItemTreeImporterXmlHelpers;

interface

uses
  System.Generics.Collections,
  vsagps_public_kml;

type
  TKmlStyleItem = class
  private
    FAvailParams: Tvsagps_KML_params;

    FColor: Cardinal;
    FBgColor: Cardinal;
    FTextColor: Cardinal;
    FTileSize: Cardinal;
    FScale: Double;
    FFill: Byte;
    FWidth: Byte;
  public
    procedure ReadFromKmlData(const AData: Pvsagps_KML_ParserData);
    procedure WriteToKmlData(const AData: Pvsagps_KML_ParserData);
  public
    constructor Create(const AData: Pvsagps_KML_ParserData = nil);
  end;

  TKmlStyleList = class
  private
    FDict: TObjectDictionary<string, TKmlStyleItem>;
  public
    procedure AddStyle(const AStyleId: string; const AStyle: TKmlStyleItem);
    function TryGetStyle(const AStyleId: string; out AStyle: TKmlStyleItem): Boolean;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TKmlStyleMap = class
  private
    type
      TKmlStyleMapItem = record
        Normal: string;
        Highlight: string;
      end;
  private
    FDict: TDictionary<string, TKmlStyleMapItem>;
  public
    procedure AddPair(const AStyleId: string; const AKey, AStyleUrl: string);
    function TryGetStyleUrl(const AStyleId: string; out AStyleUrl: string): Boolean;
    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;
  end;

function TryStyleUrlToStyleId(const AStyleUrl: string; out AStyleId: string): Boolean;

implementation

uses
  SysUtils;

function TryStyleUrlToStyleId(const AStyleUrl: string; out AStyleId: string): Boolean;
begin
  // In <styleUrl>, for referenced style elements that are local to the KML document,
  // a simple # referencing is used. For styles that are contained in external files,
  // use a full URL along with # referencing

  // https://developers.google.com/kml/documentation/kmlreference#elements-specific-to-stylemap

  if (Length(AStyleUrl) > 1) and (AStyleUrl[1] = '#') then begin
    AStyleId := Copy(AStyleUrl, 2);
    Result := AStyleId <> '';
  end else begin
    // styles in external files are not supported
    Result := False;
  end;
end;

{ TKmlStyleItem }

constructor TKmlStyleItem.Create(const AData: Pvsagps_KML_ParserData);
begin
  inherited Create;
  FAvailParams := [];
  if AData <> nil then begin
    ReadFromKmlData(AData);
  end;
end;

procedure TKmlStyleItem.ReadFromKmlData(const AData: Pvsagps_KML_ParserData);
begin
  Assert(AData <> nil);

  FAvailParams := AData.fAvail_params;

  if kml_color in FAvailParams then begin
    FColor := AData.fValues.color;
  end;
  if kml_bgColor in FAvailParams then begin
    FBgColor := AData.fValues.bgColor;
  end;
  if kml_textColor in FAvailParams then begin
    FTextColor := AData.fValues.textColor;
  end;
  if kml_tileSize in FAvailParams then begin
    FTileSize := AData.fValues.tileSize;
  end;
  if kml_scale_ in FAvailParams then begin
    FScale := AData.fValues.scale;
  end;
  if kml_fill in FAvailParams then begin
    FFill := AData.fValues.fill;
  end;
  if kml_width in FAvailParams then begin
    FWidth := AData.fValues.width;
  end;
end;

procedure TKmlStyleItem.WriteToKmlData(const AData: Pvsagps_KML_ParserData);
begin
  Assert(AData <> nil);

  if kml_color in FAvailParams then begin
    AData.fValues.color := FColor;
    Include(AData.fAvail_params, kml_color);
  end;
  if kml_bgColor in FAvailParams then begin
    AData.fValues.bgColor := FBgColor;
    Include(AData.fAvail_params, kml_bgColor);
  end;
  if kml_textColor in FAvailParams then begin
    AData.fValues.textColor := FTextColor;
    Include(AData.fAvail_params, kml_textColor);
  end;
  if kml_tileSize in FAvailParams then begin
    AData.fValues.tileSize := FTileSize;
    Include(AData.fAvail_params, kml_tileSize);
  end;
  if kml_scale_ in FAvailParams then begin
    AData.fValues.scale := FScale;
    Include(AData.fAvail_params, kml_scale_);
  end;
  if kml_fill in FAvailParams then begin
    AData.fValues.fill := FFill;
    Include(AData.fAvail_params, kml_fill);
  end;
  if kml_width in FAvailParams then begin
    AData.fValues.width := FWidth;
    Include(AData.fAvail_params, kml_width);
  end;
end;

{ TKmlStyleList }

constructor TKmlStyleList.Create;
begin
  inherited Create;
  FDict := TObjectDictionary<string, TKmlStyleItem>.Create([doOwnsValues]);
end;

destructor TKmlStyleList.Destroy;
begin
  FreeAndNil(FDict);
  inherited Destroy;
end;

procedure TKmlStyleList.Clear;
begin
  FDict.Clear;
end;

procedure TKmlStyleList.AddStyle(const AStyleId: string; const AStyle: TKmlStyleItem);
begin
  Assert(AStyleId <> '');
  Assert(AStyle <> nil);

  FDict.AddOrSetValue(AStyleId, AStyle);
end;

function TKmlStyleList.TryGetStyle(const AStyleId: string; out AStyle: TKmlStyleItem): Boolean;
begin
  Assert(AStyleId <> '');

  Result := FDict.TryGetValue(AStyleId, AStyle);
end;

{ TKmlStyleMap }

constructor TKmlStyleMap.Create;
begin
  inherited Create;
  FDict := TDictionary<string, TKmlStyleMapItem>.Create;
end;

destructor TKmlStyleMap.Destroy;
begin
  FreeAndNil(FDict);
  inherited Destroy;
end;

procedure TKmlStyleMap.Clear;
begin
  FDict.Clear;
end;

procedure TKmlStyleMap.AddPair(const AStyleId, AKey, AStyleUrl: string);
var
  VKey: string;
  VItem: TKmlStyleMapItem;
begin
  Assert(AStyleId <> '');
  Assert(AKey <> '');
  Assert(AStyleUrl <> '');

  if not FDict.TryGetValue(AStyleId, VItem) then begin
    VItem.Normal := '';
    VItem.Highlight := '';
  end;

  VKey := Trim(LowerCase(AKey));

  if VKey = 'normal' then begin
    VItem.Normal := AStyleUrl;
  end else
  if VKey = 'highlight' then begin
    VItem.Highlight := AStyleUrl;
  end else begin
    Assert(False, 'Unexpected kml:styleStateEnum value: "' + AKey + '"');
  end;

  // update value
  FDict.AddOrSetValue(AStyleId, VItem);
end;

function TKmlStyleMap.TryGetStyleUrl(const AStyleId: string; out AStyleUrl: string): Boolean;
var
  VItem: TKmlStyleMapItem;
begin
  Assert(AStyleId <> '');

  if FDict.TryGetValue(AStyleId, VItem) then begin
    AStyleUrl := VItem.Normal;
    if AStyleUrl = '' then begin
      AStyleUrl := VItem.Highlight;
    end;
  end else begin
    AStyleUrl := '';
  end;

  Result := AStyleUrl <> '';
end;

end.
