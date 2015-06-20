{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_MpSimpleParser;

interface

uses
  i_BinaryData,
  i_VectorDataLoader,
  i_GeometryLonLatFactory,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_VectorItemSubsetBuilder,
  i_DoublePointsAggregator,
  u_BaseInterfacedObject;

type
  TMpSimpleParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    procedure ParseCoordinates(
      const AData: string;
      const APointsAggregator: IDoublePointsAggregator
    );
  private
    function Load(
      const AData: IBinaryData;
      const AIdData: Pointer;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
    ): IVectorItemSubset;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  Classes,
  t_GeoTypes,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  u_StreamReadOnlyByBinaryData,
  u_DoublePointsAggregator,
  u_GeoFunc;

const
  CPoligonHeader = '[POLYGON]';
  CDataHeader = 'Data0=';

{ TMpSimpleParser }

constructor TMpSimpleParser.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory
);
begin
  inherited Create;
  FVectorItemSubsetBuilderFactory := AVectorItemSubsetBuilderFactory;
  FVectorDataFactory := AVectorDataFactory;
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
end;

procedure TMpSimpleParser.ParseCoordinates(
  const AData: string;
  const APointsAggregator: IDoublePointsAggregator
);
var
  VCoordList: TStringList;
  VString: string;
  i: Integer;
  VPos: Integer;
  VXStr: string;
  VYStr: string;
  VPoint: TDoublePoint;
  VFormatSettings: TFormatSettings;
begin
  VCoordList := TStringList.Create;
  try
    VFormatSettings.DecimalSeparator := '.';
    VCoordList.Delimiter := '(';
    VCoordList.DelimitedText := AData;
    for i := 0 to VCoordList.Count - 1 do begin
      VString := VCoordList[i];
      if VString <> '' then begin
        VPoint := CEmptyDoublePoint;
        VPos := Pos(',', VString);
        if VPos > 0 then begin
          VYStr := LeftStr(VString, VPos - 1);
          VXStr := MidStr(VString, VPos + 1, Length(VString));
          VPos := Pos(')', VXStr);
          if VPos > 0 then begin
            VXStr := LeftStr(VXStr, VPos - 1);
          end;
          VPoint.X := StrToFloatDef(VXStr, VPoint.X, VFormatSettings);
          VPoint.Y := StrToFloatDef(VYStr, VPoint.Y, VFormatSettings);
        end;
        if not PointIsEmpty(VPoint) then begin
          APointsAggregator.Add(VPoint);
        end;
      end;
    end;
  finally
    VCoordList.Free;
  end;
end;

function TMpSimpleParser.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
): IVectorItemSubset;
var
  VFileStrings: TStringList;
  VDataStream: TStream;
  VPolygon: IGeometryLonLatPolygon;
  VItem: IVectorDataItem;
  VList: IVectorItemSubsetBuilder;
  VString: string;
  VPoligonLine: Integer;
  i: integer;
  VPointsAggregator: IDoublePointsAggregator;
  VDataLine: Integer;
begin
  Result := nil;
  VPolygon := nil;
  VPointsAggregator := TDoublePointsAggregator.Create;
  if AData <> nil then begin
    VDataStream := TStreamReadOnlyByBinaryData.Create(AData);
    try
      VDataStream.Position := 0;
      VFileStrings := TStringList.Create;
      try
        VFileStrings.LoadFromStream(VDataStream);
        VPoligonLine := -1;
        for i := 0 to VFileStrings.Count - 1 do begin
          VString := VFileStrings[i];
          if LeftStr(VString, Length(CPoligonHeader)) = CPoligonHeader then begin
            VPoligonLine := i;
            Break;
          end;
        end;
        if VPoligonLine >= 0 then begin
          VDataLine := -1;
          for i := VPoligonLine + 1 to VFileStrings.Count - 1 do begin
            VString := VFileStrings[i];
            if LeftStr(VString, Length(CDataHeader)) = CDataHeader then begin
              VDataLine := i;
              Break;
            end;
          end;
          if VDataLine >= 0 then begin
            VString := MidStr(VString, Length(CDataHeader) + 1, Length(VString));
            if VString <> '' then begin
              ParseCoordinates(VString, VPointsAggregator);
            end;
          end;
        end;
      finally
        VFileStrings.Free;
      end;
    finally
      VDataStream.Free;
    end;
  end;
  if VPointsAggregator.Count > 2 then begin
    VPolygon := FVectorGeometryLonLatFactory.CreateLonLatPolygon(VPointsAggregator.Points, VPointsAggregator.Count);
  end;
  if VPolygon <> nil then begin
    VItem :=
      FVectorDataFactory.BuildItem(
        AVectorDataItemMainInfoFactory.BuildMainInfo(AIdData, '', ''),
        nil,
        VPolygon
      );
    VList := FVectorItemSubsetBuilderFactory.Build;
    VList.Add(VItem);
    Result := VList.MakeStaticAndClear;
  end;
end;

end.
