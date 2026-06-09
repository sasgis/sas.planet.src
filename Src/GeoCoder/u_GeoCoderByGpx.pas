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

unit u_GeoCoderByGpx;

interface

uses
  ActiveX,
  SysUtils,
  i_GeoCoder,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_VectorItemSubsetBuilder,
  i_VectorDataFactory,
  i_CoordToStringConverter,
  i_GeometryLonLatFactory,
  i_DoublePointsAggregator,
  i_GeometryLonLat,
  i_SystemTimeProvider,
  u_GeoCoderLocalBasic;

const
  CDistForDate = 0.0001;
  CDistForLine  = 0.005;

type
  EGeoCoderERR = class(Exception);
  EDirNotExist = class(EGeoCoderERR);

  TGeoCoderByGpx = class(TGeoCoderLocalBasic)
  private
    FPath: string;
    FCoordToStringConverter: ICoordToStringConverterChangeable;
    FSystemTimeInternal: ISystemTimeProviderInternal;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;

    procedure SearchInGpxFileByName(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AFileName: string;
      const ASearch: string;
      const AList: IInterfaceListSimple;
      const ACoordToStringConverter: ICoordToStringConverter
    );
    procedure SearchInGpxFileByDate(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AFileName: string;
      const ADateTime: string;
      const AList: IInterfaceListSimple;
      const ACoordToStringConverter: ICoordToStringConverter
    );
    function ParseDateTime(
      const ASearch:string;
      var AStrDateTime: string
    ): Boolean;
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: string;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
    constructor Create(
      const APath: string;
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AValueToStringConverter: ICoordToStringConverterChangeable;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
    );
  end;

implementation

uses
  StrUtils,
  XMLIntf,
  XMLDoc,
  RegExpr,
  t_GeoTypes,
  i_VectorDataItemSimple,
  u_AnsiStr,
  u_SystemTimeProvider,
  u_DoublePointsAggregator,
  u_InterfaceListSimple;

{ TGeoCoderByGpx }

constructor TGeoCoderByGpx.Create(
  const APath: string;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AValueToStringConverter: ICoordToStringConverterChangeable;
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
);
begin
  inherited Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory);
  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FVectorDataFactory := AVectorDataFactory;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FPath := APath;
  FCoordToStringConverter := AValueToStringConverter;
  FSystemTimeInternal := TSystemTimeProvider.Create;
end;

function ItemExist(
  const AValue: IVectorDataItem;
  const AList: IInterfaceListSimple;
  const ADist: Double
): Boolean;
var
  I: Integer;
  VPlacemark: IVectorDataItem;
begin
  Result := False;
  for I := 0 to AList.Count - 1 do begin
    VPlacemark := IVectorDataItem(AList.Items[I]);
    if Abs(VPlacemark.Geometry.GetGoToPoint.X - AValue.Geometry.GetGoToPoint.X) +
       Abs(VPlacemark.Geometry.GetGoToPoint.Y - AValue.Geometry.GetGoToPoint.Y) < ADist then begin
      Result := True;
      Break;
    end;
  end;
end;

function ISOToDateTime(const AISODateTime: string): TDateTime;
var
  VDate, VTime: TDateTime;
  VFormatSettings: TFormatSettings;
begin
  // ISO format: 2009-07-06T01:53:23Z
  VFormatSettings.DateSeparator := '-';
  VFormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  VFormatSettings.TimeSeparator := ':';
  VFormatSettings.ShortTimeFormat := 'hh:mm:ss';
  VDate := StrToDate(Copy(AISODateTime, 1, Pos('T', AISODateTime) - 1), VFormatSettings);
  VTime := StrToTime(Copy(AISODateTime, Pos('T', AISODateTime) + 1, 8), VFormatSettings);
  Result := Trunc(VDate) + Frac(VTime);
end;

function TGeoCoderByGpx.ParseDateTime(
  const ASearch:string;
  var AStrDateTime: string
):Boolean;
var
  VFormatSettings: TFormatSettingsA;
  VStrDateTime: AnsiString;
  VStrTime: AnsiString;
  VDate: TDateTime;
  VTime: TDateTime;
  VDateTime: TDateTime;
  VSearch: string;
  VSearchAnsi: AnsiString;
  VShortTimeSearch: Boolean;
  VRegExpr: TRegExpr;
begin
  VFormatSettings.DateSeparator := '-';
  VFormatSettings.ShortDateFormat := 'dd-mm-yyyy';
  VFormatSettings.TimeSeparator := ':';
  VFormatSettings.ShortTimeFormat := 'hh:mm:ss';
  VFormatSettings.DecimalSeparator := '.';

  VStrDateTime := '';

  VRegExpr  := TRegExpr.Create;
  try
    VSearch := '';
    VSearchAnsi := AnsiString(ASearch);
    VRegExpr.Expression := '([0-3]?[0-9]).([01]?[0-9]).([0-9]{4})';
    if VRegExpr.Exec(VSearchAnsi) then begin
      VStrDateTime :=
        VRegExpr.Match[1] + VFormatSettings.DateSeparator +
        VRegExpr.Match[2] + VFormatSettings.DateSeparator +
        VRegExpr.Match[3];
      VSearch := string(VRegExpr.Match[0]);
      VDate := StrToDateA(VStrDateTime, VFormatSettings);
    end else begin
      VRegExpr.Expression := '([0-9]{4}).([01]?[0-9]).([0-3]?[0-9])';
      if VRegExpr.Exec(VSearchAnsi) then begin
        VStrDateTime :=
          VRegExpr.Match[3] + VFormatSettings.DateSeparator +
          VRegExpr.Match[2] + VFormatSettings.DateSeparator +
          VRegExpr.Match[1];
        VSearch := string(VRegExpr.Match[0]);
        VDate := StrToDateA(VStrDateTime, VFormatSettings);
      end else
        VDate := 0;
    end;

    if VSearch = '' then begin
      VSearch := ASearch;
    end else begin
      VSearch := ReplaceStr(ASearch, VSearch, ''); // cut date rom parsed string and caontimue parse time value
    end;

    VShortTimeSearch := False;
    VSearchAnsi := AnsiString(VSearch);
    VRegExpr.Expression := '([0-2]?[0-9]).([0-5]?[0-9]).([0-5]?[0-9])'; // hh:mm:ss
    if VRegExpr.Exec(VSearchAnsi) then begin
      if VStrDateTime <> '' then VStrDateTime := VStrDateTime + ' ';
      VStrTime :=
        VRegExpr.Match[1] + VFormatSettings.TimeSeparator +
        VRegExpr.Match[2] + VFormatSettings.TimeSeparator +
        VRegExpr.Match[3];
      VStrDateTime := VStrDateTime + VStrTime;
      VTime := StrToTimeA(VStrTime, VFormatSettings);
    end else begin
      VRegExpr.Expression := '([0-2]?[0-9]).([0-5]?[0-9])'; // hh:mm
      if VRegExpr.Exec(VSearchAnsi) then begin
        if VStrDateTime <> '' then VStrDateTime := VStrDateTime + ' ';
        VStrTime :=
          VRegExpr.Match[1] + VFormatSettings.TimeSeparator +
          VRegExpr.Match[2];
        VStrDateTime := VStrDateTime + VStrTime;
        VTime := StrToTimeA(VStrTime, VFormatSettings);
        VShortTimeSearch := True;
      end else
        VTime := 0;
    end;
  finally
    FreeAndNil(VRegExpr);
  end;

  VDateTime := VDate + VTime;
  if VDateTime <> 0 then begin
    if VTime <> 0 then
      VDateTime := FSystemTimeInternal.LocalTimeToUTC(VDateTime);  // make UTC time to search in files
    VFormatSettings.ShortDateFormat := 'yyyy-mm-dd"T"hh:nn:ss"Z"';
    VStrDateTime := DateTimeToStrA(VDateTime, VFormatSettings);
    AStrDateTime := Copy(string(VStrDateTime),0,20); //cut last space from DateTimeToStr

    if VTime = 0 then
      AStrDateTime := Copy(AStrDateTime, 0, 11); // отрезаем время совсем

    if VShortTimeSearch then
      AStrDateTime := Copy(AStrDateTime, 0, Length(AStrDateTime) - 4); // отрезаем секунды
    if VDate = 0 then
      AStrDateTime := Copy(AStrDateTime, 11, 10); // отрезаем дату, оставляем только время
    Result := True;
  end else begin
    AStrDateTime := '';
    Result := False;
  end;
end;

procedure TGeoCoderByGpx.SearchInGpxFileByDate(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AFileName: string;
  const ADateTime: string;
  const AList: IInterfaceListSimple;
  const ACoordToStringConverter: ICoordToStringConverter
);
var
  I, J, K, L: Integer;
  VNode: IXMLNode;
  VPlacemarkNode: IXMLNode;
  VPlacemarkSubNode: IXMLNode;
  VTrkNode: IXMLNode;
  VTrksegSubNode: IXMLNode;
  VLatLonNode: IXMLNode;
  VAttribNode: IXMLNodeList;
  VPoint: TDoublePoint;
  VAddress: string;
  VDesc: string;
  VFullDesc: string;
  VPlace: IVectorDataItem;
  VXMLDocument: IXMLDocument;
  VSkip: Boolean;
  VStrDateTime: string;
  VStrDate: string;
  VTempELE: string;
  VTrkDesc: string;
  VFormatSettings: TFormatSettings;
begin
  VFormatSettings.DateSeparator := '-';
  VFormatSettings.ShortDateFormat := 'dd-mm-yyyy';
  VFormatSettings.TimeSeparator := ':';
  VFormatSettings.ShortTimeFormat := 'hh:mm:ss';
  VFormatSettings.DecimalSeparator := '.';

  VXMLDocument := TXMLDocument.Create(nil);
  try
    VXMLDocument.LoadFromFile(AFileName);
    VNode := VXMLDocument.DocumentElement;
    if (VNode <> nil) and (VNode.ChildNodes.Count > 0) then begin
      for I := 0 to VNode.ChildNodes.Count - 1 do begin
        if VNode.ChildNodes[I].NodeName = 'wpt' then begin
          VPlacemarkNode := VNode.ChildNodes[I];
          for J := 0 to VPlacemarkNode.GetAttributeNodes.getcount - 1 do begin
            VLatLonNode := VPlacemarkNode.GetAttributeNodes.Get(J);
            if VLatLonNode.GetNodeName = 'lon' then begin
              VPoint.X := StrToFloat(VLatLonNode.GetText, VFormatSettings);
            end;
            if VLatLonNode.GetNodeName = 'lat' then begin
              VPoint.Y := StrToFloat(VLatLonNode.GetText, VFormatSettings);
            end;
          end;

          VAddress := VPlacemarkNode.ChildNodes.FindNode('name').Text;
          VDesc := '';
          if VPlacemarkNode.ChildNodes.FindNode('desc') <> nil then begin
            VDesc := VPlacemarkNode.ChildNodes.FindNode('desc').Text;
          end;
          if VPlacemarkNode.ChildNodes.FindNode('ele') <> nil then begin
            if VDesc <> '' then VDesc := VDesc + sLineBreak;
            VDesc := VDesc + 'Elevation ' + VPlacemarkNode.ChildNodes.FindNode('ele'). Text;
          end;
          VSkip := True;
          VDesc := VDesc + sLineBreak + '[ ' + ACoordToStringConverter.LonLatConvert(VPoint) + ' ]';
          VDesc := VDesc + sLineBreak + ExtractFileName(AFileName);
          VFullDesc := VAddress + '<br>' + VDesc;

          if VPlacemarkNode.ChildNodes.FindNode('url') <> nil then begin
            VFullDesc := VFullDesc + '<br><a href=' + VPlacemarkNode.ChildNodes.FindNode('url').Text + '>' + VPlacemarkNode.ChildNodes.FindNode('url').Text + '</a>';
          end;
          if VPlacemarkNode.ChildNodes.FindNode('time') <> nil then begin
            VStrDateTime := VPlacemarkNode.ChildNodes.FindNode('time').Text; // '2015-12-02T08:54:43';
            if (ADateTime = VStrDateTime) or (Pos(ADateTime, VStrDateTime) <> 0) then
              VSkip := False;
          end;
          for J := 0 to VPlacemarkNode.ChildNodes.Count - 1 do begin
            VPlacemarkSubNode := VPlacemarkNode.ChildNodes[J];
            if VPlacemarkSubNode.NodeName = 'groundspeak:cache' then begin
              if VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:short_description') <> nil then begin
                VFullDesc := VFullDesc + '<br>' + VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:short_description').Text;
              end;
              if VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:difficulty') <> nil then begin
                VFullDesc := VFullDesc + '<br>Difficulty:' + VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:difficulty').Text;
              end;
              if VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:long_description') <> nil then begin
                VFullDesc := VFullDesc + VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:long_description').Text;
              end;
            end;
          end;

          if not VSkip then begin
            VPlace := PlacemarkFactory.Build(VPoint, VAddress, VDesc, VFullDesc, 4);
            VSkip := ItemExist(VPlace, AList, CDistForDate);
            if not VSkip then begin
              AList.Add(VPlace);
            end;
          end;
        end else
        if VNode.ChildNodes[I].NodeName = 'trk' then begin
          VTrkNode := VNode.ChildNodes[I];
          for L := 0 to VTrkNode.ChildNodes.Count - 1 do begin

            VPlacemarkNode := VTrkNode.ChildNodes[L];

            if VPlacemarkNode.GetNodeName = 'name' then begin
              VTrkDesc := VPlacemarkNode.Text;
            end else
            if VPlacemarkNode.GetNodeName = 'desc' then begin
              if VTrkDesc <> '' then VTrkDesc := VTrkDesc + sLineBreak;
              VTrkDesc := VTrkDesc +VPlacemarkNode.Text;
            end else
            if VPlacemarkNode.GetNodeName = 'ele' then begin
              if VTrkDesc <> '' then VTrkDesc := VTrkDesc + sLineBreak;
              VTrkDesc := VTrkDesc + 'Elevation ' + VPlacemarkNode.Text;
            end else
            if VPlacemarkNode.GetNodeName = 'trkseg' then begin
              if VPlacemarkNode.ChildNodes.Count >0 then begin
                for J := 0 to VPlacemarkNode.ChildNodes.Count - 1 do begin
                  if VPlacemarkNode.ChildNodes[J].NodeName = 'trkpt' then begin

                    VDesc := '';
                    VFullDesc := '';
                    VTrksegSubNode := VPlacemarkNode.ChildNodes[J];

                    if VTrksegSubNode.ChildNodes.FindNode('ele') <> nil then begin
                      VTempELE := 'Elevation ' + VTrksegSubNode.ChildNodes.FindNode('ele').Text;
                    end;

                    if VTrksegSubNode.ChildNodes.FindNode('time') <> nil then begin
                      VStrDateTime := VTrksegSubNode.ChildNodes.FindNode('time').Text;
                      VStrDate := DateTimeToStr(FSystemTimeInternal.UTCToLocalTime(ISOToDateTime(VStrDateTime)));
                      VAttribNode := VTrksegSubNode.GetAttributeNodes;

                      if (ADateTime = VStrDateTime) or (Pos(ADateTime, VStrDateTime) <> 0) then begin
                        for K := 0 to VAttribNode.getcount - 1 do begin
                          VLatLonNode := VAttribNode.Get(K);
                          if VLatLonNode.GetNodeName = 'lon' then begin
                            VPoint.X := StrToFloat(VLatLonNode.GetText, VFormatSettings);
                          end;
                          if VLatLonNode.GetNodeName = 'lat' then begin
                            VPoint.Y := StrToFloat(VLatLonNode.GetText, VFormatSettings);
                          end;
                        end;
                        VAddress := VTrkDesc + ' (' + VStrDate + ')';
                        VDesc := VTempELE;
                        VDesc := VDesc + sLineBreak + 'DateTime: ' + VStrDate;
                        VDesc := VDesc + sLineBreak + ExtractFileName(AFileName);
                        VFullDesc := VAddress + '<br>' + VDesc  + sLineBreak + '[ ' + ACoordToStringConverter.LonLatConvert(VPoint) + ' ]';
                        VPlace := PlacemarkFactory.Build(VPoint, VAddress , VDesc, VFullDesc, 4);
                        VSkip := ItemExist(Vplace, AList, CDistForDate);
                        if not VSkip then begin
                          AList.Add(VPlace);
                        end;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  except
    //
  end;
end;

procedure TGeoCoderByGpx.SearchInGpxFileByName(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AFileName: string;
  const ASearch: string;
  const AList: IInterfaceListSimple;
  const ACoordToStringConverter: ICoordToStringConverter
);
var
  I, J, K, L: Integer;
  VNode: IXMLNode;
  VTrkNode: IXMLNode;
  VPlacemarkNode: IXMLNode;
  VPlacemarkSubNode: IXMLNode;
  VTrksegSubNode: IXMLNode;
  VLatLonNode: IXMLNode;
  VAttribNode: IXMLNodeList;
  VPoint: TDoublePoint;
  VAddress: string;
  VDesc: string;
  VFullDesc: string;
  VTrkDesc: string;
  VPlace: IVectorDataItem;
  VXMLDocument: IXMLDocument;
  VIsFound: Boolean;
  VStrDateTime: string;
  VStrDate: string;
  VSearch: string;
  VFormatSettings: TFormatSettings;
  VPointsAggregator: IDoublePointsAggregator;
  VBuilder: IGeometryLonLatLineBuilder;
  VPath: IGeometryLonLat;
  VItem: IVectorDataItem;
begin
  VFormatSettings.DateSeparator := '-';
  VFormatSettings.ShortDateFormat := 'dd-mm-yyyy';
  VFormatSettings.TimeSeparator := ':';
  VFormatSettings.ShortTimeFormat := 'hh:mm:ss';
  VFormatSettings.DecimalSeparator := '.';
  VPointsAggregator := TDoublePointsAggregator.Create;
  VBuilder := FVectorGeometryLonLatFactory.MakeLineBuilder;

  VSearch := AnsiUpperCase(ASearch);

  VXMLDocument := TXMLDocument.Create(nil);
  try
    VXMLDocument.LoadFromFile(AFileName);
    VNode := VXMLDocument.DocumentElement;
    if (VNode <> nil) and (VNode.ChildNodes.Count > 0) then begin
      for I := 0 to VNode.ChildNodes.Count - 1 do begin

        if VNode.ChildNodes[I].NodeName = 'wpt' then begin
          VPlacemarkNode := VNode.ChildNodes[I];
          for J := 0 to VPlacemarkNode.GetAttributeNodes.getcount - 1 do begin
            VLatLonNode := VPlacemarkNode.GetAttributeNodes.Get(J);
            if VLatLonNode.GetNodeName = 'lon' then begin
              VPoint.X := StrToFloat(VLatLonNode.GetText, VFormatSettings);
            end;
            if VLatLonNode.GetNodeName = 'lat' then begin
              VPoint.Y := StrToFloat(VLatLonNode.GetText, VFormatSettings);
            end;
          end;

          VAddress := VPlacemarkNode.ChildNodes.FindNode('name').Text;
          VIsFound := (Pos(VSearch, AnsiUpperCase(VAddress)) > 0); // search by Name

          VDesc := '';
          if VPlacemarkNode.ChildNodes.FindNode('desc') <> nil then begin
            VDesc := VPlacemarkNode.ChildNodes.FindNode('desc').Text;
          end;
          if VPlacemarkNode.ChildNodes.FindNode('ele') <> nil then begin
            if VDesc <> '' then VDesc := VDesc + sLineBreak;
            VDesc := VDesc + 'Elevation ' + VPlacemarkNode.ChildNodes.FindNode('ele'). Text;
          end;
          if VPlacemarkNode.ChildNodes.FindNode('time') <> nil then begin
            VStrDateTime := VPlacemarkNode.ChildNodes.FindNode('time').Text; // '2015-12-02T08:54:43';
            if VDesc <> '' then VDesc := VDesc + sLineBreak;
            VDesc := VDesc + 'DateTime: ' + DateTimeToStr(FSystemTimeInternal.UTCToLocalTime(ISOToDateTime(VStrDateTime)));;
          end;

          VIsFound := VIsFound or (Pos(VSearch, AnsiUpperCase(VDesc)) > 0); // search by Description

          if VDesc <> '' then VDesc := VDesc + sLineBreak;
          VDesc := VDesc + '[ ' + ACoordToStringConverter.LonLatConvert(VPoint) + ' ]';
          VDesc := VDesc + sLineBreak + ExtractFileName(AFileName);

          VFullDesc := '';
          if VPlacemarkNode.ChildNodes.FindNode('url') <> nil then begin
            if VFullDesc <> '' then VFullDesc := VFullDesc + '<br>';
            VFullDesc := VFullDesc + '<a href=' + VPlacemarkNode.ChildNodes.FindNode('url').Text + '>' + VPlacemarkNode.ChildNodes.FindNode('url').Text + '</a>';
          end;
          for J := 0 to VPlacemarkNode.ChildNodes.Count - 1 do begin
            VPlacemarkSubNode := VPlacemarkNode.ChildNodes[J];
            if VPlacemarkSubNode.NodeName = 'groundspeak:cache' then begin
              if VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:short_description') <> nil then begin
                if VFullDesc <> '' then VFullDesc := VFullDesc + '<br>';
                VFullDesc := VFullDesc + VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:short_description').Text;
              end;
              if VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:difficulty') <> nil then begin
                if VFullDesc <> '' then VFullDesc := VFullDesc + '<br>';
                VFullDesc := VFullDesc + 'Difficulty:' + VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:difficulty').Text;
              end;
              if VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:long_description') <> nil then begin
                if VFullDesc <> '' then VFullDesc := VFullDesc + '<br>';
                VFullDesc := VFullDesc + VPlacemarkSubNode.ChildNodes.FindNode('groundspeak:long_description').Text;
              end;
            end;
          end;

          VIsFound := VIsFound or (Pos(VSearch, AnsiUpperCase(VFullDesc)) > 0); // search by Full Description

          if VIsFound then begin
            if VFullDesc <> '' then VFullDesc := '<br>' + VFullDesc;
            VFullDesc := VAddress + '<br>' + VDesc + VFullDesc;
            VFullDesc := StringReplace(VFullDesc, sLineBreak, '<br>', [rfReplaceAll]);

            VPlace := PlacemarkFactory.Build(VPoint, VAddress, VDesc, VFullDesc, 4);
            if not ItemExist(VPlace, AList, CDistForLine) then begin // check for duplicates
              AList.Add(VPlace);
            end;
          end;
        end else
        if VNode.ChildNodes[I].NodeName = 'trk' then begin
          VAddress := '';
          VDesc := '';
          VFullDesc := '';
          VTrkNode := VNode.ChildNodes[I];
          for L := 0 to VTrkNode.ChildNodes.Count - 1 do begin
            VPlacemarkNode := VTrkNode.ChildNodes[L];
            VDesc := VTrkDesc + VDesc;

            if VPlacemarkNode.GetNodeName = 'name' then begin
              VAddress := VPlacemarkNode.Text;
            end else
            if VPlacemarkNode.GetNodeName = 'desc' then begin
              VTrkDesc := VPlacemarkNode.Text;
            end else
            if VPlacemarkNode.GetNodeName = 'trkseg'  then begin

              VIsFound :=
                (Pos(VSearch, AnsiUpperCase(VAddress)) > 0) or // search by Track Name
                (Pos(VSearch, AnsiUpperCase(VTrkDesc)) > 0);   // search by Track Description

              if VIsFound then begin
                if VPlacemarkNode.ChildNodes.Count >0 then begin
                  for J := 0 to VPlacemarkNode.ChildNodes.Count - 1 do begin
                    if VPlacemarkNode.ChildNodes[J].NodeName = 'trkpt' then begin

                      VTrksegSubNode := VPlacemarkNode.ChildNodes[J];
                      VAttribNode := VTrksegSubNode.GetAttributeNodes;

                      for K := 0 to VAttribNode.getcount - 1 do begin
                        VLatLonNode := VAttribNode.Get(K);
                        if VLatLonNode.GetNodeName = 'lon' then begin
                          VPoint.X := StrToFloat(VLatLonNode.GetText, VFormatSettings);
                        end else
                        if VLatLonNode.GetNodeName = 'lat' then begin
                          VPoint.Y := StrToFloat(VLatLonNode.GetText, VFormatSettings);
                        end;
                      end;

                      if VStrDate = '' then begin
                        if VTrksegSubNode.ChildNodes.FindNode('time') <> nil then begin
                          VStrDateTime := VTrksegSubNode.ChildNodes.FindNode('time').Text;
                          VStrDate := DateTimeToStr(FSystemTimeInternal.UTCToLocalTime(ISOToDateTime(VStrDateTime)));
                        end;
                      end;
                      VPointsAggregator.Add(VPoint);
                    end;
                  end;
                end;
              end;
              if VStrDate <> '' then begin
                if VDesc <> '' then  VDesc := VDesc + sLineBreak;
                VDesc := VDesc + 'DateTime: ' + VStrDate;
              end;
              VDesc := VDesc + sLineBreak + '[ ' + ACoordToStringConverter.LonLatConvert(VPoint) + ' ]';
              VDesc := VDesc + sLineBreak + ExtractFileName(AFileName);
              VDesc := VDesc + sLineBreak + 'track: true';
              VDesc := VDesc + sLineBreak + IntToStr(VPointsAggregator.Count) + ' points';

              if VPointsAggregator.Count > 0 then begin
                VBuilder.AddLine(VPointsAggregator.MakeStaticAndClear);
                VPath := VBuilder.MakeStaticAndClear;
                if Assigned(VPath) then begin
                  VItem :=
                    FVectorDataFactory.BuildItem(
                      FVectorDataItemMainInfoFactory.BuildMainInfo(nil, VAddress, VDesc),
                      nil,
                      VPath
                  );
                  AList.Add(VItem);
                  VDesc := '';
                  VFullDesc := '';
                  VStrDate := '';
                end;
              end;
            end;
          end;
        end else
        if VNode.ChildNodes[I].NodeName = 'rte' then begin
          // TODO: search by name in rte
          //  <rte>
          //    <name> Т о н н е л ь н а я  Р а е в с к а я  М. У т р и ш  А б р а у  С е в. О з е р е е в к а
          //    <link href="http://www.gpsies.com/map.do?fileId=dbdmweskjstcexnx">
          //      <type>trackOnWeb</type>
          //    </link>
          //    <link href="http://www.gpsies.com/charts/db/map/dbdmweskjstcexnx_map.png">
          //      <type>elevationChartUrlMap</type>
          //    </link>
          //    <link href="http://www.gpsies.com/charts/db/mapThumb/dbdmweskjstcexnx_mapThumb.png">
          //      <type>elevationChartUrlMapThumb</type>
          //    </link>
          //    <link href="http://www.gpsies.com/charts/db/tab/dbdmweskjstcexnx_tab.png">
          //      <type>elevationChartUrlTab</type>
          //    </link>
          //    <rtept lat="44.84089920" lon="37.65958780">
          //      <ele>227.00000</ele>
          //      <time>2010-01-01T00:00:00Z</time>
          //    </rtept>
          //    <rtept lat="44.84102090" lon="37.64585490">
          //      <ele>275.00000</ele>
          //      <time>2010-01-01T00:06:29Z</time>
          //    </rtept>
        end;
        VPointsAggregator.Clear;
        VPath := nil;
        VStrDate := '';
      end;
    end;
  except
    //
  end;
end;

function TGeoCoderByGpx.DoSearch(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  VPath: string;
  VSearch: string;
  VSearchRec: TSearchRec;
  VValueConverter: ICoordToStringConverter;
  VTxtGpxDateTime: string;
  VSearchByDate: Boolean;
begin
  Result := TInterfaceListSimple.Create;;

  CoInitialize(nil);
  try
    VSearch := ASearch;
    while PosEx('  ', VSearch) > 0 do begin
      VSearch := ReplaceStr(VSearch, '  ', ' ');
    end;

    VSearchByDate := ParseDateTime(VSearch, VTxtGpxDateTime);
    VValueConverter := FCoordToStringConverter.GetStatic;

    if FindFirst(FPath + '*.gpx', faAnyFile, VSearchRec) = 0 then
    try
      repeat
        if (VSearchRec.Attr and faDirectory) = faDirectory then begin
          Continue;
        end;

        VPath := FPath + VSearchRec.Name;

        if VSearchByDate then begin
          SearchInGpxFileByDate(ACancelNotifier, AOperationID, VPath, VTxtGpxDateTime, Result, VValueConverter);
        end else begin
          SearchInGpxFileByName(ACancelNotifier, AOperationID, VPath, VSearch, Result, VValueConverter);
        end;

        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Exit;
        end;
      until FindNext(VSearchRec) <> 0;
    finally
      SysUtils.FindClose(VSearchRec);
    end;
  finally
    CoUninitialize;
  end;
end;

end.
