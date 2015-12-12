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

unit u_GeoCoderByGpx;

interface

uses
  SysUtils,
  i_GeoCoder,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_VectorItemSubsetBuilder,
  i_VectorDataFactory,
  i_ValueToStringConverter,
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
    FValueToStringConverter: IValueToStringConverterChangeable;
    FSystemTimeInternal: ISystemTimeProviderInternal;
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;

    procedure SearchInGpxFileByName(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AFile: String;
      const ASearch: string;
      const AList: IInterfaceListSimple;
      const AValueConverter: IValueToStringConverter
    );
    procedure SearchInGpxFileByDate(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AFile: string;
      const ADateTime: string;
      const AList: IInterfaceListSimple;
      const AValueConverter: IValueToStringConverter
    );
    function ParseDateTime(
      const ASearch:string;
      var AstrDateTime: string
    ): boolean;
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
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AVectorDataFactory: IVectorDataFactory;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
    );
  end;

implementation

uses
  StrUtils,
  ALString,
  RegExpr,
  XMLIntf,
  XMLDoc,
  Windows,
  t_GeoTypes,
  i_VectorDataItemSimple,
  u_SystemTimeProvider,
  u_DoublePointsAggregator,
  u_InterfaceListSimple;

{ TGeoCoderByGpx }
constructor TGeoCoderByGpx.Create(
  const APath: string;
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AValueToStringConverter: IValueToStringConverterChangeable;
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
  if not DirectoryExists(FPath) then begin
    raise EDirNotExist.CreateFmt('not found %s! skip GeoCoderByGpx', [FPath]);
  end;
  FValueToStringConverter := AValueToStringConverter;
  FSystemTimeInternal := TSystemTimeProvider.Create;
end;


function ItemExist(
  const AValue: IVectorDataItem;
  const AList: IInterfaceListSimple;
  const Adist: Real
): Boolean;
var
  I: Integer;
  VPlacemark: IVectorDataItem;
begin
  Result := false;
  for I := 0 to AList.Count - 1 do begin
    VPlacemark := IVectorDataItem(AList.Items[I]);
    if abs(VPlacemark.Geometry.GetGoToPoint.X - AValue.Geometry.GetGoToPoint.X) +
    abs(VPlacemark.Geometry.GetGoToPoint.Y - AValue.Geometry.GetGoToPoint.Y) < Adist then begin
      Result := true;
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

function TGeoCoderByGpx.ParseDateTime(const ASearch:string; var AstrDateTime: string):Boolean;
var
  VFormatSettings: TFormatSettings;
  VStrDateTime: string;
  VStrTime: string;
  VDate: TDateTime;
  VTime: TDateTime;
  VDateTime: TDateTime;
  VSearch: string;
  VShortTimeSearch: boolean;
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
    VRegExpr.Expression := '([0-3]?[0-9]).([01]?[0-9]).([0-9]{4})';
    if VRegExpr.Exec(ASearch) then begin
      VStrDateTime :=
        VRegExpr.Match[1] + VFormatSettings.DateSeparator +
        VRegExpr.Match[2] + VFormatSettings.DateSeparator +
        VRegExpr.Match[3];
      VSearch := VRegExpr.Match[0];
      VDate := StrToDate(VStrDateTime, VFormatSettings);
    end else begin
      VRegExpr.Expression := '([0-9]{4}).([01]?[0-9]).([0-3]?[0-9])';
      if VRegExpr.Exec(ASearch) then begin
        VStrDateTime :=
          VRegExpr.Match[3] + VFormatSettings.DateSeparator +
          VRegExpr.Match[2] + VFormatSettings.DateSeparator +
          VRegExpr.Match[1];
        VSearch := VRegExpr.Match[0];
        VDate := StrToDate(VStrDateTime, VFormatSettings);
      end else
        VDate := 0;
    end;

    VSearch := ReplaceStr(ASearch, VSearch, ''); // cut date rom parsed string and caontimue parse time value
    VShortTimeSearch := false;

    VRegExpr.Expression := '([0-2]?[0-9]).([0-5]?[0-9]).([0-5]?[0-9])'; // hh:mm:ss
    if VRegExpr.Exec(VSearch) then begin
      if VStrDateTime <> '' then VStrDateTime := VStrDateTime + ' ';
      VStrTime :=
        VRegExpr.Match[1] + VFormatSettings.TimeSeparator +
        VRegExpr.Match[2] + VFormatSettings.TimeSeparator +
        VRegExpr.Match[3];
      VStrDateTime := VStrDateTime + VStrTime;
      VTime := StrToTime(VStrTime, VFormatSettings);
    end else begin
      VRegExpr.Expression := '([0-2]?[0-9]).([0-5]?[0-9])'; // hh:mm
      if VRegExpr.Exec(VSearch) then begin
        if VStrDateTime <> '' then VStrDateTime := VStrDateTime + ' ';
        VStrTime :=
          VRegExpr.Match[1] + VFormatSettings.TimeSeparator +
          VRegExpr.Match[2];
        VStrDateTime := VStrDateTime + VStrTime;
        VTime := StrToTime(VStrTime, VFormatSettings);
        VShortTimeSearch := true;
      end else
        VTime := 0;
    end;

  finally
  end;

  VDateTime := VDate + VTime;
  if VDateTime <> 0 then begin
    if VTime <> 0 then
      VDateTime := FSystemTimeInternal.LocalTimeToUTC(VDateTime);  // make UTC time to search in files
    VFormatSettings.ShortDateFormat := 'yyyy-mm-dd"T"hh:nn:ss"Z"';
    AstrDateTime := copy(DateTimeToStr(VDateTime, VFormatSettings),0,20); //cut last space from DateTimeToStr

    if VTime = 0 then
      AstrDateTime := copy(AstrDateTime, 0, 11); // отрезаем время совсем

    if VShortTimeSearch then
      AstrDateTime := Copy(AstrDateTime, 0, Length(AstrDateTime) - 4); // отрезаем секунды
    if VDate = 0 then
      AstrDateTime := Copy(AstrDateTime, 11, 10); // отрезаем дату, оставляем только время
    Result := True;
  end else begin
    AstrDateTime := '';
    Result := False;
  end;
end;

procedure TGeoCoderByGpx.SearchInGpxFileByDate(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AFile: string;
  const ADateTime: string;
  const AList: IInterfaceListSimple;
  const AValueConverter: IValueToStringConverter
);
var
  VNode: IXMLNode;
  VPlacemarkNode: IXMLNode;
  VPlacemarkSubNode: IXMLNode;
  VTrksegSubNode: IXMLNode;
  VLatLonNode: IXMLNode;
  VAttribNode: IXMLNodeList;
  VPoint: TDoublePoint;
  VAddress: String;
  VDesc: String;
  VFullDesc: String;
  VPlace: IVectorDataItem;

  VXMLDocument: IXMLDocument;
  I, J, K: Integer;
  Vskip: Boolean;
  VStrDateTime: string;
  VStrDate: string;
  VTempELE: string;
  VtrkDesc: string;
  VFormatSettings: TFormatSettings;
begin
  VFormatSettings.DateSeparator := '-';
  VFormatSettings.ShortDateFormat := 'dd-mm-yyyy';
  VFormatSettings.TimeSeparator := ':';
  VFormatSettings.ShortTimeFormat := 'hh:mm:ss';
  VFormatSettings.DecimalSeparator := '.';
  //TODO: Fix for unicode file
  VXMLDocument := TXMLDocument.Create(nil);

  try
    VXMLDocument.LoadFromFile(AFile);
    VNode := VXMLDocument.DocumentElement;
    if (VNode <> nil) and (VNode.ChildNodes.Count > 0) then begin
      for I := 0 to VNode.ChildNodes.Count - 1 do begin
        if VNode.ChildNodes[I].NodeName = 'wpt' then begin
          VPlacemarkNode := VNode.ChildNodes[I];
          for J := 0 to VPlacemarkNode.GetAttributeNodes.getcount - 1 do begin
            VLatLonNode := VPlacemarkNode.GetAttributeNodes.get(J);
            if VLatLonNode.GetNodeName = 'lon' then begin
              VPoint.X := StrToFloat(VLatLonNode.gettext, VFormatSettings);
            end;
            if VLatLonNode.GetNodeName = 'lat' then begin
              VPoint.Y := StrToFloat(VLatLonNode.gettext, VFormatSettings);
            end;
          end;

          VAddress := VPlacemarkNode.ChildNodes.FindNode('name').Text;
          VDesc := '';
          if VPlacemarkNode.ChildNodes.FindNode('desc') <> nil then begin
            VDesc := VPlacemarkNode.ChildNodes.FindNode('desc').Text;
          end;
          if VPlacemarkNode.ChildNodes.FindNode('ele') <> nil then begin
            if VDesc <> '' then VDesc := VDesc + #$D#$A;
            VDesc := VDesc + 'Elevation ' + VPlacemarkNode.ChildNodes.FindNode('ele'). Text;
          end;
          Vskip := True;

          VDesc := VDesc + #$D#$A + '[ ' + AValueConverter.LonLatConvert(VPoint) + ' ]';
          VDesc := VDesc + #$D#$A + AFile;
          VFullDesc := VAddress + '<br>' + VDesc;

          if VPlacemarkNode.ChildNodes.FindNode('url') <> nil then begin
            VFullDesc := VFullDesc + '<br><a href=' + VPlacemarkNode.ChildNodes.FindNode('url').Text + '>' + VPlacemarkNode.ChildNodes.FindNode('url').Text + '</a>';
          end;
          if VPlacemarkNode.ChildNodes.FindNode('time') <> nil then begin
            VStrDateTime := VPlacemarkNode.ChildNodes.FindNode('time').Text; // '2015-12-02T08:54:43';
            if (ADateTime = VStrDateTime) or (Pos(ADateTime, VStrDateTime) <> 0) then
              Vskip := False;
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

          if not Vskip then begin
            VPlace := PlacemarkFactory.Build(VPoint, VAddress, VDesc, VFullDesc, 4);
            Vskip := ItemExist(Vplace, AList, CDistForDate );
            if not Vskip then begin
              AList.Add(VPlace);
            end;
          end;
        end;

        if VNode.ChildNodes[I].NodeName = 'trk' then begin
          VPlacemarkNode := VNode.ChildNodes[I];
          if VPlacemarkNode.ChildNodes.FindNode('name') <> nil then begin
            VtrkDesc := VPlacemarkNode.ChildNodes.FindNode('name').Text;
          end;
          if VPlacemarkNode.ChildNodes.FindNode('desc') <> nil then begin
            if VtrkDesc <> '' then VtrkDesc := VtrkDesc + #$D#$A;
            VtrkDesc := VtrkDesc +VPlacemarkNode.ChildNodes.FindNode('desc').Text;
          end;
          if VPlacemarkNode.ChildNodes.FindNode('ele') <> nil then begin
            if VtrkDesc <> '' then VtrkDesc := VtrkDesc + #$D#$A;
            VtrkDesc := VtrkDesc + 'Elevation ' + VPlacemarkNode.ChildNodes.FindNode('ele').Text;
          end;

          if VPlacemarkNode.ChildNodes.FindNode('trkseg') <> nil then begin
            VPlacemarkNode := VPlacemarkNode.ChildNodes.FindNode('trkseg');

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
                        VLatLonNode := VAttribNode.get(K);
                        if VLatLonNode.GetNodeName = 'lon' then begin
                          VPoint.X := StrToFloat(VLatLonNode.gettext, VFormatSettings);
                        end;
                        if VLatLonNode.GetNodeName = 'lat' then begin
                          VPoint.Y := StrToFloat(VLatLonNode.gettext, VFormatSettings);
                        end;
                      end;
                      VAddress := VtrkDesc + ' (' + VStrDate + ')';
                      VDesc := VTempELE;
                      VDesc := VDesc + #$D#$A + 'DateTime: ' + VStrDate;
                      VDesc := VDesc + #$D#$A + AFile;
                      VFullDesc := VAddress + '<br>' + VDesc  + #$D#$A + '[ ' + AValueConverter.LonLatConvert(VPoint) + ' ]';
                      VPlace := PlacemarkFactory.Build(VPoint, VAddress , VDesc, VFullDesc, 4);
                      Vskip := ItemExist(Vplace, AList, CDistForDate);
                      if not Vskip then begin
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
  except
  end;
end;

procedure TGeoCoderByGpx.SearchInGpxFileByName(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AFile: String;
  const ASearch: string;
  const AList: IInterfaceListSimple;
  const AValueConverter: IValueToStringConverter
);
var
  VNode: IXMLNode;
  VPlacemarkNode: IXMLNode;
  VPlacemarkSubNode: IXMLNode;
  VTrksegSubNode: IXMLNode;
  VLatLonNode: IXMLNode;
  VAttribNode: IXMLNodeList;
  VPoint: TDoublePoint;
  VAddress: String;
  VDesc: String;
  VFullDesc: String;
  VPlace: IVectorDataItem;
  VXMLDocument: IXMLDocument;
  I, J, K: Integer;
  Vskip: Boolean;
  VStrDateTime: string;
  VStrDate: string;
  VSearch: AnsiString;
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

  //TODO: Fix for unicode file
  VSearch := AnsiString(AnsiUpperCase(ASearch));
  VXMLDocument := TXMLDocument.Create(nil);

  try
    VXMLDocument.LoadFromFile(AFile);
    VNode := VXMLDocument.DocumentElement;
    if (VNode <> nil) and (VNode.ChildNodes.Count > 0) then begin
      for I := 0 to VNode.ChildNodes.Count - 1 do begin
        if VNode.ChildNodes[I].NodeName = 'wpt' then begin
          VPlacemarkNode := VNode.ChildNodes[I];
          for J := 0 to VPlacemarkNode.GetAttributeNodes.getcount - 1 do begin
            VLatLonNode := VPlacemarkNode.GetAttributeNodes.get(J);
            if VLatLonNode.GetNodeName = 'lon' then begin
              VPoint.X := StrToFloat(VLatLonNode.gettext, VFormatSettings);
            end;
            if VLatLonNode.GetNodeName = 'lat' then begin
              VPoint.Y := StrToFloat(VLatLonNode.gettext, VFormatSettings);
            end;
          end;

          VAddress := VPlacemarkNode.ChildNodes.FindNode('name').Text;
          VDesc := '';
          if VPlacemarkNode.ChildNodes.FindNode('desc') <> nil then begin
            VDesc := VPlacemarkNode.ChildNodes.FindNode('desc').Text;
          end;
          if VPlacemarkNode.ChildNodes.FindNode('ele') <> nil then begin
            if VDesc <> '' then VDesc := VDesc + #$D#$A;
            VDesc := VDesc + 'Elevation ' + VPlacemarkNode.ChildNodes.FindNode('ele'). Text;
          end;

          if VPlacemarkNode.ChildNodes.FindNode('time') <> nil then begin
            VStrDateTime := VPlacemarkNode.ChildNodes.FindNode('time').Text; // '2015-12-02T08:54:43';
            VDesc := VDesc + #$D#$A + 'DateTime: ' + DateTimeToStr(FSystemTimeInternal.UTCToLocalTime(ISOToDateTime(VStrDateTime)));;
          end;

          VDesc := VDesc + #$D#$A + '[ ' + AValueConverter.LonLatConvert(VPoint) + ' ]';
          VDesc := VDesc + #$D#$A + AFile;
          VFullDesc := VAddress + '<br>' + VDesc;

          if VPlacemarkNode.ChildNodes.FindNode('url') <> nil then begin
            VFullDesc := VFullDesc + '<br><a href=' + VPlacemarkNode.ChildNodes.FindNode('url').Text + '>' + VPlacemarkNode.ChildNodes.FindNode('url').Text + '</a>';
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

          Vskip := True;
          if Pos(VSearch, AnsiUpperCase(VAddress)) <> 0 then begin
            Vskip := False;
          end else if Pos(VSearch, AnsiUpperCase(VFullDesc)) <> 0 then begin
            Vskip := False;
          end;

          if not Vskip then begin
            VPlace := PlacemarkFactory.Build(VPoint, VAddress, VDesc, VFullDesc, 4);
            Vskip := ItemExist(Vplace, AList, CDistForLine);
            if not Vskip then begin
              AList.Add(VPlace);
            end;
          end;
        end;

        if VNode.ChildNodes[I].NodeName = 'trk' then begin
          VPlacemarkNode := VNode.ChildNodes[I];
          VAddress := VPlacemarkNode.ChildNodes.FindNode('name').Text;
          VDesc := '';
          VFullDesc := '';
          if VPlacemarkNode.ChildNodes.FindNode('desc') <> nil then begin
            VDesc := VPlacemarkNode.ChildNodes.FindNode('desc').Text;
          end;
          if (Pos(VSearch, AnsiUpperCase(VAddress)) <> 0) or
            (Pos(VSearch, AnsiUpperCase(VFullDesc)) <> 0 )then begin
            if VPlacemarkNode.ChildNodes.FindNode('trkseg') <> nil then begin
              VPlacemarkNode := VPlacemarkNode.ChildNodes.FindNode('trkseg');
              if VPlacemarkNode.ChildNodes.Count >0 then begin
                for J := 0 to VPlacemarkNode.ChildNodes.Count - 1 do begin
                  if VPlacemarkNode.ChildNodes[J].NodeName = 'trkpt' then begin

                    VTrksegSubNode := VPlacemarkNode.ChildNodes[J];
                    VAttribNode := VTrksegSubNode.GetAttributeNodes;

                    for K := 0 to VAttribNode.getcount - 1 do begin
                      VLatLonNode := VAttribNode.get(K);
                      if VLatLonNode.GetNodeName = 'lon' then begin
                        VPoint.X := StrToFloat(VLatLonNode.gettext, VFormatSettings);
                      end else
                      if VLatLonNode.GetNodeName = 'lat' then begin
                        VPoint.Y := StrToFloat(VLatLonNode.gettext, VFormatSettings);
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
              if VDesc <> '' then  VDesc := VDesc + #$D#$A;
              VDesc := VDesc + 'DateTime: ' + VStrDate;
            end;
            VDesc := VDesc + #$D#$A + AFile;
            VDesc := VDesc  + #$D#$A + '[ ' + AValueConverter.LonLatConvert(VPoint) + ' ]';

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
              end;
              AList.Add(VItem);
            end;
          end;
          VPointsAggregator.Clear;
          VPath := nil;
          VStrDate := '';
        end;
      end;
    end;
  except
  end;

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

function TGeoCoderByGpx.DoSearch(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: string;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  VList: IInterfaceListSimple;
  Vpath: String;
  VSearchRec: TSearchRec;
  VMySearch: String;
  VValueConverter: IValueToStringConverter;
  VTxtGpxDateTime: string;
  VSearchDate: Boolean;
begin
  VMySearch := ASearch;

  VValueConverter := FValueToStringConverter.GetStatic;
  while PosEx('  ', VMySearch) > 0 do begin
    VMySearch := ReplaceStr(VMySearch, '  ', ' ');
  end;
  VList := TInterfaceListSimple.Create;
  VSearchDate := ParseDateTime(VMySearch, VTxtGpxDateTime);

  if FindFirst(FPath + '*.gpx', faAnyFile, VSearchRec) = 0 then begin
    repeat
      if (VSearchRec.Attr and faDirectory) = faDirectory then begin
        Continue;
      end;
      Vpath := FPath + VSearchRec.Name;
      if VSearchDate then
        SearchInGpxFileByDate(ACancelNotifier, AOperationID, Vpath, VTxtGpxDateTime, VList, VValueConverter)
      else
        SearchInGpxFileByName(ACancelNotifier, AOperationID, Vpath, VMySearch, Vlist, VValueConverter);

      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
    until FindNext(VSearchRec) <> 0;
  end;
  Result := VList;
end;

end.
