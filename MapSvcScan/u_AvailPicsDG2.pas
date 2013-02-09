{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_AvailPicsDG2;


interface

uses
  SysUtils,
  Classes,
  XMLIntf,
  XMLDoc,
  i_InetConfig,
  i_DownloadResult,
  i_DownloadRequest,
  u_DownloadRequest,
  u_AvailPicsAbstract;

type
  TAvailPicsdg2 = class(TAvailPicsByKey)
  public
    procedure AfterConstruction; override;

    function ContentType: String; override;

    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; override;

    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; override;
  end;


implementation

uses
  forms,
  windows,
  u_GeoToStr,
  xmldom;

{ TAvailPicsDG2 }

procedure TAvailPicsdg2.AfterConstruction;
begin
  inherited;
  FDefaultKey := '/b42d085,`5cd,353/,7`6b,a0ca6644ce/4';
end;

function TAvailPicsdg2.ContentType: String;
begin
  Result := 'text/xml';
end;


function TAvailPicsdg2.ParseResponse(const AResultOk: IDownloadResultOk): Integer;
var
  XMLDocument: TXMLDocument;
  Node: IXMLNode;
  PlacemarkNode: IXMLNode;
  VDate, VfeatureId, VDate1: String;
  VSampleDistance, VcompanyName : String;
  Vsource, VlegacyId, VproductType, VdataLayer : String;
  VposList : String;
  VAddResult: Boolean;
  i : integer;
  VParams: TStrings;
  VMemoryStream: TMemoryStream;
begin
  VMemoryStream := TMemoryStream.Create;
  VMemoryStream.Position:=0;
  VMemoryStream.SetSize(AResultOk.Data.Size);
  CopyMemory(VMemoryStream.Memory, AResultOk.Data.Buffer, AResultOk.Data.Size);
  Result:=0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  if (nil=VMemoryStream) or (0=VMemoryStream.Size) then
    Exit;

  XMLDocument := TXMLDocument.Create(Application);
  XMLDocument.LoadFromStream(VMemoryStream);
  Node := XMLDocument.DocumentElement;
  Node := Node.ChildNodes[0];
    if (Node <> nil) and (Node.ChildNodes.Count > 0) then begin
      for i := 0 to Node.ChildNodes.Count - 1 do begin
        PlacemarkNode := Node.ChildNodes[i];
        if PlacemarkNode.NodeName = 'DigitalGlobe:FinishedFeature' then begin
          try
            VParams:=nil;
            VDate := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:formattedDate').text;
            VfeatureId := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:featureId').text;
            vSampleDistance := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:groundSampleDistance').text;
            VlegacyId := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:legacyId').text;
            VDate1 := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:acquisitionDate').text;
            VproductType := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:productType').text;
            VdataLayer := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:dataLayer').text;
            Vsource := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:source').text;
            VcompanyName := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:companyName').text;

            PlacemarkNode := PlacemarkNode.ChildNodes.FindNode('DigitalGlobe:geometry');
            while  PlacemarkNode.ChildNodes.Count > 0 do PlacemarkNode := PlacemarkNode.ChildNodes[0];
            VposList := PlacemarkNode.text;

            if VDate='' then VDate := copy(VDate1,1,10);
            VDate[5] := DateSeparator;
            VDate[8] := DateSeparator;

            VParams:=TStringList.Create;
            VParams.Values['FeatureId'] := VfeatureId;
            VParams.Values['Date'] := VDate1;
            VParams.Values['LegacyId'] := VlegacyId;
            VParams.Values['Resolution'] := vSampleDistance;
            VParams.Values['Color'] := VproductType;
            VParams.Values['DataLayer'] := VdataLayer;
            VParams.Values['Source'] := Vsource;
            VParams.Values['Provider'] := VcompanyName;
            VParams.Values['VposList'] := VposList;
            if length(VlegacyId)<>0 then begin
              VParams.Values['IMAGE_FILE_URL'] := 'https://browse.digitalglobe.com/imagefinder/showBrowseImage?catalogId='+VlegacyId+'&imageHeight=1024&imageWidth=1024';
              VParams.Values['METADATA_URL'] := 'https://browse.digitalglobe.com/imagefinder/showBrowseMetadata?buffer=1.0&catalogId='+VlegacyId+'&imageHeight=natres&imageWidth=natres';
            end;

            VAddResult := FTileInfoPtr.AddImageProc(Self, VDate, 'DigitalGlobe', VParams);
            FreeAndNil(VParams);
            if VAddResult then begin
              Inc(Result);
            end;
          except
           if (nil<>VParams) then begin
             try
               VParams.Free;
             except
             end;
             VParams:=nil;
           end;
          end;
      end;
     end;
    end;
end;

function TAvailPicsdg2.GetRequest(const AInetConfig: IInetConfig): IDownloadRequest;
var
  VLink: string;
  key:string;
  i: integer;
begin
 Key:= FDefaultKey;
 For i := 1 to Length(Key) do Key[i] := Chr(Ord(Key[i])+1);
 VLink  := 'https://services.digitalglobe.com/catalogservice/wfsaccess?WIDTH=256&HEIGHT=256&CONNECTID='+Key+
            '&MAXFEATURES=25&SERVICE=WFS&REQUEST=GetFeature&TYPENAME=DigitalGlobe:FinishedFeature&VERSION=1.1.0&BBOX='+
            RoundEx(FTileInfoPtr.TileRect.Bottom, 8)+','+
            RoundEx(FTileInfoPtr.TileRect.Left, 8)+','+
            RoundEx(FTileInfoPtr.TileRect.Top, 8)+','+
            RoundEx(FTileInfoPtr.TileRect.Right, 8);

 Result := TDownloadRequest.Create(
           VLink,
           '',
           AInetConfig.GetStatic
           );

end;

end.
