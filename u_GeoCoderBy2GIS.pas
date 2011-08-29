unit u_GeoCoderBy2GIS;

interface

uses
  Classes,
  forms,
  u_GeoTostr,
  XMLIntf,
  msxmldom,
  XMLDoc,
  u_GeoCoderBasic;

type
  TGeoCoderBy2GIS = class(TGeoCoderBasic)
  protected
    function PrepareURL(ASearch: WideString): string; override;
    function ParseStringToPlacemarksList(AStr: string; ASearch: WideString): IInterfaceList; override;
  public
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_GeoCoder,
  u_ResStrings,
  u_GeoCodePlacemark;

{ TGeoCoderBy2GIS }

function TGeoCoderBy2GIS.ParseStringToPlacemarksList(
  AStr: string; ASearch: WideString): IInterfaceList;
var
  Stream:TMemoryStream;
  Node:IXMLNode;
  PlacemarkNode, AddressNode:IXMLNode;
  i:Integer;
  VPoint: TDoublePoint;
  VDesc: string;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
  XMLDocument:TXMLDocument;
begin
  if AStr = '' then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceList.Create;
  Stream:=TMemoryStream.Create;
  XMLDocument:=TXMLDocument.Create(application);
  try
    Stream.Write(AStr[1],length(AStr));
    XMLDocument.LoadFromStream(Stream);
    Node:=XMLDocument.DocumentElement;
    Node:=Node.ChildNodes.FindNode('result');
    if (Node<>nil) and (Node.ChildNodes.Count>0) then begin
      for i:=0 to Node.ChildNodes.Count-1 do begin
        if Node.ChildNodes[i].NodeName='filial' then begin
          try
            PlacemarkNode:=Node.ChildNodes[i];
            AddressNode:=PlacemarkNode.ChildNodes.FindNode('name');
            VPoint.X:=StrToFloat(PlacemarkNode.ChildNodes.FindNode('lon').Text, VFormatSettings);
            VPoint.Y:=StrToFloat(PlacemarkNode.ChildNodes.FindNode('lat').Text, VFormatSettings);
            VDesc:=PlacemarkNode.ChildNodes.FindNode('city_name').Text+', '+
                   PlacemarkNode.ChildNodes.FindNode('address').text;
            if (AddressNode<>nil) then begin
              VPlace := TGeoCodePlacemark.Create(VPoint, AddressNode.Text, VDesc, '', 4);
              VList.Add(VPlace);
            end;
          except

          end;
        end;
      end;
    end;
    Result := VList;
  finally
    XMLDocument.Free;
    Stream.Free;
  end;
end;

function TGeoCoderBy2GIS.PrepareURL(ASearch: WideString): string;
var
  VSearch: String;
begin
  VSearch := ASearch;
  //point='+R2StrPoint(FCurrentPos.x)+','+R2StrPoint(FCurrentPos.y)+'&radius=40000&where=новосибирск'
  Result := 'http://catalog.api.2gis.ru/search?what='+URLEncode(AnsiToUtf8(VSearch))+
            '&point='+R2StrPoint(FCurrentPos.x)+','+R2StrPoint(FCurrentPos.y)+
            '&radius='+inttostr(40000)+
            '&page=1&pagesize=50&key=ruihvk0699&version=1.3&sort=relevance&output=xml';
end;

end.
