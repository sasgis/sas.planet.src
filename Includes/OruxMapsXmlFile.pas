unit OruxMapsXmlFile;

interface

uses
  System.Classes;

type
  TOruxMapsLayer = record
    Datum: string;
    Projection: string;
    Zoom: Integer;
    MinLat, MaxLat, MinLon, MaxLon: Double;
  end;
  TOruxMapsLayers = array of TOruxMapsLayer;

  TOruxMapsCalibrationInfo = record
    MapName: string;
    Layers: TOruxMapsLayers;

    procedure SaveToStream(AStream: TStream);
    procedure LoadFromStream(AStream: TStream);

    function SaveToBase64String: string;
    procedure LoadFromBase64String(const ABase64String: string);
  end;

  TOruxMapsXmlFile = record
    class function Parse(
      const AXmlFileName: string;
      out ACalibrationInfo: TOruxMapsCalibrationInfo
    ): Boolean; static;
  end;

implementation

uses
  Winapi.ActiveX,
  System.SysUtils,
  System.NetEncoding,
  Xml.XMLIntf,
  Xml.XMLDoc;

type
  TPointerStream = class(TCustomMemoryStream)
  public
    constructor Create(P: Pointer; Size: NativeInt);
    function Write(const Buffer; Count: LongInt): LongInt; override;
  end;

var
  GFormatSettings: TFormatSettings;

function ParseLayer(const AOruxTrackerNode: IXMLNode; out ALayer: TOruxMapsLayer): Boolean;
var
  VMapCalibrationInnerNode: IXMLNode;
  VMapChunksNode: IXMLNode;
  VMapBoundsNode: IXMLNode;
  VLevel: string;
  VLevelInt: Integer;
  VMinLat, VMaxLat, VMinLon, VMaxLon: string;
  VMinLatFloat, VMaxLatFloat, VMinLonFloat, VMaxLonFloat: Double;
begin
  Result := False;

  if AOruxTrackerNode.NodeName <> 'OruxTracker' then begin
    Exit;
  end;

  VMapCalibrationInnerNode := AOruxTrackerNode.ChildNodes.FindNode('MapCalibration');
  if not Assigned(VMapCalibrationInnerNode) or
     not SameText(VMapCalibrationInnerNode.GetAttribute('layers'), 'false')
  then begin
    Exit;
  end;

  VLevel := VMapCalibrationInnerNode.GetAttribute('layerLevel');
  if not TryStrToInt(VLevel, VLevelInt) or not (VLevelInt in [0..23]) then begin
    Exit;
  end;

  VMapChunksNode := VMapCalibrationInnerNode.ChildNodes.FindNode('MapChunks');
  if not Assigned(VMapChunksNode) then begin
    ALayer.Datum := '';
    ALayer.Projection := '';
  end else begin
    ALayer.Datum := VMapChunksNode.GetAttribute('datum');
    ALayer.Projection := VMapChunksNode.GetAttribute('projection');
  end;

  VMapBoundsNode := VMapCalibrationInnerNode.ChildNodes.FindNode('MapBounds');
  if not Assigned(VMapBoundsNode) then begin
    Exit;
  end;

  VMinLat := VMapBoundsNode.GetAttribute('minLat');
  VMaxLat := VMapBoundsNode.GetAttribute('maxLat');
  VMinLon := VMapBoundsNode.GetAttribute('minLon');
  VMaxLon := VMapBoundsNode.GetAttribute('maxLon');

  if not TryStrToFloat(VMinLat, VMinLatFloat, GFormatSettings) or
     not TryStrToFloat(VMaxLat, VMaxLatFloat, GFormatSettings) or
     not TryStrToFloat(VMinLon, VMinLonFloat, GFormatSettings) or
     not TryStrToFloat(VMaxLon, VMaxLonFloat, GFormatSettings)
  then begin
    Exit;
  end;

  ALayer.Zoom   := VLevelInt;
  ALayer.MinLat := VMinLatFloat;
  ALayer.MaxLat := VMaxLatFloat;
  ALayer.MinLon := VMinLonFloat;
  ALayer.MaxLon := VMaxLonFloat;

  Result := True;
end;

class function TOruxMapsXmlFile.Parse(
  const AXmlFileName: string;
  out ACalibrationInfo: TOruxMapsCalibrationInfo
): Boolean;
var
  I, VLayerIndex: Integer;
  VXMLDocument: IXMLDocument;
  VRootNode: IXMLNode;
  VMapCalibrationNode: IXMLNode;
  VMapNameNode: IXMLNode;
  VOruxTrackerNode: IXMLNode;
  VLayer: TOruxMapsLayer;
begin
  Result := False;

  ACalibrationInfo.MapName := '';
  SetLength(ACalibrationInfo.Layers, 0);

  CoInitialize(nil);
  try
    VXMLDocument := LoadXMLDocument(AXmlFileName);

    VRootNode := VXMLDocument.DocumentElement;
    if not Assigned(VRootNode) or (VRootNode.NodeName <> 'OruxTracker') then begin
      Exit;
    end;

    VMapCalibrationNode := VRootNode.ChildNodes.FindNode('MapCalibration');
    if not Assigned(VMapCalibrationNode) or
       not SameText(VMapCalibrationNode.GetAttribute('layers'), 'true') or
       not SameText(VMapCalibrationNode.GetAttribute('layerLevel'), '0')
    then begin
      Exit;
    end;

    VMapNameNode := VMapCalibrationNode.ChildNodes.FindNode('MapName');
    if Assigned(VMapNameNode) then begin
      ACalibrationInfo.MapName := VMapNameNode.Text;
    end;

    VLayerIndex := 0;

    for I := 0 to VMapCalibrationNode.ChildNodes.Count - 1 do begin
      VOruxTrackerNode := VMapCalibrationNode.ChildNodes.Get(I);
      if ParseLayer(VOruxTrackerNode, VLayer) then begin
        SetLength(ACalibrationInfo.Layers, VLayerIndex + 1);
        ACalibrationInfo.Layers[VLayerIndex] := VLayer;
        Inc(VLayerIndex);
      end;
    end;

    Result := Length(ACalibrationInfo.Layers) > 0;
  finally
    VXMLDocument := nil;
    CoUninitialize;
  end;
end;

{ TOruxMapsCalibrationInfo }

procedure TOruxMapsCalibrationInfo.SaveToStream(AStream: TStream);

  procedure _WriteString(const AStr: string);
  var
    VUtf8Str: UTF8String;
    VLen: Integer;
  begin
    VUtf8Str := UTF8Encode(AStr);
    VLen := Length(VUtf8Str);
    AStream.WriteBuffer(VLen, SizeOf(VLen));
    if VLen > 0 then begin
      AStream.WriteBuffer(VUtf8Str[1], VLen);
    end;
  end;

var
  I: Integer;
  VLayersCount: Integer;
begin
  _WriteString(MapName);

  VLayersCount := Length(Layers);
  AStream.WriteBuffer(VLayersCount, SizeOf(VLayersCount));

  for I := 0 to VLayersCount - 1 do begin
    _WriteString(Layers[I].Datum);
    _WriteString(Layers[I].Projection);

    AStream.WriteBuffer(Layers[I].Zoom, SizeOf(Layers[I].Zoom));

    AStream.WriteBuffer(Layers[I].MinLat, SizeOf(Layers[I].MinLat));
    AStream.WriteBuffer(Layers[I].MaxLat, SizeOf(Layers[I].MaxLat));
    AStream.WriteBuffer(Layers[I].MinLon, SizeOf(Layers[I].MinLon));
    AStream.WriteBuffer(Layers[I].MaxLon, SizeOf(Layers[I].MaxLon));
  end;
end;

procedure TOruxMapsCalibrationInfo.LoadFromStream(AStream: TStream);

  function _ReadString: string;
  var
    VLen: Integer;
    VUtf8Str: UTF8String;
  begin
    AStream.ReadBuffer(VLen, SizeOf(VLen));
    if VLen > 0 then begin
      SetLength(VUtf8Str, VLen);
      AStream.ReadBuffer(VUtf8Str[1], VLen);
      Result := UTF8ToString(VUtf8Str);
    end else begin
      Result := '';
    end;
  end;

var
  I: Integer;
  VLayersCount: Integer;
begin
  MapName := _ReadString;

  AStream.ReadBuffer(VLayersCount, SizeOf(VLayersCount));
  SetLength(Layers, VLayersCount);

  for I := 0 to VLayersCount - 1 do begin
    Layers[I].Datum := _ReadString;
    Layers[I].Projection := _ReadString;

    AStream.ReadBuffer(Layers[I].Zoom, SizeOf(Layers[I].Zoom));

    AStream.ReadBuffer(Layers[I].MinLat, SizeOf(Layers[I].MinLat));
    AStream.ReadBuffer(Layers[I].MaxLat, SizeOf(Layers[I].MaxLat));
    AStream.ReadBuffer(Layers[I].MinLon, SizeOf(Layers[I].MinLon));
    AStream.ReadBuffer(Layers[I].MaxLon, SizeOf(Layers[I].MaxLon));
  end;
end;

function TOruxMapsCalibrationInfo.SaveToBase64String: string;
var
  VStream: TMemoryStream;
begin
  VStream := TMemoryStream.Create;
  try
    SaveToStream(VStream);
    Result := TNetEncoding.Base64.EncodeBytesToString(VStream.Memory, VStream.Size);
  finally
    VStream.Free;
  end;
end;

procedure TOruxMapsCalibrationInfo.LoadFromBase64String(const ABase64String: string);
var
  VBytes: TBytes;
  VStream: TPointerStream;
begin
  MapName := '';
  Layers := nil;

  VBytes := TNetEncoding.Base64.DecodeStringToBytes(ABase64String);

  VStream := TPointerStream.Create(Pointer(VBytes), Length(VBytes));
  try
    LoadFromStream(VStream);
  finally
    VStream.Free;
  end;
end;

{ TPointerStream }

constructor TPointerStream.Create(P: Pointer; Size: NativeInt);
begin
  SetPointer(P, Size);
end;

function TPointerStream.Write(const Buffer; Count: LongInt): LongInt;
begin
  raise EWriteError.Create('Read only stream!');
end;

initialization
  GFormatSettings.DecimalSeparator := '.';

end.
