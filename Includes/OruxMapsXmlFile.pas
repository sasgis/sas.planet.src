unit OruxMapsXmlFile;

interface

type
  TOruxMapsXmlFile = record
    class function Parse(const AXmlFileName: string): string; static;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  XMLIntf,
  XMLDoc;

class function TOruxMapsXmlFile.Parse(const AXmlFileName: string): string;
var
  I: Integer;
  VXMLDocument: IXMLDocument;
  VRootNode: IXMLNode;
  VMapCalibrationNode: IXMLNode;
  VOruxTrackerNode: IXMLNode;
  VMapCalibrationInnerNode: IXMLNode;
  VMapBoundsNode: IXMLNode;
  VLevel, VMaxLat, VMinLon: string;
  VLevelInt: Integer;
  VCoordFloat: Double;
  VFormatSettings: TFormatSettings;
begin
  Result := '';

  CoInitialize(nil);
  try
    VXMLDocument := LoadXMLDocument(AXmlFileName);

    VRootNode := VXMLDocument.DocumentElement;
    if VRootNode.NodeName <> 'OruxTracker' then begin
      Exit;
    end;

    VMapCalibrationNode := VRootNode.ChildNodes.FindNode('MapCalibration');

    if not Assigned(VMapCalibrationNode) or
       not SameText(VMapCalibrationNode.GetAttribute('layers'), 'true') or
       not SameText(VMapCalibrationNode.GetAttribute('layerLevel'), '0')
    then begin
      Exit;
    end;

    VFormatSettings.DecimalSeparator := '.';

    for I := 0 to VMapCalibrationNode.ChildNodes.Count - 1 do begin
      VOruxTrackerNode := VMapCalibrationNode.ChildNodes.Get(I);

      if VOruxTrackerNode.NodeName <> 'OruxTracker' then begin
        Continue;
      end;

      VMapCalibrationInnerNode := VOruxTrackerNode.ChildNodes.FindNode('MapCalibration');

      if not Assigned(VMapCalibrationInnerNode) or
         not SameText(VMapCalibrationInnerNode.GetAttribute('layers'), 'false')
      then begin
        Continue;
      end;

      VLevel := VMapCalibrationInnerNode.GetAttribute('layerLevel');
      if (VLevel = '') or
         not TryStrToInt(VLevel, VLevelInt) or
         not (VLevelInt in [0..23])
      then begin
        Continue;
      end;

      VMapBoundsNode := VMapCalibrationInnerNode.ChildNodes.FindNode('MapBounds');
      if not Assigned(VMapBoundsNode) then begin
        Continue;
      end;

      VMaxLat := VMapBoundsNode.GetAttribute('maxLat');
      if (VMaxLat = '') or
         not TryStrToFloat(VMaxLat, VCoordFloat, VFormatSettings) or
         (VCoordFloat > 90) or (VCoordFloat < -90)
      then begin
        Continue;
      end;

      VMinLon := VMapBoundsNode.GetAttribute('minLon');
      if (VMinLon = '') or
         not TryStrToFloat(VMinLon, VCoordFloat, VFormatSettings) or
         (VCoordFloat > 180) or (VCoordFloat < -180)
      then begin
        Continue;
      end;

      if Result <> '' then begin
        Result := Result + ' ';
      end;

      Result := Result + VLevel + ';' + VMinLon + ';' + VMaxLat;
    end;
  finally
    VXMLDocument := nil;
    CoUninitialize;
  end;
end;

end.