unit u_SlsParser;

interface

uses
  i_BinaryData,
  i_VectorDataLoader,
  i_GeometryLonLatFactory,
  i_VectorItemSubset,
  i_VectorDataFactory,
  i_VectorItemSubsetBuilder,
  u_BaseInterfacedObject;

type
  TSlsParser = class(TBaseInterfacedObject, IVectorDataLoader)
  private
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FVectorDataFactory: IVectorDataFactory;
    FVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
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
  Classes,
  IniFiles,
  i_ConfigDataProvider,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  u_ConfigProviderHelpers,
  u_StreamReadOnlyByBinaryData,
  u_ConfigDataProviderByIniFile;

{ TSlsParser }

constructor TSlsParser.Create(
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

function TSlsParser.Load(
  const AData: IBinaryData;
  const AIdData: Pointer;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory
): IVectorItemSubset;
var
  VIniFile: TMemIniFile;
  VIniStrings: TStringList;
  VIniStream: TStream;
  VHLGData: IConfigDataProvider;
  VPolygonSection: IConfigDataProvider;
  VPolygon: IGeometryLonLatMultiPolygon;
  VItem: IVectorDataItemSimple;
  VList: IVectorItemSubsetBuilder;
begin
  Result := nil;
  VPolygon := nil;
  VHLGData := nil;
  if AData <> nil then begin
    VIniStream := TStreamReadOnlyByBinaryData.Create(AData);
    try
      VIniStream.Position := 0;
      VIniStrings := TStringList.Create;
      try
        VIniStrings.LoadFromStream(VIniStream);
        VIniFile := TMemIniFile.Create('');
        try
          VIniFile.SetStrings(VIniStrings);
          VHLGData := TConfigDataProviderByIniFile.CreateWithOwn(VIniFile);
          VIniFile := nil;
        finally
          VIniFile.Free;
        end;
      finally
        VIniStrings.Free;
      end;
    finally
      VIniStream.Free;
    end;
  end;
  if VHLGData <> nil then begin
    VPolygonSection := VHLGData.GetSubItem('Session');
    if VPolygonSection <> nil then begin
      VPolygon := ReadPolygon(VPolygonSection, FVectorGeometryLonLatFactory);
    end;
  end;
  if VPolygon <> nil then begin
    VItem :=
      FVectorDataFactory.BuildPoly(
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

