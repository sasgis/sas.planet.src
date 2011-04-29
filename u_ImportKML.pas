unit u_ImportKML;

interface

uses
  Classes,
  i_KmlInfoSimpleLoader,
  i_ImportConfig,
  u_MarksImportBase;

type
  TImportKML = class(TMarksImportBase)
  private
    FKmlLoader: IKmlInfoSimpleLoader;
  protected
    function DoImport(AFileName: string; AConfig: IImportConfig): IInterfaceList; override;
  public
    constructor Create(AKmlLoader: IKmlInfoSimpleLoader);
  end;

implementation

uses
  SysUtils,
  i_MarksSimple,
  u_KmlInfoSimple;

{ TImportKML }

constructor TImportKML.Create(AKmlLoader: IKmlInfoSimpleLoader);
begin
  FKmlLoader := AKmlLoader;
end;

function TImportKML.DoImport(AFileName: string;
  AConfig: IImportConfig): IInterfaceList;
var
  KML:TKmlInfoSimple;
  VMark: IMarkFull;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  KML:=TKmlInfoSimple.Create;
  try
    FKmlLoader.LoadFromFile(AFileName, KML);
    for i:=0 to length(KML.Data)-1 do begin
      VMark := nil;
      if KML.Data[i].IsPoint then begin
        if AConfig.TemplateNewPoint <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewPoint(
            KML.Data[i].coordinates[0],
            KML.Data[i].Name,
            KML.Data[i].description,
            AConfig.TemplateNewPoint
          );
        end;
      end else if KML.Data[i].IsPoly then begin
        if AConfig.TemplateNewPoly <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewPoly(
            KML.Data[i].coordinates,
            KML.Data[i].Name,
            KML.Data[i].description,
            AConfig.TemplateNewPoly
          );
        end;
      end else if KML.Data[i].IsLine then begin
        if AConfig.TemplateNewLine <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewLine(
            KML.Data[i].coordinates,
            KML.Data[i].Name,
            KML.Data[i].description,
            AConfig.TemplateNewLine
          );
        end;
      end;
      if VMark <> nil then begin
        Result.Add(VMark);
      end;
    end;
  finally
    KML.Free;
  end;
end;

end.
