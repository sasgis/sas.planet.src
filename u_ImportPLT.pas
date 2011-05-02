unit u_ImportPLT;

interface

uses
  Classes,
  i_ImportConfig,
  u_MarksImportBase;

type
  TImportPLT = class(TMarksImportBase)
  protected
    function DoImport(AFileName: string; AConfig: IImportConfig): IInterfaceList; override;
  end;

implementation

uses
  SysUtils,
  i_MarksSimple,
  u_PLT;

{ TImportKML }

function TImportPLT.DoImport(AFileName: string;
  AConfig: IImportConfig): IInterfaceList;
var
  PLT:TPLT;
  VMark: IMarkFull;
  i: Integer;
begin
  Result := TInterfaceList.Create;
  PLT:=TPLT.Create;
  try
    PLT.loadFromFile(AFileName);
    for i:=0 to length(PLT.Data)-1 do  begin
      VMark := nil;
      if PLT.Data[i].IsPoint then begin
        if AConfig.TemplateNewPoint <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewPoint(
            PLT.Data[i].coordinates[0],
            PLT.Data[i].Name,
            PLT.Data[i].description,
            AConfig.TemplateNewPoint
          );
        end;
      end else if PLT.Data[i].IsPoly then begin
        if AConfig.TemplateNewPoly <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewPoly(
            PLT.Data[i].coordinates,
            PLT.Data[i].Name,
            PLT.Data[i].description,
            AConfig.TemplateNewPoly
          );
        end;
      end else if PLT.Data[i].IsLine then begin
        if AConfig.TemplateNewLine <> nil then begin
          VMark := AConfig.MarkDB.Factory.CreateNewLine(
            PLT.Data[i].coordinates,
            PLT.Data[i].Name,
            PLT.Data[i].description,
            AConfig.TemplateNewLine
          );
        end;
      end;
      if VMark <> nil then begin
        Result.Add(VMark);
      end;
    end;
  finally
   plt.Free;
  end;
end;

end.

