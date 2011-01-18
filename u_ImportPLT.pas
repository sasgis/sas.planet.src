unit u_ImportPLT;

interface

uses
  i_IImportFile,
  i_IImportConfig;

type
  TImportPLT = class(TInterfacedObject, IImportFile)
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  end;

implementation

uses
  SysUtils,
  i_MarksSimple,
  UPLT;

{ TImportKML }

function TImportPLT.ProcessImport(AFileName: string;
  AConfig: IImportConfig): Boolean;
var
  PLT:TPLT;
  VMark: IMarkFull;
  i: Integer;
begin
  Result := False;
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
        AConfig.MarkDB.WriteMark(VMark);
        Result := True;
      end;
    end;
  finally
   plt.Free;
  end;
end;

end.

