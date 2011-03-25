unit u_MarksImportBase;

interface

uses
  Classes,
  i_MarksSimple,
  i_IImportFile,
  i_IImportConfig;

type
  TMarksImportBase = class(TInterfacedObject, IImportFile)
  protected
    function DoImport(AFileName: string; AConfig: IImportConfig): IInterfaceList; virtual; abstract;
  protected
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  end;
implementation

{ TMarksImportBase }

function TMarksImportBase.ProcessImport(AFileName: string;
  AConfig: IImportConfig): Boolean;
var
  VList: IInterfaceList;
begin
  Result := False;
  VList := DoImport(AFileName, AConfig);
  if VList <> nil then begin
    if VList.Count > 0 then begin
      AConfig.MarkDB.WriteMarksList(VList);
      Result := True;
    end;
  end;
end;

end.
