unit i_IImportFile;

interface

uses
  i_IImportConfig;

type
  IImportFile = interface
    ['{0EF61663-09C0-4C71-A6F0-4E26380296E9}']
    function ProcessImport(AFileName: string; AConfig: IImportConfig): Boolean;
  end;

implementation

end.
