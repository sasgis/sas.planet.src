unit i_FileNameIterator;

interface

type
  IFileNameIterator = interface
    ['{84B33B54-EFE1-41FF-B69C-4A3B59B4E121}']
    function GetRootFolderName: string;
    function Next(var AFileName: string): Boolean;
    procedure Reset;
  end;

  IFileNameIteratorFactory = interface
    ['{D4AB40AB-4853-4A53-8CFF-0975FAB34BD7}']
    function CreateIterator(
      const ARootFolderName: string;
      const AFolderNameFromRoot: string
    ): IFileNameIterator;
  end;

implementation

end.
