unit i_FileNameIterator;

interface

type
  IFileNameIterator = interface
    ['{84B33B54-EFE1-41FF-B69C-4A3B59B4E121}']
    function GetRootFolderName: WideString;
    function Next(var AFileName: WideString): Boolean;
    procedure Reset;
  end;

  IFileNameIteratorFactory = interface
    ['{D4AB40AB-4853-4A53-8CFF-0975FAB34BD7}']
    function CreateIterator(
      const ARootFolderName: WideString;
      const AFolderNameFromRoot: WideString
    ): IFileNameIterator;
  end;

implementation

end.
