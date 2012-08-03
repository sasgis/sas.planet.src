unit i_ArchiveReadWrite;

interface

uses
 i_BinaryData;

type
  IArchiveReader = interface
    ['{1C1C0E2A-929E-457D-880F-47EAD7FBD0CA}']
    function GetItemsCount: Integer;
    function GetItemByName(const AItemName: string): IBinaryData;
    function GetItemNameByIndex(const AItemIndex: Integer): string;
    function GetItemByIndex(const AItemIndex: Integer; out AItemName: string): IBinaryData;
  end;

  IArchiveWriter = interface
    ['{339859BF-E9E7-4E25-9BF8-5C7281734C52}']
    function AddFile(
      const AFileData: IBinaryData;
      const AFileNameInArchive: string;
      const AFileDate: TDateTime
    ): Integer;
  end;

implementation

end.
