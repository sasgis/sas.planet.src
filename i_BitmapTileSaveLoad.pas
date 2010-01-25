unit i_BitmapTileSaveLoad;

interface

uses
  Classes,
  GR32;

type
  IBitmapTileLoader = interface
    ['{07D84005-DD59-4750-BCCE-A02330734539}']
    procedure LoadFromFile(AFileName: string; ABtm: TBitmap32);
    procedure LoadFromStream(AStream: TStream; ABtm: TBitmap32);
  end;

  IBitmapTileSaver = interface
    ['{00853113-0F3E-441D-974E-CCBC2F5C6E10}']
    procedure SaveToFile(ABtm: TBitmap32; AFileName: string);
    procedure SaveToStream(ABtm: TBitmap32; AStream: TStream);
  end;

implementation

end.
 