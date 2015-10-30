unit KAZip;

interface

{$WARN SYMBOL_PLATFORM OFF}

{.$DEFINE USE_BZIP2}

uses
  Windows,
  SysUtils,
  Classes,
  Masks,
  TypInfo,
  {$IFDEF USE_BZIP2}
  BZip2,
  {$ENDIF}
  ALStringList,
  ZLib;

type
  TKAZipEntries         = class;
  TKAZip                = class;
  TBytes                = Array of Byte;
  TZipSaveMethod        = (FastSave, RebuildAll);
  TZipCompressionType   = (ctNormal, ctMaximum, ctFast, ctSuperFast, ctNone, ctUnknown);
  TZipCompressionMethod = (cmStored, cmShrunk, cmReduced1, cmReduced2, cmReduced3, cmReduced4, cmImploded, cmTokenizingReserved, cmDeflated, cmDeflated64, cmDCLImploding, cmPKWAREReserved);
  TOverwriteAction      = (oaSkip,oaSkipAll,oaOverwrite,oaOverwriteAll);

  TOnDecompressFile=Procedure(Sender:TObject; Current, Total : Integer) of Object;
  TOnCompressFile=Procedure(Sender:TObject; Current, Total : Integer) of Object;
  TOnZipOpen=Procedure(Sender:TObject; Current, Total : Integer) of Object;
  TOnZipChange=Procedure(Sender:TObject; ChangeType : Integer) of Object;
  TOnAddItem=Procedure(Sender:TObject; ItemName : AnsiString) of Object;
  TOnRebuildZip=Procedure(Sender:TObject; Current, Total : Integer) of Object;
  TOnRemoveItems=Procedure(Sender:TObject; Current, Total : Integer) of Object;
  TOnOverwriteFile=Procedure(Sender:TObject; Var FileName : String; Var Action : TOverwriteAction) of Object;

  {
          0 - The file is stored (no compression)
          1 - The file is Shrunk
          2 - The file is Reduced with compression factor 1
          3 - The file is Reduced with compression factor 2
          4 - The file is Reduced with compression factor 3
          5 - The file is Reduced with compression factor 4
          6 - The file is Imploded
          7 - Reserved for Tokenizing compression algorithm
          8 - The file is Deflated
          9 - Enhanced Deflating using Deflate64(tm)
         10 - PKWARE Data Compression Library Imploding
         11 - Reserved by PKWARE
         12 - File is compressed using BZIP2 algorithm
   }

  {DoChange Events
    0 - Zip is Closed;
    1 - Zip is Opened;
    2 - Item is added to the zip
    3 - Item is removed from the Zip
    4 - Item comment changed
    5 - Item name changed
    6 - Item name changed
  }

  TZLibStreamHeader = packed record
     CMF : Byte;
     FLG : Byte;
  end;

  TLocalFile = packed record
    LocalFileHeaderSignature       : Cardinal;   //    4 bytes  (0x04034b50)
    VersionNeededToExtract         : WORD;       //    2 bytes
    GeneralPurposeBitFlag          : WORD;       //    2 bytes
    CompressionMethod              : WORD;       //    2 bytes
    LastModFileTimeDate            : Cardinal;   //    4 bytes
    Crc32                          : Cardinal;   //    4 bytes
    CompressedSize                 : Cardinal;   //    4 bytes
    UncompressedSize               : Cardinal;   //    4 bytes
    FilenameLength                 : WORD;       //    2 bytes
    ExtraFieldLength               : WORD;       //    2 bytes
    FileName                       : AnsiString; //    variable size
    ExtraField                     : AnsiString; //    variable size
    CompressedData                 : AnsiString; //    variable size
  end;

  TDataDescriptor = packed record
    DescriptorSignature            : Cardinal;   //    4 bytes UNDOCUMENTED
    Crc32                          : Cardinal;   //    4 bytes
    CompressedSize                 : Cardinal;   //    4 bytes
    UncompressedSize               : Cardinal;   //    4 bytes
  End;

  TCentralDirectoryFile = packed record
    CentralFileHeaderSignature     : Cardinal;   //    4 bytes  (0x02014b50)
    VersionMadeBy                  : WORD;       //    2 bytes
    VersionNeededToExtract         : WORD;       //    2 bytes
    GeneralPurposeBitFlag          : WORD;       //    2 bytes
    CompressionMethod              : WORD;       //    2 bytes
    LastModFileTimeDate            : Cardinal;   //    4 bytes
    Crc32                          : Cardinal;   //    4 bytes
    CompressedSize                 : Cardinal;   //    4 bytes
    UncompressedSize               : Cardinal;   //    4 bytes
    FilenameLength                 : WORD;       //    2 bytes
    ExtraFieldLength               : WORD;       //    2 bytes
    FileCommentLength              : WORD;       //    2 bytes
    DiskNumberStart                : WORD;       //    2 bytes
    InternalFileAttributes         : WORD;       //    2 bytes
    ExternalFileAttributes         : Cardinal;   //    4 bytes
    RelativeOffsetOfLocalHeader    : Cardinal;   //    4 bytes
    FileName                       : AnsiString; //    variable size
    ExtraField                     : AnsiString; //    variable size
    FileComment                    : AnsiString; //    variable size
  end;

  TEndOfCentralDir = packed record
    EndOfCentralDirSignature        : Cardinal;  //    4 bytes  (0x06054b50)
    NumberOfThisDisk                : WORD;      //    2 bytes
    NumberOfTheDiskWithTheStart     : WORD;      //    2 bytes
    TotalNumberOfEntriesOnThisDisk  : WORD;      //    2 bytes
    TotalNumberOfEntries            : WORD;      //    2 bytes
    SizeOfTheCentralDirectory       : Cardinal;  //    4 bytes
    OffsetOfStartOfCentralDirectory : Cardinal;  //    4 bytes
    ZipfileCommentLength            : WORD;      //    2 bytes
  end;



  TKAZipEntriesEntry = Class(TCollectionItem)
  private
    { Private declarations }
    FParent               : TKAZipEntries;
    FCentralDirectoryFile : TCentralDirectoryFile;
    FLocalFile            : TLocalFile;
    FIsEncrypted          : Boolean;
    FIsFolder             : Boolean;
    FDate                 : TDateTime;
    FCompressionType      : TZipCompressionType;
    FSelected             : Boolean;

    procedure  SetSelected(const Value: Boolean);
    function   GetLocalEntrySize: Cardinal;
    function   GetCentralEntrySize: Cardinal;
    procedure  SetComment(const Value: AnsiString);
    procedure  SetFileName(const Value: AnsiString);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor  Destroy; override;
    Function    GetCompressedData : AnsiString; Overload;
    Function    GetCompressedData(Stream : TStream) : Integer;Overload;
    procedure   ExtractToFile(FileName: String);
    procedure   ExtractToStream(Stream: TStream);
    procedure   SaveToFile(FileName: String);
    procedure   SaveToStream(Stream: TStream);
    Function    Test:Boolean;

    Property    FileName          : AnsiString               Read FCentralDirectoryFile.FileName                     Write SetFileName;
    Property    Comment           : AnsiString               Read FCentralDirectoryFile.FileComment                  Write SetComment;
    Property    SizeUncompressed  : Cardinal             Read FCentralDirectoryFile.UncompressedSize;
    Property    SizeCompressed    : Cardinal             Read FCentralDirectoryFile.CompressedSize;
    Property    Date              : TDateTime            Read FDate;
    Property    CRC32             : Cardinal             Read FCentralDirectoryFile.CRC32;
    Property    Attributes        : Cardinal             Read FCentralDirectoryFile.ExternalFileAttributes;
    Property    LocalOffset       : Cardinal             Read FCentralDirectoryFile.RelativeOffsetOfLocalHeader;
    Property    IsEncrypted       : Boolean              Read FIsEncrypted;
    Property    IsFolder          : Boolean              Read FIsFolder;
    Property    BitFlag           : Word                 Read FCentralDirectoryFile.GeneralPurposeBitFlag;
    Property    CompressionMethod : Word                 Read FCentralDirectoryFile.CompressionMethod;
    Property    CompressionType   : TZipCompressionType  Read FCompressionType;
    Property    LocalEntrySize    : Cardinal             Read GetLocalEntrySize;
    Property    CentralEntrySize  : Cardinal             Read GetCentralEntrySize;
    Property    Selected          : Boolean              Read FSelected                                          Write  SetSelected;
  End;

  TKAZipEntries = class(TCollection)
  private
    { Private declarations }
    FParent              : TKAZip;
    FIsZipFile           : Boolean;
    FLocalHeaderNumFiles : Integer;

    function    GetHeaderEntry(Index: Integer): TKAZipEntriesEntry;
    procedure   SetHeaderEntry(Index: Integer; const Value: TKAZipEntriesEntry);
  protected
    { Protected declarations }
    Function    ReadBA(MS: TStream;Sz,Poz:Integer): TBytes;
    function    Adler32(adler : uLong; buf : pByte; len : uInt) : uLong;
    function    CalcCRC32(const UncompressedData : AnsiString): Cardinal;
    function    CalculateCRCFromStream(Stream: TStream): Cardinal;
    Function    RemoveRootName(Const FileName, RootName : String):AnsiString;
    Procedure   SortList(List : TList);
    function    FileTime2DateTime(FileTime: TFileTime): TDateTime;
    //**************************************************************************
    Function    FindCentralDirectory(MS:TStream):Boolean;
    function    ParseCentralHeaders(MS: TStream): Boolean;
    function    GetLocalEntry(MS: TStream; Offset : Integer; HeaderOnly : Boolean): TLocalFile;
    Procedure   LoadLocalHeaders(MS: TStream);
    Function    ParseLocalHeaders(MS:TStream):Boolean;

    //**************************************************************************
    procedure   Remove(ItemIndex: Integer; Flush : Boolean);Overload;
    procedure   RemoveBatch(Files : TList);
    procedure   InternalExtractToFile(Item: TKAZipEntriesEntry; FileName: String);
    //**************************************************************************
    Function    AddStreamFast(ItemName:AnsiString; FileAttr : Word; FileDate : TDateTime; Stream:TStream):TKAZipEntriesEntry;Overload;
    Function    AddStreamRebuild(ItemName:AnsiString; FileAttr : Word; FileDate : TDateTime; Stream:TStream):TKAZipEntriesEntry;
    Function    AddFolderChain(ItemName:AnsiString):Boolean;Overload;
    Function    AddFolderChain(ItemName:AnsiString; FileAttr : Word; FileDate : TDateTime):Boolean;Overload;
    Function    AddFolderEx(FolderName:String; RootFolder:String; WildCard:String; WithSubFolders : Boolean):Boolean;
    //**************************************************************************
  public
    { Public declarations }
    Procedure   ParseZip(MS:TStream);
    Constructor Create(AOwner : TKAZip; MS : TStream);Overload;
    Constructor Create(AOwner : TKAZip);Overload;
    Destructor  Destroy; Override;
    //**************************************************************************
    Function    IndexOf(Const FileName:AnsiString):Integer;
    //**************************************************************************
    Function    AddFile(FileName: string; NewFileName: AnsiString):TKAZipEntriesEntry;Overload;
    Function    AddFile(FileName: string):TKAZipEntriesEntry;Overload;
    Function    AddFiles(FileNames:TStrings):Boolean;
    Function    AddFolder(FolderName:String; RootFolder:String; WildCard:String; WithSubFolders : Boolean):Boolean;
    Function    AddFilesAndFolders(FileNames:TStrings; RootFolder:String; WithSubFolders : Boolean):Boolean;
    Function    AddStream(FileName:AnsiString; FileAttr : Word; FileDate : TDateTime; Stream:TStream):TKAZipEntriesEntry;Overload;
    Function    AddStream(FileName: AnsiString; Stream : TStream):TKAZipEntriesEntry;Overload;
    //**************************************************************************
    Procedure   Remove(ItemIndex:Integer);Overload;
    Procedure   Remove(Item:TKAZipEntriesEntry);Overload;
    Procedure   Remove(FileName:AnsiString);Overload;
    Procedure   RemoveFiles(List : TList);
    Procedure   RemoveSelected;
    Procedure   Rebuild;
   //**************************************************************************
    Procedure   Select(WildCard : AnsiString);
    Procedure   SelectAll;
    Procedure   DeSelectAll;
    Procedure   InvertSelection;
    //**************************************************************************
    Procedure   Rename(Item : TKAZipEntriesEntry; NewFileName: AnsiString);Overload;
    Procedure   Rename(ItemIndex : Integer; NewFileName: AnsiString);Overload;
    Procedure   Rename(FileName: AnsiString; NewFileName: AnsiString);Overload;
    procedure   CreateFolder(FolderName: AnsiString; FolderDate: TDateTime);
    procedure   RenameFolder(FolderName : AnsiString; NewFolderName : AnsiString);
    procedure   RenameMultiple(Names : TALStringList; NewNames : TALStringList);

    //**************************************************************************
    procedure   ExtractToFile  (Item : TKAZipEntriesEntry; FileName: String);Overload;
    procedure   ExtractToFile  (ItemIndex : Integer; FileName: String);Overload;
    procedure   ExtractToFile  (FileName: AnsiString; DestinationFileName:String);Overload;
    procedure   ExtractToStream(Item : TKAZipEntriesEntry; Stream: TStream);
    procedure   ExtractAll(TargetDirectory:String);
    procedure   ExtractSelected(TargetDirectory:String);
    //**************************************************************************
    Property    Items[Index : Integer] : TKAZipEntriesEntry read GetHeaderEntry write SetHeaderEntry;
  end;

  TKAZip = class(TComponent)
  private
    { Private declarations }
    FZipHeader            : TKAZipEntries;
    FIsDirty              : Boolean;
    FEndOfCentralDirPos   : Cardinal;
    FEndOfCentralDir      : TEndOfCentralDir;

    FZipCommentPos        : Cardinal;
    FZipComment           : TALStringList;

    FRebuildECDP          : Cardinal;
    FRebuildCP            : Cardinal;

    FIsZipFile            : Boolean;
    FHasBadEntries        : Boolean;
    FFileName             : String;
    FFileNames            : TALStringList;
    FZipSaveMethod        : TZipSaveMethod;

    FExternalStream       : Boolean;
    FStoreRelativePath    : Boolean;
    FZipCompressionType   : TZipCompressionType;

    FCurrentDFS           : Cardinal;
    FOnDecompressFile     : TOnDecompressFile;
    FOnCompressFile       : TOnCompressFile;
    FOnZipChange          : TOnZipChange;
    FBatchMode            : Boolean;

    NewLHOffsets          : Array of Cardinal;
    NewEndOfCentralDir    : TEndOfCentralDir;
    FOnZipOpen            : TOnZipOpen;
    FUseTempFiles         : Boolean;
    FStoreFolders         : Boolean;
    FOnAddItem            : TOnAddItem;
    FComponentVersion     : AnsiString;
    FOnRebuildZip         : TOnRebuildZip;
    FOnRemoveItems        : TOnRemoveItems;
    FOverwriteAction      : TOverwriteAction;
    FOnOverwriteFile      : TOnOverwriteFile;
    FReadOnly             : Boolean;
    FApplyAttributes      : Boolean;

    procedure   SetFileName(const Value: String);
    procedure   SetIsZipFile(const Value: Boolean);
    function    GetComment: TALStrings;
    procedure   SetComment(const Value: TALStrings);
    procedure   SetZipSaveMethod(const Value: TZipSaveMethod);
    procedure   SetActive(const Value: Boolean);
    procedure   SetZipCompressionType(const Value: TZipCompressionType);
    function    GetFileNames: TALStrings;
    procedure   SetFileNames(const Value: TALStrings);
    procedure   SetUseTempFiles(const Value: Boolean);
    procedure   SetStoreFolders(const Value: Boolean);
    procedure   SetOnAddItem(const Value: TOnAddItem);
    procedure   SetComponentVersion(const Value: AnsiString);
    procedure   SetOnRebuildZip(const Value: TOnRebuildZip);
    procedure   SetOnRemoveItems(const Value: TOnRemoveItems);
    procedure   SetOverwriteAction(const Value: TOverwriteAction);
    procedure   SetOnOverwriteFile(const Value: TOnOverwriteFile);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetApplyAtributes(const Value: Boolean);
  protected
    { Protected declarations }
    FZipStream  : TStream;
    //**************************************************************************
    Procedure   LoadFromFile(FileName: String);
    Procedure   LoadFromStream(MS : TStream; AExternalStream: Boolean);
    //**************************************************************************
    Procedure   RebuildLocalFiles(MS : TStream);
    Procedure   RebuildCentralDirectory(MS : TStream);
    Procedure   RebuildEndOfCentralDirectory(MS : TStream);
    //**************************************************************************
    procedure   OnDecompress(Sender:TObject);
    procedure   OnCompress(Sender:TObject);
    Procedure   DoChange(Sender:TObject; Const ChangeType : Integer);Virtual;
    //**************************************************************************
  public
    { Public declarations }
    Constructor Create(AOwner:TComponent);Override;
    Destructor  Destroy; Override;
    //**************************************************************************
    function    GetDelphiTempFileName: String;
    function    GetFileName(S: AnsiString): AnsiString;
    function    GetFilePath(S: AnsiString): AnsiString;
    //**************************************************************************
    Procedure   CreateZip(Stream:TStream);Overload;
    Procedure   CreateZip(FileName:String);Overload;
    Procedure   Open(FileName:String);Overload;
    Procedure   Open(MS : TStream);Overload;
    Procedure   SaveToStream(Stream:TStream);
    Procedure   Rebuild;
    Procedure   FixZip(MS : TStream);
    Procedure   Close;
    //**************************************************************************
    Function    AddFile(FileName: string; NewFileName: AnsiString):TKAZipEntriesEntry;Overload;
    Function    AddFile(FileName: String):TKAZipEntriesEntry;Overload;
    Function    AddFiles(FileNames:TStrings):Boolean;
    Function    AddFolder(FolderName:String; RootFolder:String; WildCard:String; WithSubFolders : Boolean):Boolean;
    Function    AddFilesAndFolders(FileNames:TStrings; RootFolder:String; WithSubFolders : Boolean):Boolean;
    Function    AddStream(FileName:AnsiString; FileAttr : Word; FileDate : TDateTime; Stream:TStream):TKAZipEntriesEntry;Overload;
    Function    AddStream(FileName: AnsiString; Stream : TStream):TKAZipEntriesEntry;Overload;
    //**************************************************************************
    Procedure   Remove(ItemIndex:Integer);Overload;
    Procedure   Remove(Item:TKAZipEntriesEntry);Overload;
    Procedure   Remove(FileName:AnsiString);Overload;
    Procedure   RemoveFiles(List : TList);
    Procedure   RemoveSelected;
    //**************************************************************************
    Procedure   Select(WildCard : AnsiString);
    Procedure   SelectAll;
    Procedure   DeSelectAll;
    Procedure   InvertSelection;
    //**************************************************************************
    Procedure   Rename(Item : TKAZipEntriesEntry; NewFileName: AnsiString);Overload;
    Procedure   Rename(ItemIndex : Integer; NewFileName: AnsiString);Overload;
    Procedure   Rename(FileName : AnsiString; NewFileName: AnsiString);Overload;
    Procedure   CreateFolder(FolderName : AnsiString; FolderDate : TDateTime);
    Procedure   RenameFolder(FolderName : AnsiString; NewFolderName : AnsiString);
    procedure   RenameMultiple(Names : TALStringList; NewNames : TALStringList);
    //**************************************************************************
    procedure   ExtractToFile  (Item      : TKAZipEntriesEntry; FileName: String);Overload;
    procedure   ExtractToFile  (ItemIndex : Integer; FileName: String);Overload;
    procedure   ExtractToFile  (FileName:AnsiString; DestinationFileName: string);Overload;
    procedure   ExtractToStream(Item      : TKAZipEntriesEntry; Stream: TStream);
    procedure   ExtractAll(TargetDirectory: String);
    procedure   ExtractSelected(TargetDirectory: String);
    //**************************************************************************
    Property    Entries         : TKAZipEntries Read FZipHeader;
    Property    HasBadEntries   : Boolean       Read FHasBadEntries;
  published
    { Published declarations }
    Property    FileName          : String              Read FFileName           Write SetFileName;
    Property    IsZipFile         : Boolean             Read FIsZipFile          Write SetIsZipFile;
    Property    SaveMethod        : TZipSaveMethod      Read FZipSaveMethod      Write SetZipSaveMethod;
    Property    StoreRelativePath : Boolean             Read FStoreRelativePath  Write FStoreRelativePath;
    Property    StoreFolders      : Boolean             read FStoreFolders       write SetStoreFolders;
    Property    CompressionType   : TZipCompressionType Read FZipCompressionType Write SetZipCompressionType;
    Property    Comment           : TALStrings            Read GetComment          Write SetComment;
    Property    FileNames         : TALStrings            Read GetFileNames        Write SetFileNames;
    Property    UseTempFiles      : Boolean             read FUseTempFiles       write SetUseTempFiles;
    Property    OverwriteAction   : TOverwriteAction    read FOverwriteAction    write SetOverwriteAction;
    Property    ComponentVersion  : AnsiString          read FComponentVersion   write SetComponentVersion;
    Property    ReadOnly          : Boolean             read FReadOnly           write SetReadOnly;
    Property    ApplyAtributes    : Boolean             read FApplyAttributes    write SetApplyAtributes;
    Property    OnDecompressFile  : TOnDecompressFile   Read FOnDecompressFile   Write FOnDecompressFile;
    Property    OnCompressFile    : TOnCompressFile     Read FOnCompressFile     Write FOnCompressFile;
    Property    OnZipChange       : TOnZipChange        Read FOnZipChange        Write FOnZipChange;
    Property    OnZipOpen         : TOnZipOpen          Read FOnZipOpen          Write FOnZipOpen;
    Property    OnAddItem         : TOnAddItem          read FOnAddItem          write SetOnAddItem;
    Property    OnRebuildZip      : TOnRebuildZip       read FOnRebuildZip       write SetOnRebuildZip;
    Property    OnRemoveItems     : TOnRemoveItems      read FOnRemoveItems      write SetOnRemoveItems;
    Property    OnOverwriteFile   : TOnOverwriteFile    read FOnOverwriteFile    write SetOnOverwriteFile;
    Property    Active            : Boolean             Read FIsZipFile          Write SetActive;
  end;

procedure Register;
Function ToZipName(FileName:AnsiString):AnsiString;
Function ToDosName(FileName:AnsiString):AnsiString;

implementation

uses
  ALString;

Const
  ZL_DEF_COMPRESSIONMETHOD  = $8;  { Deflate }
  ZL_ENCH_COMPRESSIONMETHOD = $9;  { Enchanced Deflate }
  ZL_DEF_COMPRESSIONINFO    = $7;  { 32k window for Deflate }
  ZL_PRESET_DICT            = $20;

  ZL_FASTEST_COMPRESSION    = $0;
  ZL_FAST_COMPRESSION       = $1;
  ZL_DEFAULT_COMPRESSION    = $2;
  ZL_MAXIMUM_COMPRESSION    = $3;

  ZL_FCHECK_MASK            = $1F;
  ZL_CINFO_MASK             = $F0; { mask out leftmost 4 bits }
  ZL_FLEVEL_MASK            = $C0; { mask out leftmost 2 bits }
  ZL_CM_MASK                = $0F; { mask out rightmost 4 bits }


  ZL_MULTIPLE_DISK_SIG      = $08074b50; // 'PK'#7#8
  ZL_DATA_DESCRIPT_SIG      = $08074b50; // 'PK'#7#8
  ZL_LOCAL_HEADERSIG        = $04034b50; // 'PK'#3#4
  ZL_CENTRAL_HEADERSIG      = $02014b50; // 'PK'#1#2
  ZL_EOC_HEADERSIG          = $06054b50; // 'PK'#5#6

  const
  CRCTable: array[0..255] of Cardinal = (
    $00000000, $77073096, $EE0E612C, $990951BA, $076DC419, $706AF48F, $E963A535,
    $9E6495A3, $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988, $09B64C2B, $7EB17CBD,
    $E7B82D07, $90BF1D91, $1DB71064, $6AB020F2, $F3B97148, $84BE41DE, $1ADAD47D,
    $6DDDE4EB, $F4D4B551, $83D385C7, $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5, $3B6E20C8, $4C69105E, $D56041E4,
    $A2677172, $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B, $35B5A8FA, $42B2986C,
    $DBBBC9D6, $ACBCF940, $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59, $26D930AC,
    $51DE003A, $C8D75180, $BFD06116, $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924, $2F6F7C87, $58684C11, $C1611DAB,
    $B6662D3D, $76DC4190, $01DB7106, $98D220BC, $EFD5102A, $71B18589, $06B6B51F,
    $9FBFE4A5, $E8B8D433, $7807C9A2, $0F00F934, $9609A88E, $E10E9818, $7F6A0DBB,
    $086D3D2D, $91646C97, $E6635C01, $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457, $65B0D9C6, $12B7E950, $8BBEB8EA,
    $FCB9887C, $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65, $4DB26158, $3AB551CE,
    $A3BC0074, $D4BB30E2, $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB, $4369E96A,
    $346ED9FC, $AD678846, $DA60B8D0, $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086, $5768B525, $206F85B3, $B966D409,
    $CE61E49F, $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4, $59B33D17, $2EB40D81,
    $B7BD5C3B, $C0BA6CAD, $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A, $EAD54739,
    $9DD277AF, $04DB2615, $73DC1683, $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1, $F00F9344, $8708A3D2, $1E01F268,
    $6906C2FE, $F762575D, $806567CB, $196C3671, $6E6B06E7, $FED41B76, $89D32BE0,
    $10DA7A5A, $67DD4ACC, $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5, $D6D6A3E8,
    $A1D1937E, $38D8C2C4, $4FDFF252, $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60, $DF60EFC3, $A867DF55, $316E8EEF,
    $4669BE79, $CB61B38C, $BC66831A, $256FD2A0, $5268E236, $CC0C7795, $BB0B4703,
    $220216B9, $5505262F, $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04, $C2D7FFA7,
    $B5D0CF31, $2CD99E8B, $5BDEAE1D, $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713, $95BF4A82, $E2B87A14, $7BB12BAE,
    $0CB61B38, $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21, $86D3D2D4, $F1D4E242,
    $68DDB3F8, $1FDA836E, $81BE16CD, $F6B9265B, $6FB077E1, $18B74777, $88085AE6,
    $FF0F6A70, $66063BCA, $11010B5C, $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2, $A7672661, $D06016F7, $4969474D,
    $3E6E77DB, $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0, $A9BCAE53, $DEBB9EC5,
    $47B2CF7F, $30B5FFE9, $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6, $BAD03605,
    $CDD70693, $54DE5729, $23D967BF, $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);



procedure Register;
begin
  RegisterComponents('KA', [TKAZip]);
end;

Function ToZipName(FileName:AnsiString):AnsiString;
Var
 P : Integer;
Begin
  Result := FileName;
  Result := ALStringReplace(Result,'\','/',[rfReplaceAll]);
  P := ALPos(':/',Result);
  if P > 0 Then
     Begin
       System.Delete(Result,1,P+1);
     End;
  P := ALPos('//',Result);
  if P > 0 Then
     Begin
       System.Delete(Result,1,P+1);
       P := ALPos('/',Result);
       if P > 0 Then
          Begin
             System.Delete(Result,1,P);
             P := ALPos('/',Result);
             if P > 0 Then System.Delete(Result,1,P);
          End;
     End;
End;


Function ToDosName(FileName:AnsiString):AnsiString;
Var
 P : Integer;
Begin
  Result := FileName;
  Result := ALStringReplace(Result,'\','/',[rfReplaceAll]);
  P := ALPos(':/',Result);
  if P > 0 Then
     Begin
       System.Delete(Result,1,P+1);
     End;
  P := ALPos('//',Result);
  if P > 0 Then
     Begin
       System.Delete(Result,1,P+1);
       P := ALPos('/',Result);
       if P > 0 Then
          Begin
             System.Delete(Result,1,P);
             P := ALPos('/',Result);
             if P > 0 Then System.Delete(Result,1,P);
          End;
     End;
  Result := ALStringReplace(Result,'/','\',[rfReplaceAll]);
End;

{ TKAZipEntriesEntry }

constructor TKAZipEntriesEntry.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParent   := TKAZipEntries(Collection);
  FSelected := False;
end;

destructor TKAZipEntriesEntry.Destroy;
begin

  inherited Destroy;
end;

procedure TKAZipEntriesEntry.ExtractToFile(FileName: String);
begin
  FParent.ExtractToFile(Self,FileName);
end;

procedure TKAZipEntriesEntry.ExtractToStream(Stream: TStream);
begin
  FParent.ExtractToStream(Self,Stream);
end;

procedure TKAZipEntriesEntry.SaveToFile(FileName: String);
begin
  ExtractToFile(FileName);
end;

procedure TKAZipEntriesEntry.SaveToStream(Stream: TStream);
begin
  ExtractToStream(Stream);
end;


function TKAZipEntriesEntry.GetCompressedData(Stream: TStream): Integer;
Var
  FZLHeader : TZLibStreamHeader;
  BA        : TLocalFile;
  ZLH       : Word;
  Compress  : Byte;
begin
  Result := 0;
  if (CompressionMethod=8) Then
     Begin
       FZLHeader.CMF := (ZL_DEF_COMPRESSIONINFO shl 4);               { 32k Window size }
       FZLHeader.CMF := FZLHeader.CMF or ZL_DEF_COMPRESSIONMETHOD;    { Deflate }
       Compress := ZL_DEFAULT_COMPRESSION;
       Case BitFlag AND 6 of
            0 : Compress := ZL_DEFAULT_COMPRESSION;
            2 : Compress := ZL_MAXIMUM_COMPRESSION;
            4 : Compress := ZL_FAST_COMPRESSION;
            6 : Compress := ZL_FASTEST_COMPRESSION;
       End;
       FZLHeader.FLG := FZLHeader.FLG or (Compress shl 6);
       FZLHeader.FLG := FZLHeader.FLG and not ZL_PRESET_DICT;         { no preset dictionary}
       FZLHeader.FLG := FZLHeader.FLG and not ZL_FCHECK_MASK;
       ZLH           := (FZLHeader.CMF * 256) + FZLHeader.FLG;
       Inc(FZLHeader.FLG, 31 - (ZLH mod 31));
       Result := Result + Stream.Write(FZLHeader,SizeOf(FZLHeader));
     End;
  BA     := FParent.GetLocalEntry(FParent.FParent.FZipStream,LocalOffset,False);
  if BA.LocalFileHeaderSignature<>$04034b50 Then
     Begin
        Result := 0;
        Exit;
     End;
  if SizeCompressed > 0 Then
     Result := Result + Stream.Write(BA.CompressedData[1],SizeCompressed);
end;

function TKAZipEntriesEntry.GetCompressedData: AnsiString;
Var
  BA        : TLocalFile;
  FZLHeader : TZLibStreamHeader;
  ZLH       : Word;
  Compress  : Byte;
begin
  Result := '';
  if (CompressionMethod=0) or (CompressionMethod=8) Then
     Begin
       BA     := FParent.GetLocalEntry(FParent.FParent.FZipStream,LocalOffset,False);
       if BA.LocalFileHeaderSignature<>$04034b50 Then Exit;
       if (CompressionMethod=8) Then
          Begin
            FZLHeader.CMF := (ZL_DEF_COMPRESSIONINFO shl 4);               { 32k Window size }
            FZLHeader.CMF := FZLHeader.CMF or ZL_DEF_COMPRESSIONMETHOD;    { Deflate }
            Compress := ZL_DEFAULT_COMPRESSION;
            Case BitFlag AND 6 of
                 0 : Compress := ZL_DEFAULT_COMPRESSION;
                 2 : Compress := ZL_MAXIMUM_COMPRESSION;
                 4 : Compress := ZL_FAST_COMPRESSION;
                 6 : Compress := ZL_FASTEST_COMPRESSION;
            End;
            FZLHeader.FLG := FZLHeader.FLG or (Compress shl 6);
            FZLHeader.FLG := FZLHeader.FLG and not ZL_PRESET_DICT;         { no preset dictionary}
            FZLHeader.FLG := FZLHeader.FLG and not ZL_FCHECK_MASK;
            ZLH           := (FZLHeader.CMF * 256) + FZLHeader.FLG;
            Inc(FZLHeader.FLG, 31 - (ZLH mod 31));
            SetLength(Result,SizeOf(FZLHeader));
            SetString(Result,PAnsiChar(@FZLHeader),SizeOf(FZLHeader));
          End;
       Result := Result + BA.CompressedData;
     End
  Else
     Begin
       SetLength(Result,0);
     End;
End;

procedure TKAZipEntriesEntry.SetSelected(const Value: Boolean);
begin
  FSelected := Value;
end;

function TKAZipEntriesEntry.GetLocalEntrySize: Cardinal;
begin
 Result := SizeOf(TLocalFile) - 3*SizeOf(AnsiString)+
           FCentralDirectoryFile.CompressedSize+
           FCentralDirectoryFile.FilenameLength+
           FCentralDirectoryFile.ExtraFieldLength;
 if (FCentralDirectoryFile.GeneralPurposeBitFlag And (1 SHL 3)) > 0 Then
    Begin
      Result := Result + SizeOf(TDataDescriptor);
    End;
end;

function TKAZipEntriesEntry.GetCentralEntrySize: Cardinal;
begin
  Result := SizeOf(TCentralDirectoryFile) - 3*SizeOf(AnsiString)+
                   FCentralDirectoryFile.FilenameLength+
                   FCentralDirectoryFile.ExtraFieldLength+
                   FCentralDirectoryFile.FileCommentLength;
end;

function TKAZipEntriesEntry.Test: Boolean;
Var
  FS : TFileStream;
  MS : TMemoryStream;
  FN : String;
begin
  Result  := True;
  Try
    if NOT FIsEncrypted Then
       Begin
         if FParent.FParent.FUseTempFiles  Then
            Begin
              FN := String(FParent.FParent.GetDelphiTempFileName);
              FS := TFileStream.Create(FN,fmOpenReadWrite or FmCreate);
              Try
                ExtractToStream(FS);
                FS.Position := 0;
                Result      := FParent.CalculateCRCFromStream(FS) = CRC32;
              Finally
                FS.Free;
                DeleteFile(FN);
              End;
            End
         Else
            Begin
              MS := TMemoryStream.Create;
              Try
                ExtractToStream(MS);
                MS.Position := 0;
                Result      := FParent.CalculateCRCFromStream(MS) = CRC32;
              Finally
                MS.Free;
              End;
            End;
       End;
  Except
    Result  := False;
  End;
end;

procedure TKAZipEntriesEntry.SetComment(const Value: AnsiString);
begin
  FCentralDirectoryFile.FileComment := Value;
  FCentralDirectoryFile.FileCommentLength := Length(FCentralDirectoryFile.FileComment);
  FParent.Rebuild;
  if NOT FParent.FParent.FBatchMode Then
     Begin
       FParent.FParent.DoChange(FParent,4);
     End;
end;

procedure TKAZipEntriesEntry.SetFileName(const Value: AnsiString);
Var
  FN : AnsiString;
begin
  FN := ToZipName(Value);
  if FParent.IndexOf(FN) > -1 Then Raise Exception.Create('File with same name already exists in Archive!');
  FCentralDirectoryFile.FileName         := ToZipName(Value);
  FCentralDirectoryFile.FilenameLength   := Length(FCentralDirectoryFile.FileName);
  if NOT FParent.FParent.FBatchMode Then
     Begin
       FParent.Rebuild;
       FParent.FParent.DoChange(FParent,5);
     End;
end;

{ TKAZipEntries }
constructor TKAZipEntries.Create(AOwner : TKAZip);
begin
  Inherited Create(TKAZipEntriesEntry);
  FParent    := AOwner;
  FIsZipFile := False;
end;

constructor TKAZipEntries.Create(AOwner : TKAZip; MS : TStream);
begin
  Inherited Create(TKAZipEntriesEntry);
  FParent               := AOwner;
  FIsZipFile            := False;
  FLocalHeaderNumFiles  := 0;
  ParseZip(MS);
end;

destructor TKAZipEntries.Destroy;
begin

  inherited Destroy;
end;

function TKAZipEntries.Adler32(adler : uLong; buf : pByte; len : uInt) : uLong;
const
  BASE = uLong(65521);
  NMAX = 3854;
var
  s1, s2 : uLong;
  k      : Integer;
begin
  s1 := adler and $ffff;
  s2 := (adler shr 16) and $ffff;

  if not Assigned(buf) then
  begin
    adler32 := uLong(1);
    exit;
  end;

  while (len > 0) do
  begin
    if len < NMAX then
      k := len
    else
      k := NMAX;
    Dec(len, k);
    while (k > 0) do
    begin
      Inc(s1, buf^);
      Inc(s2, s1);
      Inc(buf);
      Dec(k);
    end;
    s1 := s1 mod BASE;
    s2 := s2 mod BASE;
  end;
  adler32 := (s2 shl 16) or s1;
end;

function TKAZipEntries.CalcCRC32(const UncompressedData : AnsiString): Cardinal;
var
  X : Integer;
begin
  Result := $FFFFFFFF;
  for X := 0 to Length(UncompressedData) - 1 do
      Begin
        Result := (Result SHR 8) XOR (CRCTable[Byte(Result) XOR Ord(UncompressedData[X+1])]);
      End;
  Result := Result XOR $FFFFFFFF;
end;


function TKAZipEntries.CalculateCRCFromStream(Stream: TStream): Cardinal;
var
  Buffer: array[1..8192] of Byte;
  I, ReadCount: Integer;
  TempResult: Longword;
begin
  TempResult := $FFFFFFFF;
  while (Stream.Position <> Stream.Size) do begin
    ReadCount := Stream.Read(Buffer, SizeOf(Buffer));
    for I := 1 to ReadCount do
      TempResult := ((TempResult shr 8) and $FFFFFF) xor CRCTable[(TempResult xor Longword(Buffer[I])) and $FF];
  end;
  Result := not TempResult;
end;

Function TKAZipEntries.RemoveRootName(Const FileName, RootName : String):AnsiString;
Var
  P : Integer;
  S : String;
Begin
  Result := AnsiString(FileName);
  P      := Pos(AnsiLowerCase(RootName),AnsiLowerCase(FileName));
  if P=1 Then
     Begin
       S := FileName;
       System.Delete(S,1,Length(RootName));
       if (Length(S) > 0) AND (S[1]='\') Then
          Begin
             System.Delete(S,1,1);
             Result := AnsiString(S);
          End;
     End;
End;

Procedure TKAZipEntries.SortList(List : TList);
Var
  X        : Integer;
  I1       : Cardinal;
  I2       : Cardinal;
  NoChange : Boolean;
Begin
  if List.Count=1 Then Exit;
  Repeat
    NoChange := True;
    For X := 0 To List.Count-2 Do
      Begin
        I1 := Integer(List.Items[X]);
        I2 := Integer(List.Items[X+1]);
        if I1 > I2 Then
           Begin
             List.Exchange(X,X+1);
             NoChange := False;
           End;
      End;
  Until NoChange;
End;




function TKAZipEntries.FileTime2DateTime(FileTime: TFileTime): TDateTime;
var
   LocalFileTime: TFileTime;
   SystemTime: TSystemTime;
begin
   FileTimeToLocalFileTime(FileTime, LocalFileTime) ;
   FileTimeToSystemTime(LocalFileTime, SystemTime) ;
   Result := SystemTimeToDateTime(SystemTime) ;
end;

function TKAZipEntries.GetHeaderEntry(Index: Integer): TKAZipEntriesEntry;
begin
  Result := TKAZipEntriesEntry(Inherited Items[Index]);
end;

procedure TKAZipEntries.SetHeaderEntry(Index: Integer; const Value: TKAZipEntriesEntry);
begin
  Inherited Items[Index] := TCollectionItem(Value);
end;

Function TKAZipEntries.ReadBA(MS: TStream; Sz, Poz:Integer): TBytes;
Begin
  SetLength(Result,SZ);
  MS.Position := Poz;
  MS.Read(Result[0],SZ);
End;

function TKAZipEntries.FindCentralDirectory(MS: TStream): Boolean;
Var
  SeekStart : Integer;
  Poz       : Integer;
  BR        : Integer;
  Byte_     : Array[0..3] of Byte;

begin
  Result     := False;
  if MS.Size < 22 Then Exit;
  if MS.Size < 256 Then
     SeekStart := MS.Size
  Else
     SeekStart := 256;
  Poz       := MS.Size-22;
  BR        := SeekStart;
  Repeat
    MS.Position := Poz;
    MS.Read(Byte_,4);
    If Byte_[0]=$50 Then
       Begin
         if  (Byte_[1]=$4B)
         And (Byte_[2]=$05)
         And (Byte_[3]=$06) Then
             Begin
               MS.Position                  := Poz;
               FParent.FEndOfCentralDirPos  := MS.Position;
               MS.Read(FParent.FEndOfCentralDir,SizeOf(FParent.FEndOfCentralDir));
               FParent.FZipCommentPos       := MS.Position;
               FParent.FZipComment.Clear;
               Result  := True;
             End
         Else
             Begin
               Dec(Poz,4);
               Dec(BR ,4);
             End;
       End
    Else
       Begin
         Dec(Poz);
         Dec(BR)
       End;
    if BR < 0 Then
       Begin
         Case SeekStart of
               256   : Begin
                        SeekStart := 1024;
                        Poz       := MS.Size-(256+22);
                        BR        := SeekStart;
                      End;
              1024  : Begin
                        SeekStart := 65536;
                        Poz       := MS.Size-(1024+22);
                        BR        := SeekStart;
                      End;
              65536 : Begin
                        SeekStart := -1;
                      End;
         End;
       End;
    if BR < 0              Then SeekStart := -1;
    if MS.Size < SeekStart Then SeekStart := -1;
  Until (Result) or (SeekStart=-1);
end;


function TKAZipEntries.ParseCentralHeaders(MS: TStream): Boolean;
Var
  X                 : Integer;
  Entry             : TKAZipEntriesEntry;
  CDFile            : TCentralDirectoryFile;
begin
  Result            := False;
  Try
    MS.Position     := FParent.FEndOfCentralDir.OffsetOfStartOfCentralDirectory;
    For X := 0 To FParent.FEndOfCentralDir.TotalNumberOfEntriesOnThisDisk-1 do
        Begin
          FillChar(CDFile,SizeOf(TCentralDirectoryFile),0);
          MS.Read(CDFile,SizeOf(TCentralDirectoryFile)-3*SizeOf(String));
          Entry                       := TKAZipEntriesEntry.Create(Self);
          if CDFile.LastModFileTimeDate > 0 then begin
            try
              Entry.FDate               := FileDateToDateTime(CDFile.LastModFileTimeDate);
            except
              Entry.FDate               := 0;
            end;
          end else begin
            Entry.FDate                 := 0;
          end;
          if (CDFile.GeneralPurposeBitFlag And 1) > 0 Then
              Entry.FIsEncrypted := True
          Else
              Entry.FIsEncrypted := False;
          If CDFile.FilenameLength > 0 Then
             Begin
               SetLength(CDFile.FileName,CDFile.FilenameLength);
               MS.Read(CDFile.FileName[1],   CDFile.FilenameLength)
             End;
          If CDFile.ExtraFieldLength > 0 Then
             Begin
               SetLength(CDFile.ExtraField,CDFile.ExtraFieldLength);
               MS.Read(CDFile.ExtraField[1], CDFile.ExtraFieldLength);
             End;
          If CDFile.FileCommentLength > 0 Then
             Begin
               SetLength(CDFile.FileComment,CDFile.FileCommentLength);
               MS.Read(CDFile.FileComment[1],CDFile.FileCommentLength);
             End;
          Entry.FIsFolder          := (CDFile.ExternalFileAttributes and faDirectory) > 0;

          Entry.FCompressionType   := ctUnknown;
          if (CDFile.CompressionMethod=8) or (CDFile.CompressionMethod=9) Then
             Begin
               Case CDFile.GeneralPurposeBitFlag AND 6 of
                    0 : Entry.FCompressionType := ctNormal;
                    2 : Entry.FCompressionType := ctMaximum;
                    4 : Entry.FCompressionType := ctFast;
                    6 : Entry.FCompressionType := ctSuperFast
               End;
             End;
          Entry.FCentralDirectoryFile := CDFile;
          Finalize(CDFile);
          If Assigned(FParent.FOnZipOpen) Then FParent.FOnZipOpen(FParent,X,FParent.FEndOfCentralDir.TotalNumberOfEntriesOnThisDisk);
        End;
   Except
     Exit;
   End;
   Result := Count=FParent.FEndOfCentralDir.TotalNumberOfEntriesOnThisDisk;
end;


procedure TKAZipEntries.ParseZip(MS: TStream);
begin
  FIsZipFile := False;
  Clear;
  if FindCentralDirectory(MS) Then
    Begin
      if ParseCentralHeaders(MS) Then
         Begin
           FIsZipFile := True;
           LoadLocalHeaders(MS);
         End;
    End
  Else
    Begin
      if ParseLocalHeaders(MS) Then
         Begin
           FIsZipFile := Count > 0;
           if FIsZipFile Then FParent.FHasBadEntries := True;
         End;
    End;
end;


function TKAZipEntries.GetLocalEntry(MS: TStream; Offset : Integer; HeaderOnly : Boolean): TLocalFile;
Var
  Byte_             : Array[0..4] of Byte;
  DataDescriptor    : TDataDescriptor;
begin
  Finalize(Result);
  FillChar(Result,SizeOf(Result),0);
  MS.Position := Offset;
  MS.Read(Byte_,4);
  if  (Byte_[0]  = $50)
  And (Byte_[1]  = $4B)
  And (Byte_[2]  = $03)
  And (Byte_[3]  = $04) Then
    Begin
      MS.Position := Offset;
      MS.Read(Result,SizeOf(Result)-3*SizeOf(AnsiString));
      if Result.FilenameLength > 0 Then
         Begin
           SetLength(Result.FileName,Result.FilenameLength);
           MS.Read(Result.FileName[1],Result.FilenameLength);
         End;
      if Result.ExtraFieldLength > 0 Then
         Begin
           SetLength(Result.ExtraField,Result.ExtraFieldLength);
           MS.Read(Result.ExtraField[1],Result.ExtraFieldLength);
         End;
      if (Result.GeneralPurposeBitFlag And (1 SHL 3)) > 0 Then
         Begin
           MS.Read(DataDescriptor,SizeOf(TDataDescriptor));
           Result.Crc32            := DataDescriptor.Crc32;
           Result.CompressedSize   := DataDescriptor.CompressedSize;
           Result.UnCompressedSize := DataDescriptor.UnCompressedSize;
         End;
      if Not HeaderOnly Then
         Begin
           if Result.CompressedSize > 0 Then
              Begin
                SetLength(Result.CompressedData,Result.CompressedSize);
                MS.Read(Result.CompressedData[1],Result.CompressedSize);
              End;
         End;
    End
  Else
    Begin
    End;
end;

procedure TKAZipEntries.LoadLocalHeaders(MS: TStream);
Var
  X : Integer;
begin
  FParent.FHasBadEntries := False;
  For X := 0 To Count-1 do
      Begin
        If Assigned(FParent.FOnZipOpen) Then FParent.FOnZipOpen(FParent,X,FParent.FEndOfCentralDir.TotalNumberOfEntriesOnThisDisk);
        Items[X].FLocalFile := GetLocalEntry(MS,Items[X].FCentralDirectoryFile.RelativeOffsetOfLocalHeader,True);
        if Items[X].FLocalFile.LocalFileHeaderSignature<>$04034b50 Then FParent.FHasBadEntries := True;
      End;
end;

function TKAZipEntries.ParseLocalHeaders(MS: TStream): Boolean;
Var
  Poz                 : Integer;
  NLE                 : Integer;
  Byte_               : Array[0..4] of Byte;
  LocalFile           : TLocalFile;
  DataDescriptor      : TDataDescriptor;
  Entry               : TKAZipEntriesEntry;
  CDFile              : TCentralDirectoryFile;
  CDSize              : Cardinal;
  L                   : Integer;
  NoMore              : Boolean;
begin
  Result               := False;
  FLocalHeaderNumFiles := 0;
  Clear;
  Try
      Poz    := 0;
      NLE    := 0;
      CDSize := 0;
      Repeat
        NoMore      := True;
        MS.Position := Poz;
        MS.Read(Byte_,4);
        if  (Byte_[0]  = $50)
        And (Byte_[1]  = $4B)
        And (Byte_[2]  = $03)
        And (Byte_[3]  = $04) Then
            Begin
              Result := True;
              Inc(FLocalHeaderNumFiles);
              NoMore      := False;
              MS.Position := Poz;
              MS.Read(LocalFile,SizeOf(TLocalFile)-3*SizeOf(String));
              if LocalFile.FilenameLength > 0 Then
                 Begin
                   SetLength(LocalFile.FileName,LocalFile.FilenameLength);
                   MS.Read(LocalFile.FileName[1],LocalFile.FilenameLength);
                 End;
              if LocalFile.ExtraFieldLength > 0 Then
                 Begin
                   SetLength(LocalFile.ExtraField,LocalFile.ExtraFieldLength);
                   MS.Read(LocalFile.ExtraField[1],LocalFile.ExtraFieldLength);
                 End;
              if (LocalFile.GeneralPurposeBitFlag And (1 SHL 3)) > 0 Then
                 Begin
                   MS.Read(DataDescriptor,SizeOf(TDataDescriptor));
                   LocalFile.Crc32            := DataDescriptor.Crc32;
                   LocalFile.CompressedSize   := DataDescriptor.CompressedSize;
                   LocalFile.UncompressedSize := DataDescriptor.UncompressedSize;
                 End;
              MS.Position := MS.Position+LocalFile.CompressedSize;

              FillChar(CDFile,SizeOf(TCentralDirectoryFile),0);
              CDFile.CentralFileHeaderSignature     := $02014B50;
              CDFile.VersionMadeBy                  := 20;
              CDFile.VersionNeededToExtract         := LocalFile.VersionNeededToExtract;
              CDFile.GeneralPurposeBitFlag          := LocalFile.GeneralPurposeBitFlag;
              CDFile.CompressionMethod              := LocalFile.CompressionMethod;
              CDFile.LastModFileTimeDate            := LocalFile.LastModFileTimeDate;
              CDFile.Crc32                          := LocalFile.Crc32;
              CDFile.CompressedSize                 := LocalFile.CompressedSize;
              CDFile.UncompressedSize               := LocalFile.UncompressedSize;
              CDFile.FilenameLength                 := LocalFile.FilenameLength;
              CDFile.ExtraFieldLength               := LocalFile.ExtraFieldLength;
              CDFile.FileCommentLength              := 0;
              CDFile.DiskNumberStart                := 0;
              CDFile.InternalFileAttributes         := LocalFile.VersionNeededToExtract;
              CDFile.ExternalFileAttributes         := faArchive;
              CDFile.RelativeOffsetOfLocalHeader    := Poz;
              CDFile.FileName                       := LocalFile.FileName;
              L := Length(CDFile.FileName);
              if L > 0 Then
                 Begin
                   if CDFile.FileName[L]='/' Then CDFile.ExternalFileAttributes := faDirectory;
                 End;
              CDFile.ExtraField                     := LocalFile.ExtraField;
              CDFile.FileComment                    := '';

              Entry                                 := TKAZipEntriesEntry.Create(Self);
              Entry.FDate                           := FileDateToDateTime(CDFile.LastModFileTimeDate);
              if (CDFile.GeneralPurposeBitFlag And 1) > 0 Then
                 Entry.FIsEncrypted    := True
              Else
                 Entry.FIsEncrypted    := False;
              Entry.FIsFolder          := (CDFile.ExternalFileAttributes and faDirectory) > 0;
              Entry.FCompressionType   := ctUnknown;
              if (CDFile.CompressionMethod=8) or (CDFile.CompressionMethod=9) Then
                 Begin
                   Case CDFile.GeneralPurposeBitFlag AND 6 of
                        0 : Entry.FCompressionType := ctNormal;
                        2 : Entry.FCompressionType := ctMaximum;
                        4 : Entry.FCompressionType := ctFast;
                        6 : Entry.FCompressionType := ctSuperFast
                   End;
                 End;
              Entry.FCentralDirectoryFile := CDFile;
              Poz         := MS.Position;
              Inc(NLE);
              CDSize      := CDSize+Entry.CentralEntrySize;
            End;
      Until NoMore;

      FParent.FEndOfCentralDir.EndOfCentralDirSignature        := $06054b50;
      FParent.FEndOfCentralDir.NumberOfThisDisk                := 0;
      FParent.FEndOfCentralDir.NumberOfTheDiskWithTheStart     := 0;
      FParent.FEndOfCentralDir.TotalNumberOfEntriesOnThisDisk  := NLE;
      FParent.FEndOfCentralDir.SizeOfTheCentralDirectory       := CDSize;
      FParent.FEndOfCentralDir.OffsetOfStartOfCentralDirectory := MS.Position;
      FParent.FEndOfCentralDir.ZipfileCommentLength            := 0;
  Except
    Exit;
  End;
end;

procedure TKAZipEntries.Remove(ItemIndex: Integer; Flush : Boolean);
Var
  TempStream          : TFileStream;
  TempMSStream        : TMemoryStream;
  TempFileName        : String;
  BUF                 : AnsiString;
  ZipComment          : AnsiString;
  OSL                 : Cardinal;
  //*********************************************
  X                   : Integer;
  TargetPos           : Cardinal;
  Border              : Cardinal;

  BufStart            : Integer;
  BufLen              : Integer;
  ShiftSize           : Cardinal;
  NewSize             : Cardinal;
begin
 TargetPos          := Items[ItemIndex].FCentralDirectoryFile.RelativeOffsetOfLocalHeader;
 ShiftSize          := Items[ItemIndex].LocalEntrySize;
 BufStart           := TargetPos+ShiftSize;
 BufLen             := FParent.FZipStream.Size-BufStart;
 Border             := TargetPos;
 Delete(ItemIndex);
 if (FParent.FZipSaveMethod=FastSave) AND (Count > 0) Then
    Begin
       ZipComment := FParent.Comment.Text;

       SetLength(BUF,BufLen);
       FParent.FZipStream.Position := BufStart;
       FParent.FZipStream.Read(BUF[1],BufLen);

       FParent.FZipStream.Position := TargetPos;
       FParent.FZipStream.Write(BUF[1],BufLen);

       SetLength(BUF,0);

       For X := 0 to Count-1 do
           Begin
             if Items[X].FCentralDirectoryFile.RelativeOffsetOfLocalHeader > Border Then
                Begin
                  Dec(Items[X].FCentralDirectoryFile.RelativeOffsetOfLocalHeader, ShiftSize);
                  TargetPos := TargetPos+Items[X].LocalEntrySize;
                End
           End;

       FParent.FZipStream.Position := TargetPos;
       //************************************ MARK START OF CENTRAL DIRECTORY
       FParent.FEndOfCentralDir.OffsetOfStartOfCentralDirectory := FParent.FZipStream.Position;
       //************************************ SAVE CENTRAL DIRECTORY
       For X := 0 To Count-1 do
           Begin
             FParent.FZipStream.Write(Self.Items[X].FCentralDirectoryFile,SizeOf(Self.Items[X].FCentralDirectoryFile)-3*SizeOf(String));
             if Self.Items[X].FCentralDirectoryFile.FilenameLength > 0 Then
                FParent.FZipStream.Write(Self.Items[X].FCentralDirectoryFile.FileName[1],Self.Items[X].FCentralDirectoryFile.FilenameLength);
             if Self.Items[X].FCentralDirectoryFile.ExtraFieldLength > 0 Then
                FParent.FZipStream.Write(Self.Items[X].FCentralDirectoryFile.ExtraField[1],Self.Items[X].FCentralDirectoryFile.ExtraFieldLength);
             if Self.Items[X].FCentralDirectoryFile.FileCommentLength > 0 Then
                FParent.FZipStream.Write(Self.Items[X].FCentralDirectoryFile.FileComment[1],Self.Items[X].FCentralDirectoryFile.FileCommentLength);
           End;
     //************************************ SAVE END CENTRAL DIRECTORY RECORD
     FParent.FEndOfCentralDirPos := FParent.FZipStream.Position;
     FParent.FEndOfCentralDir.SizeOfTheCentralDirectory := FParent.FEndOfCentralDirPos-FParent.FEndOfCentralDir.OffsetOfStartOfCentralDirectory;
     Dec(FParent.FEndOfCentralDir.TotalNumberOfEntriesOnThisDisk);
     Dec(FParent.FEndOfCentralDir.TotalNumberOfEntries);
     FParent.FZipStream.Write(FParent.FEndOfCentralDir, SizeOf(TEndOfCentralDir));
     //************************************ SAVE ZIP COMMENT IF ANY
     FParent.FZipCommentPos := FParent.FZipStream.Position;
     if Length(ZipComment) > 0 Then
        Begin
          FParent.FZipStream.Write(ZipComment[1],Length(ZipComment));
        End;
     FParent.FZipStream.Size     := FParent.FZipStream.Position;
    End
 Else
    Begin
       if FParent.FUseTempFiles Then
          Begin
             TempFileName := FParent.GetDelphiTempFileName;
             TempStream   := TFileStream.Create(TempFileName,fmOpenReadWrite or FmCreate);
             Try
               FParent.SaveToStream(TempStream);
               TempStream.Position := 0;
               OSL                 := FParent.FZipStream.Size;
               Try
                 FParent.FZipStream.Size := TempStream.Size;
               Except
                 FParent.FZipStream.Size := OSL;
                 Raise;
               End;
               FParent.FZipStream.Position := 0;
               FParent.FZipStream.CopyFrom(TempStream,TempStream.Size);
               //*********************************************************************
               FParent.FZipHeader.ParseZip(FParent.FZipStream);
               //*********************************************************************
             Finally
               TempStream.Free;
               DeleteFile(TempFileName)
             End;
          End
       Else
          Begin
            NewSize := 0;
            For X := 0 To Count-1 do
                Begin
                  NewSize := NewSize+Items[X].LocalEntrySize+Items[X].CentralEntrySize;
                  if Assigned(FParent.FOnRemoveItems) Then FParent.FOnRemoveItems(FParent,X,Count-1);
                End;
            NewSize := NewSize+SizeOf(FParent.FEndOfCentralDir)+FParent.FEndOfCentralDir.ZipfileCommentLength;
            TempMSStream := TMemoryStream.Create;
            Try
               TempMSStream.SetSize(NewSize);
               TempMSStream.Position := 0;
               FParent.SaveToStream(TempMSStream);
               TempMSStream.Position := 0;
               OSL                   := FParent.FZipStream.Size;
               Try
                 FParent.FZipStream.Size := TempMSStream.Size;
               Except
                 FParent.FZipStream.Size := OSL;
                 Raise;
               End;
               FParent.FZipStream.Position := 0;
               FParent.FZipStream.CopyFrom(TempMSStream,TempMSStream.Size);
               //*********************************************************************
               FParent.FZipHeader.ParseZip(FParent.FZipStream);
               //*********************************************************************
             Finally
               TempMSStream.Free;
             End;
          End;
    End;
 FParent.FIsDirty := True;
 if NOT FParent.FBatchMode Then
    Begin
       FParent.DoChange(FParent,3);
    End;
end;

procedure TKAZipEntries.Remove(ItemIndex: Integer);
Begin
  Remove(ItemIndex,True);
End;

procedure TKAZipEntries.Remove(Item: TKAZipEntriesEntry);
Var
 X : Integer;
begin
 For X := 0 To Count-1 do
     Begin
       if Self.Items[X]=Item Then
          Begin
            Remove(X);
            Exit;
          End;
     End;
end;

procedure TKAZipEntries.Remove(FileName: AnsiString);
Var
  I : Integer;
begin
  I := IndexOf(FileName);
  if I <> -1 Then Remove(I);
end;

procedure TKAZipEntries.RemoveBatch(Files:TList);
Var
  X             : Integer;
  OSL           : Integer;
  NewSize       : Cardinal;
  TempStream    : TFileStream;
  TempMSStream  : TMemoryStream;
  TempFileName  : String;
Begin
  For X := Files.Count-1 DownTo 0 do
      Begin
        Delete(Integer(Files.Items[X]));
        if Assigned(FParent.FOnRemoveItems) Then FParent.FOnRemoveItems(FParent,Files.Count-X,Files.Count);
      End;
  NewSize := 0;
  if FParent.FUseTempFiles Then
      Begin
         TempFileName := FParent.GetDelphiTempFileName;
         TempStream   := TFileStream.Create(TempFileName,fmOpenReadWrite or FmCreate);
         Try
           FParent.SaveToStream(TempStream);
           TempStream.Position := 0;
           OSL                 := FParent.FZipStream.Size;
           Try
             FParent.FZipStream.Size := TempStream.Size;
           Except
             FParent.FZipStream.Size := OSL;
             Raise;
           End;
           FParent.FZipStream.Position := 0;
           FParent.FZipStream.CopyFrom(TempStream,TempStream.Size);
           //*********************************************************************
           FParent.FZipHeader.ParseZip(FParent.FZipStream);
           //*********************************************************************
         Finally
           TempStream.Free;
           DeleteFile(TempFileName)
         End;
      End
   Else
      Begin
        For X := 0 To Count-1 do
            Begin
              NewSize := NewSize+Items[X].LocalEntrySize+Items[X].CentralEntrySize;
              if Assigned(FParent.FOnRemoveItems) Then FParent.FOnRemoveItems(FParent,X,Count-1);
            End;
        NewSize := NewSize+SizeOf(FParent.FEndOfCentralDir)+FParent.FEndOfCentralDir.ZipfileCommentLength;
        TempMSStream := TMemoryStream.Create;
        Try
           TempMSStream.SetSize(NewSize);
           TempMSStream.Position := 0;
           FParent.SaveToStream(TempMSStream);
           TempMSStream.Position := 0;
           OSL                   := FParent.FZipStream.Size;
           Try
             FParent.FZipStream.Size := TempMSStream.Size;
           Except
             FParent.FZipStream.Size := OSL;
             Raise;
           End;
           FParent.FZipStream.Position := 0;
           FParent.FZipStream.CopyFrom(TempMSStream,TempMSStream.Size);
           //*********************************************************************
           FParent.FZipHeader.ParseZip(FParent.FZipStream);
           //*********************************************************************
         Finally
           TempMSStream.Free;
         End;
      End;
End;

Function TKAZipEntries.IndexOf(Const FileName:AnsiString):Integer;
Var
  X   : Integer;
  FN  : AnsiString;
Begin
  Result := -1;
  FN     := ToZipName(FileName);
  For X := 0 To Count-1 do
      Begin
        if ALCompareText(FN,ToZipName(Items[X].FCentralDirectoryFile.FileName))=0 Then
           Begin
             Result := X;
             Exit;
           End;
      End;
End;


Function TKAZipEntries.AddStreamFast( ItemName  : AnsiString;
                                      FileAttr  : Word;
                                      FileDate  : TDateTime;
                                      Stream    : TStream):TKAZipEntriesEntry;
Var
  Compressor   : TCompressionStream;
  CS           : TALStringStream;
  CM           : WORD;
  S            : AnsiString;
  X            : Integer;
  I            : Integer;
  UL           : Integer;
  CL           : Integer;
  FCRC32       : Cardinal;
  SizeToAppend : Integer;
  ZipComment   : AnsiString;
  Level        : TCompressionLevel;
  OBM          : Boolean;
begin
  //*********************************** COMPRESS DATA
  ZipComment              := FParent.Comment.Text;

  if NOT FParent.FStoreRelativePath Then
     ItemName             := ALExtractFileName(ItemName);

  ItemName                := ToZipName(ItemName);
  I   := IndexOf(ItemName);
  if I > -1 Then
     Begin
       OBM := FParent.FBatchMode;
       Try
         if OBM=False Then FParent.FBatchMode := True;
         Remove(I);
       Finally
         FParent.FBatchMode := OBM;
       End;
     End;

  CS                      := TALStringStream.Create('');
  CS.Position             := 0;
  Try
    UL                    := Stream.Size-Stream.Position;
    SetLength(S,UL);
    CM                    := 0;
    if UL > 0 Then
       Begin
         Stream.Read(S[1],UL);
         CM               := 8;
       End;
    FCRC32                := CalcCRC32(S);
    FParent.FCurrentDFS   := UL;


    Level                 := clDefault;
    Case FParent.FZipCompressionType of
         ctNormal    : Level := clDefault;
         ctMaximum   : Level := clMax;
         ctFast      : Level := clFastest;
         ctSuperFast : Level := clFastest;
         ctNone      : Level := clNone;
    End;

    if CM = 8 Then
       Begin
         Compressor            := TCompressionStream.Create(Level,CS);
         Try
           Compressor.OnProgress := FParent.OnCompress;
           Compressor.Write(S[1],UL);
         Finally
           Compressor.Free;
         End;
         S                     := Copy(CS.DataString, 3, Length(CS.DataString)-6);
       End;
  Finally
    CS.Free;
  End;
  //***********************************
  CL  := Length(S);
  //*********************************** FILL RECORDS
  Result := TKAZipEntriesEntry(Self.Add);
  With Result.FLocalFile do
    Begin
      LocalFileHeaderSignature := $04034B50;
      VersionNeededToExtract   := 20;
      GeneralPurposeBitFlag    := 0;
      CompressionMethod        := CM;
      LastModFileTimeDate      := DateTimeToFileDate(FileDate);
      Crc32                    := FCRC32;
      CompressedSize           := CL;
      UncompressedSize         := UL;
      FilenameLength           := Length(ItemName);
      ExtraFieldLength         := 0;
      FileName                 := ItemName;
      ExtraField               := '';
      CompressedData           := '';
    End;

 With Result.FCentralDirectoryFile Do
   Begin
      CentralFileHeaderSignature     := $02014B50;
      VersionMadeBy                  := 20;
      VersionNeededToExtract         := 20;
      GeneralPurposeBitFlag          := 0;
      CompressionMethod              := CM;
      LastModFileTimeDate            := DateTimeToFileDate(FileDate);
      Crc32                          := FCRC32;
      CompressedSize                 := CL;
      UncompressedSize               := UL;
      FilenameLength                 := Length(ItemName);
      ExtraFieldLength               := 0;
      FileCommentLength              := 0;
      DiskNumberStart                := 0;
      InternalFileAttributes         := 0;
      ExternalFileAttributes         := FileAttr;
      RelativeOffsetOfLocalHeader    := FParent.FEndOfCentralDir.OffsetOfStartOfCentralDirectory;
      FileName                       := ItemName;
      ExtraField                     := '';
      FileComment                    := '';
   End;

 //************************************ EXPAND ZIP STREAM SIZE
 SizeToAppend := 0;
 SizeToAppend := SizeToAppend+SizeOf(Result.FLocalFile)-3*SizeOf(AnsiString);
 SizeToAppend := SizeToAppend+Result.FLocalFile.FilenameLength;
 SizeToAppend := SizeToAppend+CL;
 SizeToAppend := SizeToAppend+SizeOf(Result.FCentralDirectoryFile)-3*SizeOf(AnsiString);
 SizeToAppend := SizeToAppend+Result.FCentralDirectoryFile.FilenameLength;
 FParent.FZipStream.Size := FParent.FZipStream.Size+SizeToAppend;

 //************************************ SAVE LOCAL HEADER AND COMPRESSED DATA
 FParent.FZipStream.Position := Result.FCentralDirectoryFile.RelativeOffsetOfLocalHeader;
 FParent.FZipStream.Write(Result.FLocalFile,SizeOf(Result.FLocalFile)-3*SizeOf(AnsiString));
 if Result.FLocalFile.FilenameLength > 0 Then FParent.FZipStream.Write(Result.FLocalFile.FileName[1],Result.FLocalFile.FilenameLength);
 if CL > 0 Then FParent.FZipStream.Write(S[1],CL);

 //************************************ MARK START OF CENTRAL DIRECTORY
 FParent.FEndOfCentralDir.OffsetOfStartOfCentralDirectory := FParent.FZipStream.Position;

 //************************************ SAVE CENTRAL DIRECTORY
 For X := 0 To Count-1 do
     Begin
       FParent.FZipStream.Write(Self.Items[X].FCentralDirectoryFile,SizeOf(Self.Items[X].FCentralDirectoryFile)-3*SizeOf(AnsiString));
       if Self.Items[X].FCentralDirectoryFile.FilenameLength > 0 Then
          FParent.FZipStream.Write(Self.Items[X].FCentralDirectoryFile.FileName[1],Self.Items[X].FCentralDirectoryFile.FilenameLength);
       if Self.Items[X].FCentralDirectoryFile.ExtraFieldLength > 0 Then
          FParent.FZipStream.Write(Self.Items[X].FCentralDirectoryFile.ExtraField[1],Self.Items[X].FCentralDirectoryFile.ExtraFieldLength);
       if Self.Items[X].FCentralDirectoryFile.FileCommentLength > 0 Then
          FParent.FZipStream.Write(Self.Items[X].FCentralDirectoryFile.FileComment[1],Self.Items[X].FCentralDirectoryFile.FileCommentLength);
     End;

 //************************************ SAVE END CENTRAL DIRECTORY RECORD
 FParent.FEndOfCentralDirPos := FParent.FZipStream.Position;
 FParent.FEndOfCentralDir.SizeOfTheCentralDirectory := FParent.FEndOfCentralDirPos-FParent.FEndOfCentralDir.OffsetOfStartOfCentralDirectory;
 Inc(FParent.FEndOfCentralDir.TotalNumberOfEntriesOnThisDisk);
 Inc(FParent.FEndOfCentralDir.TotalNumberOfEntries);
 FParent.FZipStream.Write(FParent.FEndOfCentralDir, SizeOf(TEndOfCentralDir));

 //************************************ SAVE ZIP COMMENT IF ANY
 FParent.FZipCommentPos := FParent.FZipStream.Position;
 if Length(ZipComment) > 0 Then
    Begin
      FParent.FZipStream.Write(ZipComment[1],Length(ZipComment));
    End;

  Result.FDate   := FileDate;

  if (Result.FCentralDirectoryFile.GeneralPurposeBitFlag And 1) > 0 Then
      Result.FIsEncrypted := True
  Else
      Result.FIsEncrypted := False;
  Result.FIsFolder          := (Result.FCentralDirectoryFile.ExternalFileAttributes and faDirectory) > 0;
  Result.FCompressionType   := ctUnknown;
  if (Result.FCentralDirectoryFile.CompressionMethod=8) or (Result.FCentralDirectoryFile.CompressionMethod=9) Then
     Begin
       Case Result.FCentralDirectoryFile.GeneralPurposeBitFlag AND 6 of
            0 : Result.FCompressionType := ctNormal;
            2 : Result.FCompressionType := ctMaximum;
            4 : Result.FCompressionType := ctFast;
            6 : Result.FCompressionType := ctSuperFast
       End;
     End;
  FParent.FIsDirty := True;
  if NOT FParent.FBatchMode Then
    Begin
      FParent.DoChange(FParent,2);
    End;
end;

Function TKAZipEntries.AddStreamRebuild( ItemName  : AnsiString;
                                         FileAttr  : Word;
                                         FileDate  : TDateTime;
                                         Stream    : TStream):TKAZipEntriesEntry;
Var
  Compressor   : TCompressionStream;
  CS           : TALStringStream;
  CM           : Word;
  S            : AnsiString;
  UL           : Integer;
  CL           : Integer;
  I            : Integer;
  X            : Integer;
  FCRC32       : Cardinal;
  OSL          : Cardinal;
  NewSize      : Cardinal;
  ZipComment   : AnsiString;
  TempStream   : TFileStream;
  TempMSStream : TMemoryStream;
  TempFileName : String;
  Level        : TCompressionLevel;
  OBM          : Boolean;
Begin
  if FParent.FUseTempFiles Then
     Begin
        TempFileName := FParent.GetDelphiTempFileName;
        TempStream   := TFileStream.Create(TempFileName,fmOpenReadWrite or FmCreate);
        Try
            //*********************************** SAVE ALL OLD LOCAL ITEMS
            FParent.RebuildLocalFiles(TempStream);
            //*********************************** COMPRESS DATA
            ZipComment              := FParent.Comment.Text;
            if NOT FParent.FStoreRelativePath Then
               ItemName             := ALExtractFileName(ItemName);
            ItemName                := ToZipName(ItemName);
            I := IndexOf(ItemName);
            if I > -1 Then
               Begin
                 OBM := FParent.FBatchMode;
                 Try
                   if OBM=False Then FParent.FBatchMode := True;
                   Remove(I);
                 Finally
                   FParent.FBatchMode := OBM;
                 End;
               End;

            CM                      := 0;
            CS                      := TALStringStream.Create('');
            CS.Position             := 0;
            Try
              UL                    := Stream.Size-Stream.Position;
              SetLength(S,UL);
              if UL > 0 Then
                 Begin
                   Stream.Read(S[1],UL);
                   CM               := 8;
                 End;
              FCRC32                := CalcCRC32(S);
              FParent.FCurrentDFS   := UL;

              Level                 := clDefault;
              Case FParent.FZipCompressionType of
                   ctNormal    : Level := clDefault;
                   ctMaximum   : Level := clMax;
                   ctFast      : Level := clFastest;
                   ctSuperFast : Level := clFastest;
                   ctNone      : Level := clNone;
              End;

              if CM=8 Then
                 Begin
                    Compressor            := TCompressionStream.Create(Level,CS);
                    Try
                       Compressor.OnProgress := FParent.OnCompress;
                       Compressor.Write(S[1],UL);
                    Finally
                       Compressor.Free;
                    End;
                    S                     := Copy(CS.DataString, 3, Length(CS.DataString)-6);
                 End;
            Finally
              CS.Free;
            End;
            //************************************************************************
            CL := Length(S);
            //*********************************** FILL RECORDS
            Result := TKAZipEntriesEntry(Self.Add);
            With Result.FLocalFile do
              Begin
                LocalFileHeaderSignature := $04034B50;
                VersionNeededToExtract   := 20;
                GeneralPurposeBitFlag    := 0;
                CompressionMethod        := CM;
                LastModFileTimeDate      := DateTimeToFileDate(FileDate);
                Crc32                    := FCRC32;
                CompressedSize           := CL;
                UncompressedSize         := UL;
                FilenameLength           := Length(ItemName);
                ExtraFieldLength         := 0;
                FileName                 := ItemName;
                ExtraField               := '';
                CompressedData           := '';
              End;

            With Result.FCentralDirectoryFile Do
             Begin
                CentralFileHeaderSignature     := $02014B50;
                VersionMadeBy                  := 20;
                VersionNeededToExtract         := 20;
                GeneralPurposeBitFlag          := 0;
                CompressionMethod              := CM;
                LastModFileTimeDate            := DateTimeToFileDate(FileDate);
                Crc32                          := FCRC32;
                CompressedSize                 := CL;
                UncompressedSize               := UL;
                FilenameLength                 := Length(ItemName);
                ExtraFieldLength               := 0;
                FileCommentLength              := 0;
                DiskNumberStart                := 0;
                InternalFileAttributes         := 0;
                ExternalFileAttributes         := FileAttr;
                RelativeOffsetOfLocalHeader    := TempStream.Position;
                FileName                       := ItemName;
                ExtraField                     := '';
                FileComment                    := '';
             End;

           //************************************ SAVE LOCAL HEADER AND COMPRESSED DATA
           TempStream.Write(Result.FLocalFile,SizeOf(Result.FLocalFile)-3*SizeOf(AnsiString));
           if Result.FLocalFile.FilenameLength > 0 Then TempStream.Write(Result.FLocalFile.FileName[1],Result.FLocalFile.FilenameLength);
           if CL > 0 Then TempStream.Write(S[1],CL);
           //************************************
           FParent.NewLHOffsets[Count-1] := Result.FCentralDirectoryFile.RelativeOffsetOfLocalHeader;
           FParent.RebuildCentralDirectory(TempStream);
           FParent.RebuildEndOfCentralDirectory(TempStream);
           //************************************
           TempStream.Position := 0;
           OSL                 := FParent.FZipStream.Size;
           Try
             FParent.FZipStream.Size := TempStream.Size;
           Except
             FParent.FZipStream.Size := OSL;
             Raise;
           End;
           FParent.FZipStream.Position := 0;
           FParent.FZipStream.CopyFrom(TempStream,TempStream.Size);
       Finally
         TempStream.Free;
         DeleteFile(TempFileName)
       End;
     End
  Else
     Begin
        TempMSStream := TMemoryStream.Create;
        NewSize := 0;
        For X := 0 To Count-1 do
            Begin
              NewSize := NewSize+Items[X].LocalEntrySize+Items[X].CentralEntrySize;
              if Assigned(FParent.FOnRemoveItems) Then FParent.FOnRemoveItems(FParent,X,Count-1);
            End;
        NewSize := NewSize+SizeOf(FParent.FEndOfCentralDir)+FParent.FEndOfCentralDir.ZipfileCommentLength;
        Try
            TempMSStream.SetSize(NewSize);
            TempMSStream.Position := 0;
            //*********************************** SAVE ALL OLD LOCAL ITEMS
            FParent.RebuildLocalFiles(TempMSStream);
            //*********************************** COMPRESS DATA
            ZipComment              := FParent.Comment.Text;
            if NOT FParent.FStoreRelativePath Then
               ItemName             := ALExtractFileName(ItemName);
            ItemName                := ToZipName(ItemName);
            I := IndexOf(ItemName);
            if I > -1 Then
               Begin
                 OBM := FParent.FBatchMode;
                 Try
                   if OBM=False Then FParent.FBatchMode := True;
                   Remove(I);
                 Finally
                   FParent.FBatchMode := OBM;
                 End;
               End;

            CM                      := 0;
            CS                      := TALStringStream.Create('');
            CS.Position             := 0;
            Try
              UL                    := Stream.Size-Stream.Position;
              SetLength(S,UL);
              if UL > 0 Then
                 Begin
                   Stream.Read(S[1],UL);
                   CM               := 8;
                 End;
              FCRC32                := CalcCRC32(S);
              FParent.FCurrentDFS   := UL;

              Level                 := clDefault;
              Case FParent.FZipCompressionType of
                   ctNormal    : Level := clDefault;
                   ctMaximum   : Level := clMax;
                   ctFast      : Level := clFastest;
                   ctSuperFast : Level := clFastest;
                   ctNone      : Level := clNone;
              End;

              if CM=8 Then
                 Begin
                    Compressor            := TCompressionStream.Create(Level,CS);
                    Try
                       Compressor.OnProgress := FParent.OnCompress;
                       Compressor.Write(S[1],UL);
                    Finally
                       Compressor.Free;
                    End;
                    S                     := Copy(CS.DataString, 3, Length(CS.DataString)-6);
                 End;
            Finally
              CS.Free;
            End;
            //************************************************************************
            CL := Length(S);
            //*********************************** FILL RECORDS
            Result := TKAZipEntriesEntry(Self.Add);
            With Result.FLocalFile do
              Begin
                LocalFileHeaderSignature := $04034B50;
                VersionNeededToExtract   := 20;
                GeneralPurposeBitFlag    := 0;
                CompressionMethod        := CM;
                LastModFileTimeDate      := DateTimeToFileDate(FileDate);
                Crc32                    := FCRC32;
                CompressedSize           := CL;
                UncompressedSize         := UL;
                FilenameLength           := Length(ItemName);
                ExtraFieldLength         := 0;
                FileName                 := ItemName;
                ExtraField               := '';
                CompressedData           := '';
              End;

            With Result.FCentralDirectoryFile Do
             Begin
                CentralFileHeaderSignature     := $02014B50;
                VersionMadeBy                  := 20;
                VersionNeededToExtract         := 20;
                GeneralPurposeBitFlag          := 0;
                CompressionMethod              := CM;
                LastModFileTimeDate            := DateTimeToFileDate(FileDate);
                Crc32                          := FCRC32;
                CompressedSize                 := CL;
                UncompressedSize               := UL;
                FilenameLength                 := Length(ItemName);
                ExtraFieldLength               := 0;
                FileCommentLength              := 0;
                DiskNumberStart                := 0;
                InternalFileAttributes         := 0;
                ExternalFileAttributes         := FileAttr;
                RelativeOffsetOfLocalHeader    := TempMSStream.Position;
                FileName                       := ItemName;
                ExtraField                     := '';
                FileComment                    := '';
             End;

           //************************************ SAVE LOCAL HEADER AND COMPRESSED DATA
           TempMSStream.Write(Result.FLocalFile,SizeOf(Result.FLocalFile)-3*SizeOf(AnsiString));
           if Result.FLocalFile.FilenameLength > 0 Then TempMSStream.Write(Result.FLocalFile.FileName[1],Result.FLocalFile.FilenameLength);
           if CL > 0 Then TempMSStream.Write(S[1],CL);
           //************************************
           FParent.NewLHOffsets[Count-1] := Result.FCentralDirectoryFile.RelativeOffsetOfLocalHeader;
           FParent.RebuildCentralDirectory(TempMSStream);
           FParent.RebuildEndOfCentralDirectory(TempMSStream);
           //************************************
           TempMSStream.Position := 0;
           OSL                 := FParent.FZipStream.Size;
           Try
             FParent.FZipStream.Size := TempMSStream.Size;
           Except
             FParent.FZipStream.Size := OSL;
             Raise;
           End;
           FParent.FZipStream.Position := 0;
           FParent.FZipStream.CopyFrom(TempMSStream,TempMSStream.Size);
       Finally
         TempMSStream.Free;
       End;
     End;

  Result.FDate              := FileDateToDateTime(Result.FCentralDirectoryFile.LastModFileTimeDate);
  if (Result.FCentralDirectoryFile.GeneralPurposeBitFlag And 1) > 0 Then
      Result.FIsEncrypted := True
  Else
      Result.FIsEncrypted := False;
  Result.FIsFolder          := (Result.FCentralDirectoryFile.ExternalFileAttributes and faDirectory) > 0;
  Result.FCompressionType   := ctUnknown;
  if (Result.FCentralDirectoryFile.CompressionMethod=8) or (Result.FCentralDirectoryFile.CompressionMethod=9) Then
     Begin
       Case Result.FCentralDirectoryFile.GeneralPurposeBitFlag AND 6 of
            0 : Result.FCompressionType := ctNormal;
            2 : Result.FCompressionType := ctMaximum;
            4 : Result.FCompressionType := ctFast;
            6 : Result.FCompressionType := ctSuperFast
       End;
     End;
 FParent.FIsDirty := True;
 if NOT FParent.FBatchMode Then
    Begin
      FParent.DoChange(FParent,2);
    End;
End;

function TKAZipEntries.AddFolderChain(ItemName: AnsiString; FileAttr: Word;
  FileDate: TDateTime): Boolean;
Var
 FN     : AnsiString;
 TN     : AnsiString;
 INCN   : AnsiString;
 P      : Integer;
 MS     : TMemoryStream;
 NoMore : Boolean;
Begin
  MS     := TMemoryStream.Create;
  Try
    Result := False;
    FN     := ALExtractFilePath(ToDosName(ToZipName(ItemName)));
    TN     := FN;
    INCN   := '';
    Repeat
      NoMore := True;
      P      := ALPos('\',TN);
      if P > 0 Then
         Begin
            INCN        := INCN+Copy(TN,1,P);
            System.Delete(TN,1,P);
            MS.Position := 0;
            MS.Size     := 0;
            If IndexOf(INCN) = -1 Then
               Begin
                  if FParent.FZipSaveMethod = FastSave Then
                     AddStreamFast(INCN,FileAttr,FileDate,MS)
                  Else
                  if FParent.FZipSaveMethod = RebuildAll Then
                     AddStreamRebuild(INCN,FileAttr,FileDate,MS);
               End;
            NoMore := False;
         End;
    Until NoMore;
    Result := True;
  Finally
    MS.Free;
  End;
End;

Function TKAZipEntries.AddFolderChain(ItemName : AnsiString):Boolean;
begin
  Result := AddFolderChain(ItemName,faDirectory,Now);
end;

function TKAZipEntries.AddStream(FileName : AnsiString; FileAttr : Word; FileDate : TDateTime; Stream : TStream):TKAZipEntriesEntry;
Begin
  Result := Nil;
  if (FParent.FStoreFolders) AND (FParent.FStoreRelativePath) Then AddFolderChain(FileName);
  if FParent.FZipSaveMethod = FastSave Then
     Result := AddStreamFast(FileName,FileAttr,FileDate,Stream)
  Else
  if FParent.FZipSaveMethod = RebuildAll Then
     Result := AddStreamRebuild(FileName,FileAttr,FileDate,Stream);
  if Assigned(FParent.FOnAddItem) Then FParent.FOnAddItem(FParent,FileName);
End;

Function TKAZipEntries.AddStream(FileName: AnsiString; Stream : TStream):TKAZipEntriesEntry;
begin
  Result := AddStream(FileName,faArchive,Now,Stream);
end;

Function TKAZipEntries.AddFile(FileName: String; NewFileName: AnsiString):TKAZipEntriesEntry;
Var
 FS  : TFileStream;
 Dir : TSearchRec;
 Res : Integer;
begin
 Result := Nil;
 Res    := FindFirst(FileName,faAnyFile,Dir);
 if Res=0 Then
    Begin
      FS := TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
      Try
        FS.Position := 0;
        Result := AddStream(NewFileName,Dir.Attr,FileDateToDateTime(Dir.Time),FS)
      Finally
        FS.Free;
      End;
    End;
 FindClose(Dir);
end;

Function TKAZipEntries.AddFile(FileName: String):TKAZipEntriesEntry;
begin
  Result := AddFile(FileName, AnsiString(FileName));
end;

function TKAZipEntries.AddFiles(FileNames: TStrings): Boolean;
Var
  X : Integer;
begin
  Result     := False;
  FParent.FBatchMode := True;
  Try
    For X := 0 To FileNames.Count-1 do AddFile(FileNames.Strings[X]);
  Except
    FParent.FBatchMode := False;
    FParent.DoChange(FParent,2);
    Exit;
  End;
  FParent.FBatchMode := False;
  FParent.DoChange(FParent,2);
  Result     := True;
end;

Function  TKAZipEntries.AddFolderEx(FolderName:String; RootFolder:String; WildCard : String; WithSubFolders : Boolean):Boolean;
Var
  Res : Integer;
  Dir : TSearchRec;
  FN  : String;
Begin
  Res := FindFirst(FolderName+'\*.*',faAnyFile,Dir);
  While Res=0 Do
     Begin
        if (Dir.Attr and faDirectory) > 0 Then
            Begin
              if (Dir.Name <> '..') And (Dir.Name <> '.') Then
                 Begin
                   FN := FolderName+'\'+Dir.Name;
                   if (FParent.FStoreFolders) AND (FParent.FStoreRelativePath) Then
                      AddFolderChain(RemoveRootName(FN+'\',RootFolder),Dir.Attr,FileDateToDateTime(Dir.Time));
                   if WithSubFolders Then
                      Begin
                        AddFolderEx(FN, RootFolder, WildCard, WithSubFolders);
                      End;
                 End
              Else
                 Begin
                   if (Dir.Name = '.') Then AddFolderChain(RemoveRootName(FolderName+'\',RootFolder),Dir.Attr,FileDateToDateTime(Dir.Time));
                 End;
            End
        Else
            Begin
              FN := FolderName+'\'+Dir.Name;
              if MatchesMask(FN,WildCard) Then
                 Begin
                   AddFile(FN,RemoveRootName(FN,RootFolder));
                 End;
            End;
        Res := FindNext(Dir);
     End;
  FindClose(Dir);
  Result := True;
End;

Function  TKAZipEntries.AddFolder(FolderName:String; RootFolder:String; WildCard : String; WithSubFolders : Boolean):Boolean;
Begin
  FParent.FBatchMode := True;
  Try
    Result := AddFolderEx(FolderName,RootFolder,WildCard,WithSubFolders);
  Finally
    FParent.FBatchMode := False;
    FParent.DoChange(FParent,2);
  End;
End;

Function TKAZipEntries.AddFilesAndFolders(FileNames:TStrings; RootFolder:String; WithSubFolders : Boolean):Boolean;
Var
  X   : Integer;
  Res : Integer;
  Dir : TSearchRec;
Begin
  FParent.FBatchMode := True;
  Try
    For X := 0 To FileNames.Count-1 do
        Begin
           Res := FindFirst(FileNames.Strings[X],faAnyFile,Dir);
           if Res=0 Then
              Begin
                if (Dir.Attr and faDirectory) > 0 Then
                   Begin
                     if (Dir.Name <> '..') And (Dir.Name <> '.') Then
                        Begin
                          AddFolderEx(FileNames.Strings[X],RootFolder,'*.*',WithSubFolders);
                        End;  
                   End
                Else
                   Begin
                     AddFile(FileNames.Strings[X],RemoveRootName(FileNames.Strings[X],RootFolder));
                   End;
              End;
           FindClose(Dir);
        End;
  Finally
    FParent.FBatchMode := False;
    FParent.DoChange(FParent,2);
  End;
  Result := True;
End;


procedure TKAZipEntries.RemoveFiles(List: TList);
begin
  if List.Count=1 Then
     Begin
       Remove(Integer(List.Items[0]));
     End
  Else
     Begin
       SortList(List);
       FParent.FBatchMode := True;
       Try
         RemoveBatch(List);
       Finally
         FParent.FBatchMode := False;
         FParent.DoChange(Self,3);
       End;
     End;
end;

Procedure TKAZipEntries.RemoveSelected;
Var
 X    : Integer;
 List : TList;
Begin
 FParent.FBatchMode := True;
 List               := TList.Create;
 Try
    For X := 0 to Count-1 do
       Begin
         if Self.Items[X].Selected Then List.Add(Pointer(X));
       End;
    RemoveBatch(List);
 Finally
   List.Free;
   FParent.FBatchMode := False;
   FParent.DoChange(Self,3);
 End;
End;


procedure TKAZipEntries.ExtractToStream(Item : TKAZipEntriesEntry; Stream: TStream);
Var
  SFS             : TMemoryStream;
  TFS             : TStream;
  BUF             : AnsiString;
  NR              : Cardinal;
  Decompressor    : TDecompressionStream;
  {$IFDEF USE_BZIP2}
  DecompressorBZ2 : TBZDecompressionStream;
  {$ENDIF}
begin
  if  (
       (Item.CompressionMethod=8) or
       {$IFDEF USE_BZIP2}
       (Item.CompressionMethod=12) or
       {$ENDIF}
       (Item.CompressionMethod=0)
       )
  And (NOT Item.FIsEncrypted) Then
     Begin
        SFS := TMemoryStream.Create;
        TFS := Stream;
        Try
          if Item.GetCompressedData(SFS) > 0 Then
             Begin
                SFS.Position  := 0;
                FParent.FCurrentDFS   := Item.SizeUncompressed;
                //****************************************************** DEFLATE
                if (Item.CompressionMethod=8) Then
                   Begin
                      Decompressor  := TDecompressionStream.Create(SFS);
                      Decompressor.OnProgress := FParent.OnDecompress;
                      SetLength(BUF,FParent.FCurrentDFS);
                      Try
                        NR := Decompressor.Read(BUF[1],FParent.FCurrentDFS);
                        if NR=FParent.FCurrentDFS Then TFS.Write(BUF[1],FParent.FCurrentDFS);
                      Finally
                        Decompressor.Free;
                      End;
                   End
                //******************************************************* BZIP2
                {$IFDEF USE_BZIP2}
                Else
                If Item.CompressionMethod=12 Then
                   Begin
                      DecompressorBZ2  := TBZDecompressionStream.Create(SFS);
                      DecompressorBZ2.OnProgress := FParent.OnDecompress;
                      SetLength(BUF,FParent.FCurrentDFS);
                      Try
                        NR := DecompressorBZ2.Read(BUF[1],FParent.FCurrentDFS);
                        if NR=FParent.FCurrentDFS Then TFS.Write(BUF[1],FParent.FCurrentDFS);
                      Finally
                        DecompressorBZ2.Free;
                      End;
                   End
                {$ENDIF}
                //****************************************************** STORED
                Else
                If Item.CompressionMethod=0 Then
                   Begin
                     TFS.CopyFrom(SFS,FParent.FCurrentDFS);
                   End;
             End;
        Finally
          SFS.Free;
        End;
     End
  Else
     Begin
       Raise Exception.Create('Cannot process this file: '+String(Item.FileName)+' ');
     End;
end;

procedure TKAZipEntries.InternalExtractToFile(Item: TKAZipEntriesEntry;
  FileName: String);
Var
  TFS           : TFileStream;
  Attr          : Integer;
begin
  if Item.IsFolder Then
     Begin
       ForceDirectories(FileName);
     End
  Else
     Begin
        TFS := TFileStream.Create(FileName,fmCreate or fmOpenReadWrite or fmShareDenyNone);
        Try
          ExtractToStream(Item,TFS);
        Finally
          TFS.Free;
        End;
        If FParent.FApplyAttributes Then
           Begin
             Attr := faArchive;
             if Item.FCentralDirectoryFile.ExternalFileAttributes And faHidden   > 0 Then Attr := Attr Or faHidden;
             if Item.FCentralDirectoryFile.ExternalFileAttributes And faSysFile  > 0 Then Attr := Attr Or faSysFile;
             if Item.FCentralDirectoryFile.ExternalFileAttributes And faReadOnly > 0 Then Attr := Attr Or faReadOnly;
             FileSetAttr(FileName,Attr);
           End;
     End;
end;


procedure TKAZipEntries.ExtractToFile(Item: TKAZipEntriesEntry; FileName: String);
var
  Can : Boolean;
  OA  : TOverwriteAction;
Begin
  OA  := FParent.FOverwriteAction;
  Can := True;
  if ((OA<>oaOverwriteAll) And (OA<>oaSkipAll)) And (Assigned(FParent.FOnOverwriteFile)) Then
   Begin
      if FileExists(FileName) Then
         Begin
           FParent.FOnOverwriteFile(FParent,FileName,OA);
         End
      Else
         Begin
           OA := oaOverwrite;
         End;
   End;
   Case OA Of
     oaSkip          : Can := False;
     oaSkipAll       : Can := False;
     oaOverwrite     : Can := True;
     oaOverwriteAll  : Can := True;
   End;
   if Can Then InternalExtractToFile(Item, FileName);
End;

procedure TKAZipEntries.ExtractToFile(ItemIndex: Integer; FileName: String);
var
  Can       : Boolean;
  OA        : TOverwriteAction;                                                     
Begin
  OA  := FParent.FOverwriteAction;
  Can := True;
  if ((OA<>oaOverwriteAll) And (OA<>oaSkipAll)) And (Assigned(FParent.FOnOverwriteFile)) Then
   Begin
      if FileExists(FileName) Then
         Begin
           FParent.FOnOverwriteFile(FParent,FileName,OA);
         End
      Else
         Begin
           OA := oaOverwrite;
         End;
   End;
   Case OA Of
     oaSkip          : Can := False;
     oaSkipAll       : Can := False;
     oaOverwrite     : Can := True;
     oaOverwriteAll  : Can := True;
   End;
   if Can Then InternalExtractToFile(Items[ItemIndex],FileName);
end;

procedure TKAZipEntries.ExtractToFile(FileName: AnsiString; DestinationFileName: string);
Var
  I   : Integer;
  Can : Boolean;
  OA  : TOverwriteAction;
Begin
  OA  := FParent.FOverwriteAction;
  Can := True;
  if ((OA<>oaOverwriteAll) And (OA<>oaSkipAll)) And (Assigned(FParent.FOnOverwriteFile)) Then
   Begin
      if FileExists(DestinationFileName) Then
         Begin
           FParent.FOnOverwriteFile(FParent,DestinationFileName,OA);
         End
      Else
         Begin
           OA := oaOverwrite;
         End;
   End;
   Case OA Of
     oaSkip          : Can := False;
     oaSkipAll       : Can := False;
     oaOverwrite     : Can := True;
     oaOverwriteAll  : Can := True;
   End;
   if Can Then
      Begin
        I := IndexOf(FileName);
        InternalExtractToFile(Items[I],DestinationFileName);
      End;
end;

procedure TKAZipEntries.ExtractAll(TargetDirectory: String);
Var
  FN        : String;
  DN        : String;
  X         : Integer;
  Can       : Boolean;
  OA        : TOverwriteAction;
  FileName  : String;
begin
  OA    := FParent.FOverwriteAction;
  Can   := True;
  Try
    For X := 0 To Count-1 do
        Begin
          FN := string(FParent.GetFileName(Items[X].FileName));
          DN := string(FParent.GetFilePath(Items[X].FileName));
          if DN <> '' Then ForceDirectories(TargetDirectory+'\'+DN);
          FileName := TargetDirectory+'\'+DN+FN;
          if ((OA<>oaOverwriteAll) And (OA<>oaSkipAll)) And (Assigned(FParent.FOnOverwriteFile)) Then
             Begin
                if FileExists(FileName) Then
                   Begin
                     FParent.FOnOverwriteFile(FParent,FileName,OA);
                   End;
             End;
          Case OA Of
            oaSkip          : Can := False;
            oaSkipAll       : Can := False;
            oaOverwrite     : Can := True;
            oaOverwriteAll  : Can := True;
          End;
          if Can Then InternalExtractToFile(Items[X],FileName);
        End;
  Finally
  End;
end;

procedure TKAZipEntries.ExtractSelected(TargetDirectory: String);
Var
  FN        : String;
  DN        : String;
  X         : Integer;
  OA        : TOverwriteAction;
  Can       : Boolean;
  FileName  : String;
begin
  OA    := FParent.FOverwriteAction;
  Can   := True;
  Try
    For X := 0 To Count-1 do
      Begin
        if Items[X].FSelected Then
           Begin
              FN := String(FParent.GetFileName(Items[X].FileName));
              DN := String(FParent.GetFilePath(Items[X].FileName));
              if DN <> '' Then ForceDirectories(TargetDirectory+'\'+DN);
              FileName := TargetDirectory+'\'+DN+FN;
              if ((OA<>oaOverwriteAll) And (OA<>oaSkipAll)) And (Assigned(FParent.FOnOverwriteFile)) Then
                 Begin
                    if FileExists(FileName) Then
                       Begin
                         FParent.FOnOverwriteFile(FParent,FileName,OA);
                       End;
                 End;
              Case OA Of
                oaSkip          : Can := False;
                oaSkipAll       : Can := False;
                oaOverwrite     : Can := True;
                oaOverwriteAll  : Can := True;
              End;
             if Can Then InternalExtractToFile(Items[X],TargetDirectory+'\'+DN+FN);
           End;
      End;
  Finally
  End;
end;



procedure TKAZipEntries.DeSelectAll;
Var
  X : Integer;
begin
  For X := 0 To Count-1 do Items[X].Selected := False;
end;

procedure TKAZipEntries.InvertSelection;
Var
  X : Integer;
begin
  For X := 0 To Count-1 do Items[X].Selected := Not Items[X].Selected;
end;

procedure TKAZipEntries.SelectAll;
Var
  X : Integer;
begin
  For X := 0 To Count-1 do Items[X].Selected := True;
end;

procedure TKAZipEntries.Select(WildCard: AnsiString);
Var
  X : Integer;
begin
  For X := 0 To Count-1 do
      Begin
        if ALMatchesMask(ToDosName(Items[X].FileName),WildCard) Then
           Items[X].Selected := True;
      End;
end;


procedure TKAZipEntries.Rebuild;
begin
  FParent.Rebuild;
end;

procedure TKAZipEntries.Rename(Item: TKAZipEntriesEntry; NewFileName: AnsiString);
begin
  Item.FileName := NewFileName;
end;

procedure TKAZipEntries.Rename(ItemIndex: Integer; NewFileName: AnsiString);
begin
  Rename(Items[ItemIndex],NewFileName);
end;

procedure TKAZipEntries.Rename(FileName, NewFileName: AnsiString);
Var
  I    : Integer;
begin
  I := IndexOf(FileName);
  Rename(I,NewFileName);
end;


procedure TKAZipEntries.CreateFolder(FolderName: AnsiString; FolderDate: TDateTime);
Var
  FN : AnsiString;
begin
  FN       := ALIncludeTrailingPathDelimiter(FolderName);
  AddFolderChain(FN,faDirectory,FolderDate);
  FParent.FIsDirty := True;
end;

procedure TKAZipEntries.RenameFolder(FolderName : AnsiString; NewFolderName : AnsiString);
Var
  FN  : AnsiString;
  NFN : AnsiString;
  S   : AnsiString;
  X   : Integer;
  L   : Integer;
begin
  FN  := ToZipName(ALIncludeTrailingPathDelimiter(FolderName));
  NFN := ToZipName(ALIncludeTrailingPathDelimiter(NewFolderName));
  L   := Length(FN);
  if IndexOf(NFN) = -1 Then
     Begin
       For X := 0 To Count-1 do
           Begin
             S := Items[X].FileName;
             if ALPos(FN,S) = 1 Then
                Begin
                  System.Delete(S,1,L);
                  S := NFN+S;
                  Items[X].FileName := S;
                  FParent.FIsDirty := True;
                End;
           End;
       If (FParent.FIsDirty) And (FParent.FBatchMode=False) Then Rebuild;
     End;
end;

procedure TKAZipEntries.RenameMultiple(Names : TALStringList; NewNames : TALStringList);
Var
  X    : Integer;
  BR   : Integer;
  L    : Integer;
Begin
  BR := 0;
  If Names.Count <> NewNames.Count Then
     Begin
       Raise Exception.Create('Names and NewNames must have equal count');
     End
  Else
     Begin
       FParent.FBatchMode := True;
       Try
         For X := 0 To Names.Count-1 do
             Begin
               L := Length(Names.Strings[X]);
               if (L>0) And ((Names.Strings[X][L]='\') or (Names.Strings[X][L]='/')) Then
                  Begin
                    RenameFolder(Names.Strings[X],NewNames.Strings[X]);
                    Inc(BR);
                  End
               Else
                  Begin
                    Rename(Names.Strings[X],NewNames.Strings[X]);
                    Inc(BR);
                  End;
             End;
       Finally
         FParent.FBatchMode := False;
       End;
       If BR > 0 Then
          Begin
            Rebuild;
            FParent.DoChange(FParent,6);
          End;
     End;
End;


{ TKAZip }
constructor TKAZip.Create(AOwner: TComponent);
begin
 Inherited Create(AOwner);
 FZipStream          := Nil;
 FOnDecompressFile   := Nil;
 FOnCompressFile     := Nil;
 FOnZipChange        := Nil;
 FOnZipOpen          := Nil;
 FOnAddItem          := Nil;
 FOnOverwriteFile    := Nil;
 FComponentVersion   := '2.0';
 FBatchMode          := False;
 FFileNames          := TALStringList.Create;
 FZipHeader          := TKAZipEntries.Create(Self);
 FZipComment         := TALStringList.Create;
 FIsZipFile          := False;
 FFileName           := '';
 FCurrentDFS         := 0;
 FExternalStream     := False;
 FIsDirty            := True;
 FHasBadEntries      := False;
 FReadOnly           := False;

 FApplyAttributes     := True;
 FOverwriteAction    := oaSkip;
 FZipSaveMethod      := FastSave;
 FUseTempFiles       := False;
 FStoreRelativePath  := True;
 FStoreFolders       := True;
 FZipCompressionType := ctMaximum;
end;


destructor TKAZip.Destroy;
begin
  if Assigned(FZipStream) AND (NOT FExternalStream) Then FZipStream.Free;
  FZipHeader.Free;
  FZipComment.Free;
  FFileNames.Free;
  inherited Destroy;
end;

procedure TKAZip.DoChange(Sender: TObject; Const ChangeType : Integer);
begin
  if Assigned(FOnZipChange) Then FOnZipChange(Self, ChangeType);
end;


function TKAZip.GetFileName(S: AnsiString): AnsiString;
Var
 FN : AnsiString;
 P  : Integer;
begin
 FN := S;
 FN := ALStringReplace(FN,'//','\',[rfReplaceAll]);
 FN := ALStringReplace(FN,'/','\',[rfReplaceAll]);
 P := ALPos(':\',FN);
 if P > 0 Then System.Delete(FN,1,P+1);
 Result := ALExtractFileName(ALStringReplace(FN,'/','\',[rfReplaceAll]));
end;

function TKAZip.GetFilePath(S: AnsiString): AnsiString;
Var
 FN : AnsiString;
 P  : Integer;
begin
 FN := S;
 FN := ALStringReplace(FN,'//','\',[rfReplaceAll]);
 FN := ALStringReplace(FN,'/','\',[rfReplaceAll]);
 P := ALPos(':\',FN);
 if P > 0 Then System.Delete(FN,1,P+1);
 Result := ALExtractFilePath(ALStringReplace(FN,'/','\',[rfReplaceAll]));
end;


procedure TKAZip.LoadFromFile(FileName: String);
Var
   Res : Integer;
   Dir : TSearchRec;
begin
   Res := FindFirst(FileName,faAnyFile,Dir);
   If Res=0 Then
      Begin
         if Dir.Attr And faReadOnly > 0 Then
            Begin
              FZipStream := TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone);
              FReadOnly  := True;
            End
         Else
            Begin
              FZipStream := TFileStream.Create(FileName,fmOpenReadWrite or fmShareDenyNone);
              FReadOnly  := False;
            End;
         LoadFromStream(FZipStream, False);
      End
   Else
      Begin
        Raise Exception.Create('File "'+FileName+'" not found!');
      End;
end;

procedure TKAZip.LoadFromStream(MS : TStream; AExternalStream: Boolean);
begin
  FZipStream := MS;
  FExternalStream := AExternalStream;
  FZipHeader.ParseZip(MS);
  FIsZipFile := FZipHeader.FIsZipFile;
  if Not FIsZipFile Then Close;
  FIsDirty := True;
  DoChange(Self,1);
end;

procedure TKAZip.Close;
begin
  Entries.Clear;
  if Assigned(FZipStream) AND (NOT FExternalStream) Then FZipStream.Free;
  FExternalStream := False;
  FZipStream      := Nil;
  FIsZipFile      := False;
  FIsDirty        := True;
  FReadOnly       := False;
  DoChange(Self,0);
end;

procedure TKAZip.SetFileName(const Value: String);
begin
  FFileName := Value;
end;

procedure TKAZip.Open(FileName: String);
begin
  Close;
  LoadFromFile(FileName);
  FFileName := FileName;
end;

procedure TKAZip.Open(MS: TStream);
begin
  Close;
  LoadFromStream(MS, True);
end;

procedure TKAZip.SetIsZipFile(const Value: Boolean);
begin
  //****************************************************************************
end;

function TKAZip.GetDelphiTempFileName: String;
Var
 TmpDir : Array[0..1000] of Char;
 TmpFN  : Array[0..1000] of Char;
Begin
 Result := GetCurrentDir;
 if GetTempPath(1000,TmpDir) <> 0 Then
    Begin
     if GetTempFileName(TmpDir,'',0,TmpFN) <> 0 Then Result := StrPas(TmpFN);
    End;
End;

procedure TKAZip.OnDecompress(Sender: TObject);
Var
  DS : TStream;
begin
  DS := TStream(Sender);
  if Assigned(FOnDecompressFile) Then FOnDecompressFile(Self,DS.Position,FCurrentDFS);
end;

procedure TKAZip.OnCompress(Sender: TObject);
Var
  CS : TStream;
begin
  CS := TStream(Sender);
  if Assigned(FOnCompressFile) Then FOnCompressFile(Self,CS.Position,FCurrentDFS);
end;

procedure TKAZip.ExtractToFile(Item : TKAZipEntriesEntry; FileName: String);
begin
  Entries.ExtractToFile(Item,FileName);
end;

procedure TKAZip.ExtractToFile(ItemIndex: Integer; FileName: String);
begin
  Entries.ExtractToFile(ItemIndex,FileName);
end;

procedure TKAZip.ExtractToFile(FileName: AnsiString; DestinationFileName: string);
begin
  Entries.ExtractToFile(FileName,DestinationFileName);
end;

procedure TKAZip.ExtractToStream(Item : TKAZipEntriesEntry; Stream: TStream);
begin
  Entries.ExtractToStream(Item,Stream);
end;

procedure TKAZip.ExtractAll(TargetDirectory: String);
begin
  Entries.ExtractAll(TargetDirectory);
end;

procedure TKAZip.ExtractSelected(TargetDirectory: String);
Begin
  Entries.ExtractSelected(TargetDirectory);
End;

function TKAZip.AddFile(FileName: string; NewFileName: AnsiString): TKAZipEntriesEntry;
begin
  Result := Entries.AddFile(FileName, NewFileName);
end;

function TKAZip.AddFile(FileName: String): TKAZipEntriesEntry;
begin
  Result := Entries.AddFile(FileName);
end;

function TKAZip.AddFiles(FileNames: TStrings): Boolean;
begin
  Result := Entries.AddFiles(FileNames);
end;

function TKAZip.AddFolder(FolderName, RootFolder, WildCard: String;
  WithSubFolders: Boolean): Boolean;
begin
  Result := Entries.AddFolder(FolderName,RootFolder,WildCard,WithSubFolders);
end;

function TKAZip.AddFilesAndFolders(FileNames: TStrings; RootFolder: String;
  WithSubFolders: Boolean): Boolean;
begin
  Result := Entries.AddFilesAndFolders(FileNames,RootFolder,WithSubFolders);
end;

function TKAZip.AddStream(FileName: AnsiString; FileAttr: Word;  FileDate: TDateTime; Stream: TStream): TKAZipEntriesEntry;
begin
  Result := Entries.AddStream(FileName,FileAttr,FileDate,Stream);
end;

function TKAZip.AddStream(FileName: AnsiString;  Stream: TStream): TKAZipEntriesEntry;
begin
  Result := Entries.AddStream(FileName,Stream);
end;


procedure TKAZip.Remove(Item: TKAZipEntriesEntry);
begin
  Entries.Remove(Item);
end;

procedure TKAZip.Remove(ItemIndex: Integer);
begin
  Entries.Remove(ItemIndex);
end;

procedure TKAZip.Remove(FileName: AnsiString);
begin
  Entries.Remove(FileName);
end;

procedure TKAZip.RemoveFiles(List: TList);
begin
  Entries.RemoveFiles(List);
end;

procedure TKAZip.RemoveSelected;
begin
  Entries.RemoveSelected;;
end;

function TKAZip.GetComment: TALStrings;
Var
 S : AnsiString;
begin
  Result := FZipComment;
  FZipComment.Clear;
  if FIsZipFile Then
     Begin
       if FEndOfCentralDir.ZipfileCommentLength > 0 Then
          Begin
            FZipStream.Position := FZipCommentPos;
            SetLength(S,FEndOfCentralDir.ZipfileCommentLength);
            FZipStream.Read(S[1],FEndOfCentralDir.ZipfileCommentLength);
            FZipComment.Text    := S;
          End;
     End;
end;

procedure TKAZip.SetComment(const Value: TALStrings);
Var
  Comment : AnsiString;
  L       : Integer;
begin
  //****************************************************************************
  if FZipComment.Text=Value.Text Then Exit;
  FZipComment.Clear;
  if FIsZipFile Then
     Begin
       FZipComment.Assign(Value);
       Comment                               := FZipComment.Text;
       L                                     := Length(Comment);
       FEndOfCentralDir.ZipfileCommentLength := L;
       FZipStream.Position                   := FEndOfCentralDirPos;
       FZipStream.Write(FEndOfCentralDir,SizeOf(TEndOfCentralDir));
       FZipCommentPos                        := FZipStream.Position;
       if L > 0 Then
          Begin
            FZipStream.Write(Comment[1],L)
          End
       Else
          Begin
            FZipStream.Size := FZipStream.Position;
          End;
     End;
end;

procedure TKAZip.DeSelectAll;
begin
  Entries.DeSelectAll;
end;

procedure TKAZip.Select(WildCard : AnsiString);
begin
  Entries.Select(WildCard);
end;

procedure TKAZip.InvertSelection;
begin
  Entries.InvertSelection;
end;

procedure TKAZip.SelectAll;
begin
  Entries.SelectAll;
end;

procedure TKAZip.RebuildLocalFiles(MS: TStream);
Var
  X    : Integer;
  LF   : TLocalFile;
begin
  //************************************************* RESAVE ALL LOCAL BLOCKS
  SetLength(NewLHOffsets,Entries.Count+1);
  For X := 0 To Entries.Count-1 do
      Begin
        NewLHOffsets[X] := MS.Position;
        LF  := Entries.GetLocalEntry(FZipStream,Entries.Items[X].LocalOffset,False);
        MS.Write(LF, SizeOf(LF)-3*SizeOf(AnsiString));
        if LF.FilenameLength   > 0 Then MS.Write(LF.FileName[1]  ,LF.FilenameLength);
        if LF.ExtraFieldLength > 0 Then MS.Write(LF.ExtraField[1],LF.ExtraFieldLength);
        if LF.CompressedSize   > 0 Then MS.Write(LF.CompressedData[1],LF.CompressedSize);
        if Assigned(FOnRebuildZip) Then FOnRebuildZip(Self,X,Entries.Count-1);
      End;
end;

procedure TKAZip.RebuildCentralDirectory(MS: TStream);
Var
  X    : Integer;
  CDF  : TCentralDirectoryFile;
begin
  NewEndOfCentralDir := FEndOfCentralDir;
  NewEndOfCentralDir.TotalNumberOfEntriesOnThisDisk  := Entries.Count;
  NewEndOfCentralDir.TotalNumberOfEntries            := Entries.Count;
  NewEndOfCentralDir.OffsetOfStartOfCentralDirectory := MS.Position;
  For X := 0 To Entries.Count-1 do
      Begin
        CDF := Entries.Items[X].FCentralDirectoryFile;
        CDF.RelativeOffsetOfLocalHeader := NewLHOffsets[X];
        MS.Write(CDF,SizeOf(CDF)-3*SizeOf(AnsiString));
        if CDF.FilenameLength > 0 Then
           MS.Write(CDF.FileName[1],CDF.FilenameLength);
        if CDF.ExtraFieldLength > 0 Then
           MS.Write(CDF.ExtraField[1],CDF.ExtraFieldLength);
        if CDF.FileCommentLength > 0 Then
           MS.Write(CDF.FileComment[1],CDF.FileCommentLength);
        if Assigned(FOnRebuildZip) Then FOnRebuildZip(Self,X,Entries.Count-1);
      End;
  NewEndOfCentralDir.SizeOfTheCentralDirectory := MS.Position-NewEndOfCentralDir.OffsetOfStartOfCentralDirectory;
end;

procedure TKAZip.RebuildEndOfCentralDirectory(MS: TStream);
Var
  ZipComment : AnsiString;
begin
  ZipComment   := Comment.Text;
  FRebuildECDP := MS.Position;
  MS.Write(NewEndOfCentralDir,SizeOf(NewEndOfCentralDir));
  FRebuildCP   := MS.Position;
  if NewEndOfCentralDir.ZipfileCommentLength > 0 Then
     Begin
       MS.Write(ZipComment[1],NewEndOfCentralDir.ZipfileCommentLength);
     End;
  if Assigned(FOnRebuildZip) Then FOnRebuildZip(Self,100,100);
end;

Procedure TKAZip.FixZip(MS : TStream);
Var
  X          : Integer;
  Y          : Integer;
  NewCount   : Integer;
  LF         : TLocalFile;
  CDF        : TCentralDirectoryFile;
  ZipComment : AnsiString;
Begin
  ZipComment   := Comment.Text;
  Y            := 0;
  SetLength(NewLHOffsets,Entries.Count+1);
  For X := 0 To Entries.Count-1 do
      Begin
        LF  := Entries.GetLocalEntry(FZipStream,Entries.Items[X].LocalOffset,False);
        if (LF.LocalFileHeaderSignature=$04034b50) And (Entries.Items[X].Test) Then
           Begin
             NewLHOffsets[Y] := MS.Position;
             MS.Write(LF, SizeOf(LF)-3*SizeOf(AnsiString));
             if LF.FilenameLength   > 0 Then MS.Write(LF.FileName[1]  ,LF.FilenameLength);
             if LF.ExtraFieldLength > 0 Then MS.Write(LF.ExtraField[1],LF.ExtraFieldLength);
             if LF.CompressedSize   > 0 Then MS.Write(LF.CompressedData[1],LF.CompressedSize);
             if Assigned(FOnRebuildZip) Then FOnRebuildZip(Self,X,Entries.Count-1);
             Inc(Y);
           End
        Else
           Begin
             Entries.Items[X].FCentralDirectoryFile.CentralFileHeaderSignature := 0;
             if Assigned(FOnRebuildZip) Then FOnRebuildZip(Self,X,Entries.Count-1);
           End;
      End;

  NewCount := Y;
  Y        := 0;
  NewEndOfCentralDir := FEndOfCentralDir;
  NewEndOfCentralDir.TotalNumberOfEntriesOnThisDisk  := NewCount;
  NewEndOfCentralDir.TotalNumberOfEntries            := NewCount;
  NewEndOfCentralDir.OffsetOfStartOfCentralDirectory := MS.Position;
  For X := 0 To Entries.Count-1 do
      Begin
        CDF := Entries.Items[X].FCentralDirectoryFile;
        if CDF.CentralFileHeaderSignature=$02014b50 Then
           Begin
             CDF.RelativeOffsetOfLocalHeader := NewLHOffsets[Y];
             MS.Write(CDF,SizeOf(CDF)-3*SizeOf(AnsiString));
             if CDF.FilenameLength > 0 Then
                MS.Write(CDF.FileName[1],CDF.FilenameLength);
             if CDF.ExtraFieldLength > 0 Then
                MS.Write(CDF.ExtraField[1],CDF.ExtraFieldLength);
             if CDF.FileCommentLength > 0 Then
                MS.Write(CDF.FileComment[1],CDF.FileCommentLength);
             if Assigned(FOnRebuildZip) Then FOnRebuildZip(Self,X,Entries.Count-1);
             Inc(Y);
           End;
      End;
  NewEndOfCentralDir.SizeOfTheCentralDirectory := MS.Position-NewEndOfCentralDir.OffsetOfStartOfCentralDirectory;

  FRebuildECDP := MS.Position;
  MS.Write(NewEndOfCentralDir,SizeOf(NewEndOfCentralDir));
  FRebuildCP   := MS.Position;
  if NewEndOfCentralDir.ZipfileCommentLength > 0 Then
     Begin
       MS.Write(ZipComment[1],NewEndOfCentralDir.ZipfileCommentLength);
     End;
  if Assigned(FOnRebuildZip) Then FOnRebuildZip(Self,100,100);
End;


Procedure TKAZip.SaveToStream(Stream:TStream);
Begin
  RebuildLocalFiles(Stream);
  RebuildCentralDirectory(Stream);
  RebuildEndOfCentralDirectory(Stream);
End;

Procedure TKAZip.Rebuild;
var
  TempStream          : TFileStream;
  TempMSStream        : TMemoryStream;
  TempFileName        : String;
Begin
  if FUseTempFiles Then
     Begin
        TempFileName := GetDelphiTempFileName;
        TempStream   := TFileStream.Create(TempFileName,fmOpenReadWrite or FmCreate);
        Try
          SaveToStream(TempStream);
          FZipStream.Position := 0;
          FZipStream.Size     := 0;
          TempStream.Position := 0;
          FZipStream.CopyFrom(TempStream,TempStream.Size);
          Entries.ParseZip(FZipStream);
        Finally
          TempStream.Free;
          DeleteFile(TempFileName)
        End;
     End
  Else
     Begin
        TempMSStream   := TMemoryStream.Create;
        Try
          SaveToStream(TempMSStream);
          FZipStream.Position   := 0;
          FZipStream.Size       := 0;
          TempMSStream.Position := 0;
          FZipStream.CopyFrom(TempMSStream,TempMSStream.Size);
          Entries.ParseZip(FZipStream);
        Finally
          TempMSStream.Free;
        End;
     End;
  FIsDirty := True;   
End;

Procedure TKAZip.CreateZip(Stream:TStream);
Var
  ECD : TEndOfCentralDir;
Begin
  ECD.EndOfCentralDirSignature        := $06054b50;
  ECD.NumberOfThisDisk                := 0;
  ECD.NumberOfTheDiskWithTheStart     := 0;
  ECD.TotalNumberOfEntriesOnThisDisk  := 0;
  ECD.TotalNumberOfEntries            := 0;
  ECD.SizeOfTheCentralDirectory       := 0;
  ECD.OffsetOfStartOfCentralDirectory := 0;
  ECD.ZipfileCommentLength            := 0;
  Stream.Write(ECD,SizeOf(ECD));
End;


Procedure TKAZip.CreateZip(FileName:String);
var
  FS : TFileStream;
Begin
  FS := TFileStream.Create(FileName,fmOpenReadWrite or FmCreate);
  Try
    CreateZip(FS);
  Finally
    FS.Free;
  End;
End;

procedure TKAZip.SetZipSaveMethod(const Value: TZipSaveMethod);
begin
  FZipSaveMethod := Value;
end;

procedure TKAZip.SetActive(const Value: Boolean);
begin
  if FFileName='' Then Exit;
  if Value Then Open(FFileName) Else Close;
end;

procedure TKAZip.SetZipCompressionType(const Value: TZipCompressionType);
begin
  FZipCompressionType := Value;
  if FZipCompressionType = ctUnknown Then FZipCompressionType := ctNormal;
end;

function TKAZip.GetFileNames: TALStrings;
Var
  X : Integer;
begin
  if FIsDirty Then
     Begin
       FFileNames.Clear;
       For X := 0 To Entries.Count-1 do
           Begin
             FFileNames.Add(GetFilePath(Entries.Items[X].FileName)+GetFileName(Entries.Items[X].FileName));
           End;
       FIsDirty := False;
     End;
  Result := FFileNames;
end;

procedure TKAZip.SetFileNames(const Value: TALStrings);
begin
  //*************************************************** READ ONLY
end;


procedure TKAZip.SetUseTempFiles(const Value: Boolean);
begin
  FUseTempFiles := Value;
end;

procedure TKAZip.Rename(Item: TKAZipEntriesEntry; NewFileName: AnsiString);
begin
  Entries.Rename(Item,NewFileName);
end;

procedure TKAZip.Rename(ItemIndex: Integer; NewFileName: AnsiString);
begin
  Entries.Rename(ItemIndex,NewFileName);
end;

procedure TKAZip.Rename(FileName, NewFileName: AnsiString);
begin
  Entries.Rename(FileName, NewFileName);
end;

procedure TKAZip.RenameMultiple(Names, NewNames: TALStringList);
begin
  Entries.RenameMultiple(Names, NewNames);
end;


procedure TKAZip.SetStoreFolders(const Value: Boolean);
begin
  FStoreFolders := Value;
end;

procedure TKAZip.SetOnAddItem(const Value: TOnAddItem);
begin
  FOnAddItem := Value;
end;

procedure TKAZip.SetComponentVersion(const Value: AnsiString);
begin
  //****************************************************************************
end;

procedure TKAZip.SetOnRebuildZip(const Value: TOnRebuildZip);
begin
  FOnRebuildZip := Value;
end;

procedure TKAZip.SetOnRemoveItems(const Value: TOnRemoveItems);
begin
  FOnRemoveItems := Value;
end;

procedure TKAZip.SetOverwriteAction(const Value: TOverwriteAction);
begin
  FOverwriteAction := Value;
end;

procedure TKAZip.SetOnOverwriteFile(const Value: TOnOverwriteFile);
begin
  FOnOverwriteFile := Value;
end;

procedure TKAZip.CreateFolder(FolderName: AnsiString; FolderDate: TDateTime);
begin
  Entries.CreateFolder(FolderName,FolderDate);
end;

procedure TKAZip.RenameFolder(FolderName : AnsiString; NewFolderName : AnsiString);
begin
  Entries.RenameFolder(FolderName,NewFolderName);
end;

procedure TKAZip.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TKAZip.SetApplyAtributes(const Value: Boolean);
begin
  FApplyAttributes := Value;
end;

{$WARN SYMBOL_PLATFORM ON}

end.




