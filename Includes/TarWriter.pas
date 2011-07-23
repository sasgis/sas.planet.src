UNIT TarWriter;

INTERFACE

USES
   Windows, SysUtils, Classes;

TYPE
  // --- File Access Permissions
  TTarPermission  = (tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                     tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                     tpReadByOther, tpWriteByOther, tpExecuteByOther);
  TTarPermissions = SET OF TTarPermission;

  // --- Type of File
  TFileType = (ftNormal,          // Regular file
               ftLink,            // Link to another, previously archived, file (LinkName)
               ftSymbolicLink,    // Symbolic link to another file              (LinkName)
               ftCharacter,       // Character special files
               ftBlock,           // Block special files
               ftDirectory,       // Directory entry. Size is zero (unlimited) or max. number of bytes
               ftFifo,            // FIFO special file. No data stored in the archive.
               ftContiguous,      // Contiguous file, if supported by OS
               ftDumpDir,         // List of files
               ftMultiVolume,     // Multi-volume file part
               ftVolumeHeader,    // Volume header. Can appear only as first record in the archive
               ftLongNameLink);   // Long name

  // --- Mode
  TTarMode  = (tmSetUid, tmSetGid, tmSaveText);
  TTarModes = SET OF TTarMode;

  // --- Record for a Directory Entry
  //     Adjust the ClearDirRec procedure when this record changes!
  TTarDirRec  = RECORD
                  Name        : STRING;            // File path and name
                  Size        : INT64;             // File size in Bytes
                  DateTime    : TDateTime;         // Last modification date and time
                  Permissions : TTarPermissions;   // Access permissions
                  FileType    : TFileType;         // Type of file
                  LinkName    : STRING;            // Name of linked file (for ftLink, ftSymbolicLink)
                  UID         : INTEGER;           // User ID
                  GID         : INTEGER;           // Group ID
                  UserName    : STRING;            // User name
                  GroupName   : STRING;            // Group name
                  ChecksumOK  : BOOLEAN;           // Checksum was OK
                  Mode        : TTarModes;         // Mode
                  Magic       : STRING;            // Contents of the "Magic" field
                  MajorDevNo  : INTEGER;           // Major Device No. for ftCharacter and ftBlock
                  MinorDevNo  : INTEGER;           // Minor Device No. for ftCharacter and ftBlock
                  FilePos     : INT64;             // Position in TAR file
                END;

   // --- The TAR Archive Writer CLASS
  TTarWriter = CLASS
               PROTECTED
                 FStream      : TStream;
                 FOwnsStream  : BOOLEAN;
                 FFinalized   : BOOLEAN;
                 FPermissions : TTarPermissions;   // Access permissions
                 FUID         : INTEGER;           // User ID
                 FGID         : INTEGER;           // Group ID
                 FUserName    : STRING;            // User name
                 FGroupName   : STRING;            // Group name
                 FMode        : TTarModes;         // Mode
                 FMagic       : STRING;            // Contents of the "Magic" field
                 CONSTRUCTOR CreateEmpty;
               PUBLIC
                 CONSTRUCTOR Create (TargetFilename : STRING; Mode : INTEGER = fmCreate);  OVERLOAD;
                 DESTRUCTOR Destroy; OVERRIDE;                   // Writes End-Of-File Tag
                 PROCEDURE AddStream (Stream   : TStream; TarFilename : STRING; FileDateGmt : TDateTime);
                 PROCEDURE AddLongLink (TarFilename: STRING);
                 PROCEDURE Finalize;
                 PROPERTY Permissions : TTarPermissions READ FPermissions WRITE FPermissions;   // Access permissions
                 PROPERTY UID         : INTEGER         READ FUID         WRITE FUID;           // User ID
                 PROPERTY GID         : INTEGER         READ FGID         WRITE FGID;           // Group ID
                 PROPERTY UserName    : STRING          READ FUserName    WRITE FUserName;      // User name
                 PROPERTY GroupName   : STRING          READ FGroupName   WRITE FGroupName;     // Group name
                 PROPERTY Mode        : TTarModes       READ FMode        WRITE FMode;          // Mode
                 PROPERTY Magic       : STRING          READ FMagic       WRITE FMagic;         // Contents of the "Magic" field
               END;

// --- Some useful constants
CONST
  FILETYPE_NAME : ARRAY [TFileType] OF STRING =
                  ('Regular', 'Link', 'Symbolic Link', 'Char File', 'Block File',
                   'Directory', 'FIFO File', 'Contiguous', 'Dir Dump', 'Multivol', 'Volume Header', 'Long Name');

  ALL_PERMISSIONS     = [tpReadByOwner, tpWriteByOwner, tpExecuteByOwner,
                         tpReadByGroup, tpWriteByGroup, tpExecuteByGroup,
                         tpReadByOther, tpWriteByOther, tpExecuteByOther];
  READ_PERMISSIONS    = [tpReadByOwner, tpReadByGroup,  tpReadByOther];
  WRITE_PERMISSIONS   = [tpWriteByOwner, tpWriteByGroup, tpWriteByOther];
  EXECUTE_PERMISSIONS = [tpExecuteByOwner, tpExecuteByGroup, tpExecuteByOther];


FUNCTION  PermissionString      (Permissions : TTarPermissions) : STRING;
FUNCTION  ConvertFilename       (Filename    : STRING)          : STRING;
PROCEDURE ClearDirRec           (VAR DirRec  : TTarDirRec);


(*
===============================================================================================
IMPLEMENTATION
===============================================================================================
*)

IMPLEMENTATION

FUNCTION PermissionString (Permissions : TTarPermissions) : STRING;
BEGIN
  Result := '';
  IF tpReadByOwner    IN Permissions THEN Result := Result + 'r' ELSE Result := Result + '-';
  IF tpWriteByOwner   IN Permissions THEN Result := Result + 'w' ELSE Result := Result + '-';
  IF tpExecuteByOwner IN Permissions THEN Result := Result + 'x' ELSE Result := Result + '-';
  IF tpReadByGroup    IN Permissions THEN Result := Result + 'r' ELSE Result := Result + '-';
  IF tpWriteByGroup   IN Permissions THEN Result := Result + 'w' ELSE Result := Result + '-';
  IF tpExecuteByGroup IN Permissions THEN Result := Result + 'x' ELSE Result := Result + '-';
  IF tpReadByOther    IN Permissions THEN Result := Result + 'r' ELSE Result := Result + '-';
  IF tpWriteByOther   IN Permissions THEN Result := Result + 'w' ELSE Result := Result + '-';
  IF tpExecuteByOther IN Permissions THEN Result := Result + 'x' ELSE Result := Result + '-';
END;


FUNCTION ConvertFilename  (Filename : STRING) : STRING;
BEGIN
  Result := StringReplace (Filename, '\', '/', [rfReplaceAll]);
END;

PROCEDURE ClearDirRec (VAR DirRec : TTarDirRec);
BEGIN
  WITH DirRec DO BEGIN
    Name        := '';
    Size        := 0;
    DateTime    := 0.0;
    Permissions := [];
    FileType    := TFileType (0);
    LinkName    := '';
    UID         := 0;
    GID         := 0;
    UserName    := '';
    GroupName   := '';
    ChecksumOK  := FALSE;
    Mode        := [];
    Magic       := '';
    MajorDevNo  := 0;
    MinorDevNo  := 0;
    FilePos     := 0;
  END;
END;

(*
===============================================================================================
TAR format
===============================================================================================
*)

CONST
  RECORDSIZE = 512;
  NAMSIZ     = 100;
  TUNMLEN    =  32;
  TGNMLEN    =  32;
  CHKBLANKS  = #32#32#32#32#32#32#32#32;

TYPE
  TTarHeader = PACKED RECORD
                 Name     : ARRAY [0..NAMSIZ-1] OF CHAR;
                 Mode     : ARRAY [0..7] OF CHAR;
                 UID      : ARRAY [0..7] OF CHAR;
                 GID      : ARRAY [0..7] OF CHAR;
                 Size     : ARRAY [0..11] OF CHAR;
                 MTime    : ARRAY [0..11] OF CHAR;
                 ChkSum   : ARRAY [0..7] OF CHAR;
                 LinkFlag : CHAR;
                 LinkName : ARRAY [0..NAMSIZ-1] OF CHAR;
                 Magic    : ARRAY [0..7] OF CHAR;
                 UName    : ARRAY [0..TUNMLEN-1] OF CHAR;
                 GName    : ARRAY [0..TGNMLEN-1] OF CHAR;
                 DevMajor : ARRAY [0..7] OF CHAR;
                 DevMinor : ARRAY [0..7] OF CHAR;
               END;

PROCEDURE Octal (N : INTEGER; P : PChar; Len : INTEGER);
VAR
  I     : INTEGER;
BEGIN
  FOR I := Len-2 DOWNTO 0 DO BEGIN
    (P+I)^ := CHR (ORD ('0') + ORD (N AND $07));
    N := N SHR 3;
    END;
  FOR I := 0 TO Len-3 DO
    IF (P+I)^ = '0'
      THEN (P+I)^ := #32
      ELSE BREAK;
  (P+Len-1)^ := #32;
END;


PROCEDURE Octal64 (N : INT64; P : PChar; Len : INTEGER);
VAR
  I     : INTEGER;
BEGIN
  FOR I := Len-2 DOWNTO 0 DO BEGIN
    (P+I)^ := CHR (ORD ('0') + ORD (N AND $07));
    N := N SHR 3;
    END;
  FOR I := 0 TO Len-3 DO
    IF (P+I)^ = '0'
      THEN (P+I)^ := #32
      ELSE BREAK;
  (P+Len-1)^ := #32;
END;


PROCEDURE OctalN (N : INTEGER; P : PChar; Len : INTEGER);
BEGIN
  Octal (N, P, Len-1);
  (P+Len-1)^ := #0;
END;


PROCEDURE WriteTarHeader (Dest : TStream; DirRec : TTarDirRec);
VAR
  Rec      : ARRAY [0..RECORDSIZE-1] OF CHAR;
  TH       : TTarHeader ABSOLUTE Rec;
  Mode     : INTEGER;
  NullDate : TDateTime;
  Checksum : CARDINAL;
  I        : INTEGER;
BEGIN
  FillChar (Rec, RECORDSIZE, 0);
  StrLCopy (TH.Name, PChar (DirRec.Name), NAMSIZ);
  Mode := 0;
  IF tmSaveText IN DirRec.Mode THEN Mode := Mode OR $0200;
  IF tmSetGid   IN DirRec.Mode THEN Mode := Mode OR $0400;
  IF tmSetUid   IN DirRec.Mode THEN Mode := Mode OR $0800;
  IF tpReadByOwner    IN DirRec.Permissions THEN Mode := Mode OR $0100;
  IF tpWriteByOwner   IN DirRec.Permissions THEN Mode := Mode OR $0080;
  IF tpExecuteByOwner IN DirRec.Permissions THEN Mode := Mode OR $0040;
  IF tpReadByGroup    IN DirRec.Permissions THEN Mode := Mode OR $0020;
  IF tpWriteByGroup   IN DirRec.Permissions THEN Mode := Mode OR $0010;
  IF tpExecuteByGroup IN DirRec.Permissions THEN Mode := Mode OR $0008;
  IF tpReadByOther    IN DirRec.Permissions THEN Mode := Mode OR $0004;
  IF tpWriteByOther   IN DirRec.Permissions THEN Mode := Mode OR $0002;
  IF tpExecuteByOther IN DirRec.Permissions THEN Mode := Mode OR $0001;
  OctalN (Mode, @TH.Mode, 8);
  OctalN (DirRec.UID, @TH.UID, 8);
  OctalN (DirRec.GID, @TH.GID, 8);
  Octal64 (DirRec.Size, @TH.Size, 12);
  NullDate := EncodeDate (1970, 1, 1);
  IF DirRec.DateTime >= NullDate
    THEN Octal (Trunc ((DirRec.DateTime - NullDate) * 86400.0), @TH.MTime, 12)
    ELSE Octal (Trunc (                   NullDate  * 86400.0), @TH.MTime, 12);
  CASE DirRec.FileType OF
    ftNormal       : TH.LinkFlag := '0';
    ftLink         : TH.LinkFlag := '1';
    ftSymbolicLink : TH.LinkFlag := '2';
    ftCharacter    : TH.LinkFlag := '3';
    ftBlock        : TH.LinkFlag := '4';
    ftDirectory    : TH.LinkFlag := '5';
    ftFifo         : TH.LinkFlag := '6';
    ftContiguous   : TH.LinkFlag := '7';
    ftDumpDir      : TH.LinkFlag := 'D';
    ftMultiVolume  : TH.LinkFlag := 'M';
    ftVolumeHeader : TH.LinkFlag := 'V';
    ftLongNameLink : TH.LinkFlag := 'L';
  END;
  StrLCopy (TH.LinkName, PChar (DirRec.LinkName), NAMSIZ);
  StrLCopy (TH.Magic, PChar (DirRec.Magic + #32#32#32#32#32#32#32#32), 8);
  StrLCopy (TH.UName, PChar (DirRec.UserName), TUNMLEN);
  StrLCopy (TH.GName, PChar (DirRec.GroupName), TGNMLEN);
  OctalN (DirRec.MajorDevNo, @TH.DevMajor, 8);
  OctalN (DirRec.MinorDevNo, @TH.DevMinor, 8);
  StrMove (TH.ChkSum, CHKBLANKS, 8);

  CheckSum := 0;
  FOR I := 0 TO SizeOf (TTarHeader)-1 DO
    INC (CheckSum, INTEGER (ORD (Rec [I])));
  OctalN (CheckSum, @TH.ChkSum, 8);

  Dest.Write (TH, RECORDSIZE);
END;




(*
===============================================================================================
TTarWriter
===============================================================================================
*)


CONSTRUCTOR TTarWriter.CreateEmpty;
VAR
  TP : TTarPermission;
BEGIN
  INHERITED Create;
  FOwnsStream  := FALSE;
  FFinalized   := FALSE;
  FPermissions := [];
  FOR TP := Low (TP) TO High (TP) DO
    Include (FPermissions, TP);
  FUID       := 0;
  FGID       := 0;
  FUserName  := '';
  FGroupName := '';
  FMode      := [];
  FMagic     := 'ustar';
END;

CONSTRUCTOR TTarWriter.Create (TargetFilename : STRING; Mode : INTEGER = fmCreate);
BEGIN
  CreateEmpty;
  FStream     := TFileStream.Create (TargetFilename, Mode);
  FOwnsStream := TRUE;
  // go to end of archive (last 512 byte - is ID EOF, so erise it)
  if FStream.Size > RECORDSIZE then FStream.Position := FStream.Size - RECORDSIZE;
END;

DESTRUCTOR TTarWriter.Destroy;
BEGIN
  IF NOT FFinalized THEN BEGIN
    Finalize;
    FFinalized := TRUE;
  END;
  IF FOwnsStream THEN FStream.Free;
  INHERITED Destroy;
END;

PROCEDURE TTarWriter.AddStream (Stream : TStream; TarFilename : STRING; FileDateGmt : TDateTime);
VAR
  DirRec      : TTarDirRec;
  Rec         : ARRAY [0..RECORDSIZE-1] OF CHAR;
  BytesToRead : INT64;      // Bytes to read from the Source Stream
  BlockSize   : INT64;      // Bytes to write out for the current record
BEGIN
  if Length(TarFilename) >= 100 then AddLongLink(TarFilename);
  // clear head rec
  ClearDirRec (DirRec);
  // initialize head rec by data
  DirRec.Name        := TarFilename;
  DirRec.Size        := Stream.Size - Stream.Position;
  DirRec.DateTime    := FileDateGmt;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftNormal;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;
  // write header
  WriteTarHeader (FStream, DirRec);
  // write body data if it size > 0
  BytesToRead := DirRec.Size;
  WHILE BytesToRead > 0 DO BEGIN
    BlockSize := BytesToRead;
    IF BlockSize > RECORDSIZE THEN BlockSize := RECORDSIZE;
    FillChar (Rec, RECORDSIZE, 0);
    Stream.Read (Rec, BlockSize);
    FStream.Write (Rec, RECORDSIZE); // write 512 byte to archive
    DEC (BytesToRead, BlockSize);
  END;
END;

PROCEDURE TTarWriter.AddLongLink (TarFilename: STRING);
VAR
  DirRec      : TTarDirRec;
  Rec         : ARRAY [0..RECORDSIZE-1] OF CHAR;
  S           : TStringStream;
  BytesToRead : INT64;
  BlockSize   : INT64;
BEGIN
  ClearDirRec (DirRec);
  DirRec.Name        := '././@LongLink';
  DirRec.Size        := Length(TarFilename);
  DirRec.DateTime    := Now;
  DirRec.Permissions := FPermissions;
  DirRec.FileType    := ftLongNameLink;
  DirRec.LinkName    := '';
  DirRec.UID         := FUID;
  DirRec.GID         := FGID;
  DirRec.UserName    := FUserName;
  DirRec.GroupName   := FGroupName;
  DirRec.ChecksumOK  := TRUE;
  DirRec.Mode        := FMode;
  DirRec.Magic       := FMagic;
  DirRec.MajorDevNo  := 0;
  DirRec.MinorDevNo  := 0;
  WriteTarHeader (FStream, DirRec);
  S := TStringStream.Create (TarFilename);
  TRY
    BytesToRead := DirRec.Size;
    WHILE BytesToRead > 0 DO BEGIN
      BlockSize := BytesToRead;
      IF BlockSize > RECORDSIZE THEN BlockSize := RECORDSIZE;
      FillChar (Rec, RECORDSIZE, 0);
      S.Read (Rec, BlockSize);
      FStream.Write (Rec, RECORDSIZE); // write 512 byte to archive
      DEC (BytesToRead, BlockSize);
    END;
  FINALLY
    S.Free
  END
END;

PROCEDURE TTarWriter.Finalize;
VAR
  Rec : ARRAY [0..RECORDSIZE-1] OF CHAR;
BEGIN
  FillChar (Rec, SizeOf (Rec), 0);
  FStream.Write (Rec, RECORDSIZE);
  FFinalized := TRUE;
END;

END.

