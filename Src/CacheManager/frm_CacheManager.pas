{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit frm_CacheManager;

interface

uses
  Windows,
  Messages,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  TB2Item,
  TB2Dock,
  TB2Toolbar,
  TBX,
  fr_CacheTypeList,
  i_NotifierOperation,
  i_ProjectionSet,
  i_ProjectionSetFactory,
  i_TileStorage,
  i_LanguageManager,
  i_NotifierTime,
  i_ContentTypeManager,
  i_ArchiveReadWriteFactory,
  i_TileFileNameGeneratorsList,
  i_TileFileNameParsersList,
  i_TileStorageTypeList,
  i_TileStorageAbilities,
  i_ValueToStringConverter,
  i_MapVersionFactory,
  i_MapVersionFactoryList,
  i_GlobalBerkeleyDBHelper,
  u_CommonFormAndFrameParents;

type
  TfrmCacheManager = class(TFormWitghLanguageManager)
    PageControl1: TPageControl;
    tsConverter: TTabSheet;
    pnlBottomButtons: TPanel;
    btnStart: TButton;
    btnCansel: TButton;
    grpSrc: TGroupBox;
    lblPath: TLabel;
    edtPath: TEdit;
    pnlCacheTypes: TPanel;
    lblCacheType: TLabel;
    chkIgnoreTNE: TCheckBox;
    chkRemove: TCheckBox;
    edtDefExtention: TEdit;
    lblDefExtention: TLabel;
    grpDestCache: TGroupBox;
    lblDestPath: TLabel;
    lblDestFormat: TLabel;
    edtDestPath: TEdit;
    pnlDestCacheTypes: TPanel;
    chkOverwrite: TCheckBox;
    btnSelectSrcPath: TButton;
    btnSelectDestPath: TButton;
    chkCheckSourceVersion: TCheckBox;
    edtSourceVersion: TEdit;
    chkReplaceDestVersion: TCheckBox;
    edtDestVersion: TEdit;
    TBXDontClose: TTBXToolbar;
    tbtmDontClose: TTBItem;
    procedure btnStartClick(Sender: TObject);
    procedure btnSelectSrcPathClick(Sender: TObject);
    procedure btnSelectDestPathClick(Sender: TObject);
    procedure btnCanselClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    type
      TArchiveType = (atNotArch, atTar, atZip);
  private
    FLanguageManager: ILanguageManager;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
    FGCNotifier: INotifierTime;
    FMapVersionFactory: IMapVersionFactory;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FContentTypeManager: IContentTypeManager;
    FProjectionSetFactory: IProjectionSetFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FFileNameGeneratorsList: ITileFileNameGeneratorsList;
    FFileNameParsersList: ITileFileNameParsersList;
    FValueToStringConverter: IValueToStringConverterChangeable;
    FfrSrcCacheTypesList: TfrCacheTypeList;
    FfrDestCacheTypesList: TfrCacheTypeList;
    procedure ProcessCacheConverter;
    function CreateSimpleTileStorage(
      const ARootPath: string;
      const ADefExtention: string;
      const AArchiveType: TArchiveType;
      const AStorageAbilities: ITileStorageAbilities;
      const AProjectionSet: IProjectionSet;
      const AFormatID: Byte
    ): ITileStorage;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AGCNotifier: INotifierTime;
      const AMapVersionFactoryList: IMapVersionFactoryList;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AContentTypeManager: IContentTypeManager;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const AFileNameGeneratorsList: ITileFileNameGeneratorsList;
      const AFileNameParsersList: ITileFileNameParsersList;
      const AValueToStringConverter: IValueToStringConverterChangeable
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  SysUtils,
  c_CacheTypeCodes,
  c_CoordConverter,
  i_ArchiveReadWrite,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_PathConfig,
  i_TileStorageType,
  i_TileStorageTypeConfig,
  i_TileFileNameGenerator,
  i_TileFileNameParser,
  i_TileStorageTypeListItem,
  i_CacheConverterProgressInfo,
  u_Notifier,
  u_NotifierOperation,
  u_PathConfig,
  u_ThreadCacheConverter,
  u_TileStorageArchive,
  u_TileStorageTypeArchive,
  u_TileStorageTypeConfig,
  u_TileStorageAbilities,
  u_CacheConverterProgressInfo,
  u_Synchronizer,
  u_StrFunc,
  frm_ProgressCacheConvrter;

{$R *.dfm}

{TfrmCacheManager}

constructor TfrmCacheManager.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AGCNotifier: INotifierTime;
  const AMapVersionFactoryList: IMapVersionFactoryList;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AContentTypeManager: IContentTypeManager;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const AFileNameGeneratorsList: ITileFileNameGeneratorsList;
  const AFileNameParsersList: ITileFileNameParsersList;
  const AValueToStringConverter: IValueToStringConverterChangeable
);
begin
  inherited Create(ALanguageManager);
  FLanguageManager := ALanguageManager;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FGCNotifier := AGCNotifier;
  FMapVersionFactory := AMapVersionFactoryList.GetSimpleVersionFactory;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
  FFileNameGeneratorsList := AFileNameGeneratorsList;
  FFileNameParsersList := AFileNameParsersList;
  FContentTypeManager := AContentTypeManager;
  FProjectionSetFactory := AProjectionSetFactory;
  FValueToStringConverter := AValueToStringConverter;
  FTileStorageTypeList := ATileStorageTypeList;

  FfrSrcCacheTypesList :=
    TfrCacheTypeList.Create(
      ALanguageManager,
      ATileStorageTypeList,
      False,
      CTileStorageTypeClassAll - [tstcInMemory],
      [tsacScan]
    );
  FfrDestCacheTypesList :=
    TfrCacheTypeList.Create(
      ALanguageManager,
      ATileStorageTypeList,
      False,
      CTileStorageTypeClassAll - [tstcInMemory],
      [tsacAdd]
    );
end;

destructor TfrmCacheManager.Destroy;
begin
  FreeAndNil(FfrSrcCacheTypesList);
  FreeAndNil(FfrDestCacheTypesList);
  inherited;
end;

procedure TfrmCacheManager.FormShow(Sender: TObject);
begin
  FfrSrcCacheTypesList.Show(pnlCacheTypes);
  FfrDestCacheTypesList.Show(pnlDestCacheTypes);
  FfrSrcCacheTypesList.IntCode := c_File_Cache_Id_SAS;
  FfrDestCacheTypesList.IntCode := c_File_Cache_Id_SQLite;
end;

function IsFileSystemStorage(const AFormatID: Byte): Boolean; inline;
begin
  Result :=
    AFormatID in [
      c_File_Cache_Id_GMV,
      c_File_Cache_Id_SAS,
      c_File_Cache_Id_ES,
      c_File_Cache_Id_GM,
      c_File_Cache_Id_GM_Aux,
      c_File_Cache_Id_GM_Bing,
      c_File_Cache_Id_MOBAC,
      c_File_Cache_Id_OsmAnd,
      c_File_Cache_Id_TMS
    ];
end;

function TfrmCacheManager.CreateSimpleTileStorage(
  const ARootPath: string;
  const ADefExtention: string;
  const AArchiveType: TArchiveType;
  const AStorageAbilities: ITileStorageAbilities;
  const AProjectionSet: IProjectionSet;
  const AFormatID: Byte
): ITileStorage;
var
  VContentType: IContentTypeInfoBasic;
  VItem: ITileStorageTypeListItem;
  VStorageType: ITileStorageType;
  VStorageConfig: ITileStorageTypeConfig;
  VArchiveReader: IArchiveReaderBase;
  VArchiveWriter: IArchiveWriterBase;
begin
  Result := nil;

  if AArchiveType = atNotArch then begin
    VItem := FTileStorageTypeList.GetItemByCode(AFormatID);
    if VItem <> nil then begin
      VStorageType := VItem.StorageType;
    end else begin
      raise Exception.CreateFmt('Fail to get StorageType by ID: %d', [AFormatID]);
    end;
  end else begin
    if not IsFileSystemStorage(AFormatID) then begin
      raise Exception.Create('Supported archives with FileSystem storages only!');
    end;

    VArchiveReader := nil;
    VArchiveWriter := nil;

    case AArchiveType of
      atTar: begin
        if AStorageAbilities.AllowScan then begin
          VArchiveReader := FArchiveReadWriteFactory.TarSequential.ReaderFactory.Build(ARootPath);
        end else
        if AStorageAbilities.AllowAdd then begin
          VArchiveWriter := FArchiveReadWriteFactory.TarSequential.WriterFactory.Build(ARootPath);
        end;
      end;
      atZip: begin
        if AStorageAbilities.AllowScan then begin
          VArchiveReader := FArchiveReadWriteFactory.ZipSequential.ReaderFactory.Build(ARootPath);
        end else
        if AStorageAbilities.AllowAdd then begin
          VArchiveWriter := FArchiveReadWriteFactory.ZipSequential.WriterFactory.Build(ARootPath);
        end;
      end;
    else
      raise Exception.CreateFmt('Unexpected ArchiveType value: %d', [Integer(AArchiveType)]);
    end;

    if (VArchiveReader <> nil) and (VArchiveWriter <> nil) then begin
      Assert(False);
    end;

    VStorageConfig :=
      TTileStorageTypeConfig.Create(
        TPathConfig.Create('', ARootPath, nil) as IPathConfig
      );

    VStorageType :=
      TTileStorageTypeArchive.Create(
        AStorageAbilities,
        VArchiveReader,
        VArchiveWriter,
        FContentTypeManager,
        FFileNameGeneratorsList.GetGenerator(AFormatID),
        FFileNameParsersList.GetParser(AFormatID),
        FMapVersionFactory,
        VStorageConfig
      );
  end;

  VContentType := FContentTypeManager.GetInfoByExt(StringToAnsiSafe(ADefExtention));
  if VContentType = nil then begin
    raise Exception.Create('Fail to get ContentType for extention: ' + ADefExtention);
  end;

  Result :=
    VStorageType.BuildStorage(
      nil,
      AProjectionSet,
      VContentType,
      nil,
      ARootPath,
      nil
    );
end;

procedure TfrmCacheManager.btnSelectSrcPathClick(Sender: TObject);
var
  VPath: string;
begin
  VPath := edtPath.Text;
  if SelectDirectory('', '', VPath) then begin
    edtPath.Text := IncludeTrailingPathDelimiter(VPath);
  end;
end;

procedure TfrmCacheManager.btnCanselClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmCacheManager.btnSelectDestPathClick(Sender: TObject);
var
  VPath: string;
begin
  VPath := edtDestPath.Text;
  if SelectDirectory('', '', VPath) then begin
    edtDestPath.Text := IncludeTrailingPathDelimiter(VPath);
  end;
end;

procedure TfrmCacheManager.btnStartClick(Sender: TObject);
begin
  if PageControl1.ActivePageIndex = 0 then begin
    ProcessCacheConverter;
  end;
  if not tbtmDontClose.Checked then begin
    Self.Close;
  end;
end;

procedure TfrmCacheManager.ProcessCacheConverter;

  function IsTileCacheInArchive(
    const APath: string;
    const AFileMustExists: Boolean;
    out AArchType: TArchiveType
  ): Boolean;
  var
    VExt: string;
  begin
    Result := False;
    AArchType := atNotArch;

    if AFileMustExists and not FileExists(APath) then begin
      Exit;
    end;

    VExt := LowerCase(ExtractFileExt(APath));
    if VExt = '.tar' then begin
      AArchType := atTar;
    end else if VExt = '.zip' then begin
      AArchType := atZip;
    end else if VExt <> '' then begin
      raise Exception.Create('Unsupported archive type: ' + VExt);
    end;

    Result := AArchType <> atNotArch;
  end;

var
  VProgressInfo: ICacheConverterProgressInfo;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VConverterThread: TThreadCacheConverter;
  VProjectionSet: IProjectionSet;
  VSourceStorage: ITileStorage;
  VDestStorage: ITileStorage;
  VSourceVersion: IMapVersionInfo;
  VDestVersion: IMapVersionInfo;
  VSourcePath: string;
  VDestPath: string;
  VDefExtention: string;
  VDotPos: Integer;
  VArchType: TArchiveType;
begin
  VProgressInfo := TCacheConverterProgressInfo.Create;

  VCancelNotifierInternal :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'))
    );
  VOperationID := VCancelNotifierInternal.CurrentOperation;

  VProjectionSet := FProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);

  VDefExtention := Trim(edtDefExtention.Text);
  VDotPos := Pos('.', VDefExtention);
  if VDotPos > 0 then begin
    VDefExtention := Copy(VDefExtention, VDotPos, Length(VDefExtention) - VDotPos + 1);
  end else begin
    VDefExtention := '.' + VDefExtention;
  end;
  VDefExtention := LowerCase(VDefExtention);

  VSourcePath := Trim(edtPath.Text);
  if not IsTileCacheInArchive(VSourcePath, True, VArchType) then begin
    VSourcePath := IncludeTrailingPathDelimiter(VSourcePath);
  end;

  VSourceStorage :=
    CreateSimpleTileStorage(
      VSourcePath,
      VDefExtention,
      VArchType,
      TTileStorageAbilities.Create([tsatScan]),
      VProjectionSet,
      FfrSrcCacheTypesList.IntCode
    );

  VDestPath := Trim(edtDestPath.Text);
  if not IsTileCacheInArchive(VDestPath, False, VArchType) then begin
    VDestPath := IncludeTrailingPathDelimiter(VDestPath);
    if FfrDestCacheTypesList.IntCode <> c_File_Cache_Id_DBMS then begin
      ForceDirectories(VDestPath);
    end;
  end;

  VDestStorage :=
    CreateSimpleTileStorage(
      VDestPath,
      VDefExtention,
      VArchType,
      TTileStorageAbilities.Create([tsatAdd]),
      VProjectionSet,
      FfrDestCacheTypesList.IntCode
    );

  VSourceVersion := nil;
  VDestVersion := nil;

  if chkCheckSourceVersion.Checked then begin
    VSourceVersion := FMapVersionFactory.CreateByStoreString(Trim(edtSourceVersion.Text));
  end;

  if chkReplaceDestVersion.Checked then begin
    VDestVersion := FMapVersionFactory.CreateByStoreString(Trim(edtDestVersion.Text));
  end;

  VConverterThread := TThreadCacheConverter.Create(
    VCancelNotifierInternal,
    VOperationID,
    VSourceStorage,
    VSourceVersion,
    VSourcePath,
    VDestStorage,
    VDestVersion,
    chkIgnoreTNE.Checked,
    chkRemove.Checked,
    chkOverwrite.Checked,
    VProgressInfo
  );

  TfrmProgressCacheConverter.Create(
    VConverterThread,
    FLanguageManager,
    FAppClosingNotifier,
    FTimerNoifier,
    VCancelNotifierInternal,
    VProgressInfo,
    FValueToStringConverter
  );

  if VCancelNotifierInternal.IsOperationCanceled(VOperationID) then begin
    VConverterThread.Terminate;
  end else begin
    VConverterThread.Start;
  end;
end;

end.


