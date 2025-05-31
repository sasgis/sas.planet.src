{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
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
  TBXDkPanels,
  fr_CacheTypeList,
  frm_ArchiverSettings,
  i_PathConfig,
  i_NotifierOperation,
  i_ProjectionSet,
  i_ProjectionSetFactory,
  i_TileStorage,
  i_LanguageManager,
  i_NotifierTime,
  i_ContentTypeManager,
  i_ArchiveReadWriteConfig,
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
    pgcMain: TPageControl;
    tsConverter: TTabSheet;
    pnlBottomButtons: TPanel;
    btnStart: TButton;
    btnClose: TButton;
    grpSrc: TGroupBox;
    lblPath: TLabel;
    edtPath: TEdit;
    pnlCacheTypes: TPanel;
    lblCacheType: TLabel;
    chkIgnoreTNE: TCheckBox;
    chkRemove: TCheckBox;
    lblDefExtension: TLabel;
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
    tbxtlbrDontClose: TTBXToolbar;
    tbtmDontClose: TTBItem;
    cbbSourceType: TComboBox;
    dlgOpenFile: TOpenDialog;
    dlgSaveFile: TSaveDialog;
    cbbDestType: TComboBox;
    cbbExt: TComboBox;
    btnArchiveWriterConfig: TTBXButton;
    procedure btnStartClick(Sender: TObject);
    procedure btnSelectSrcPathClick(Sender: TObject);
    procedure btnSelectDestPathClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnArchiveWriterConfigClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    type
      TSrcType = (stArchive, stFolder, stFile);
      TDestType = (dtArchiveZip, dtArchiveTar, dtFolder, dtFile);
      TArchiveType = (atUndef, atTar, atZip);
  private
    FSrcType: TSrcType;
    FDestType: TDestType;
    FLanguageManager: ILanguageManager;
    FBaseApplicationPath: IPathConfig;
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
    FfrmArchiverSettings: TfrmArchiverSettings;
    procedure OnSourceTypeChange(Sender: TObject);
    procedure OnDestTypeChange(Sender: TObject);
    procedure PrepareExtList;
    procedure ProcessCacheConverter;
    function CreateSimpleTileStorage(
      const ARootPath: string;
      const ADefExtension: string;
      const AIsArchive: Boolean;
      const AArchiveType: TArchiveType;
      const AArchiveWriteConfig: IArchiveWriteConfig;
      const AStorageAbilities: ITileStorageAbilities;
      const AProjectionSet: IProjectionSet;
      const AFormatID: Byte
    ): ITileStorage;
  protected
    procedure RefreshTranslation; override;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const ABaseApplicationPath: IPathConfig;
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
  System.Generics.Collections,
  gnugettext,
  c_CacheTypeCodes,
  c_CoordConverter,
  i_ArchiveReadWrite,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileStorageType,
  i_TileStorageTypeConfig,
  i_TileFileNameGenerator,
  i_TileFileNameParser,
  i_TileStorageTypeListItem,
  i_StringListStatic,
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
  u_AnsiStr,
  u_Dialogs,
  u_FileSystemFunc,
  frm_ProgressCacheConvrter;

{$R *.dfm}

{TfrmCacheManager}

constructor TfrmCacheManager.Create(
  const ALanguageManager: ILanguageManager;
  const ABaseApplicationPath: IPathConfig;
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
  FBaseApplicationPath := ABaseApplicationPath;
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

  FSrcType := stFolder;
  FDestType := dtFolder;

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

  FfrmArchiverSettings :=
    TfrmArchiverSettings.Create(
      Self,
      ALanguageManager
    );

  FPropertyState := CreateComponentPropertyState(
    Self, [], [], True, False, True, True
  );
  FPropertyState.ExcludeAll(FfrmArchiverSettings.Name);

  cbbSourceType.ItemIndex := 1; // Folder
  cbbDestType.ItemIndex := 2; // Folder

  PrepareExtList;
end;

destructor TfrmCacheManager.Destroy;
begin
  FreeAndNil(FfrmArchiverSettings);
  FreeAndNil(FfrSrcCacheTypesList);
  FreeAndNil(FfrDestCacheTypesList);
  inherited;
end;

procedure TfrmCacheManager.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FfrSrcCacheTypesList.Hide;
  FfrDestCacheTypesList.Hide;
end;

procedure TfrmCacheManager.FormShow(Sender: TObject);
begin
  cbbSourceType.OnChange := Self.OnSourceTypeChange;
  cbbDestType.OnChange := Self.OnDestTypeChange;

  OnSourceTypeChange(nil);
  OnDestTypeChange(nil);

  FfrSrcCacheTypesList.Show(pnlCacheTypes);
  FfrSrcCacheTypesList.Visible := True;

  FfrDestCacheTypesList.Show(pnlDestCacheTypes);
  FfrDestCacheTypesList.Visible := True;
end;

procedure TfrmCacheManager.PrepareExtList;
var
  I, J: Integer;
  VItems: TStrings;
  VList: IStringListStatic;
begin
  VList := FContentTypeManager.GetKnownExtList;
  if VList = nil then begin
    raise Exception.Create('Failed get list of extensions!');
  end;
  VItems := cbbExt.Items;
  VItems.BeginUpdate;
  try
    VItems.Clear;
    for I := 0 to VList.Count - 1 do begin
      VItems.Add(VList.Items[I]);
    end;
    Assert(VItems.Count > 0);
    I := VItems.IndexOf('.jpg');
    J := VItems.IndexOf('.jpeg');
    if (I >= 0) and (J >= 0) then begin
      VItems.Delete(J);
    end;
  finally
    VItems.EndUpdate;
  end;
  cbbExt.ItemIndex := -1;
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
  const ADefExtension: string;
  const AIsArchive: Boolean;
  const AArchiveType: TArchiveType;
  const AArchiveWriteConfig: IArchiveWriteConfig;
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

  if AIsArchive then begin
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
          VArchiveWriter := FArchiveReadWriteFactory.ZipSequential.WriterFactory.Build(ARootPath, AArchiveWriteConfig);
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
  end else begin
    VItem := FTileStorageTypeList.GetItemByCode(AFormatID);
    if VItem <> nil then begin
      VStorageType := VItem.StorageType;
    end else begin
      raise Exception.CreateFmt('Fail to get StorageType by ID: %d', [AFormatID]);
    end;
  end;

  VContentType := FContentTypeManager.GetInfoByExt(StringToAnsiSafe(ADefExtension));
  if VContentType = nil then begin
    raise Exception.Create('Fail to get ContentType for extension: ' + ADefExtension);
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
  VPath := Trim(edtPath.Text);
  case FSrcType of
    stArchive: begin
      dlgOpenFile.Filter := 'zip|*.zip|tar|*.tar';
      dlgOpenFile.DefaultExt := '*.zip';
      if dlgOpenFile.Execute then begin
        dlgOpenFile.InitialDir := ExtractFileDir(dlgOpenFile.FileName);
        edtPath.Text := dlgOpenFile.FileName;
      end;
    end;
    stFolder: begin
      if VPath = '' then begin
        VPath := IncludeTrailingPathDelimiter(dlgOpenFile.InitialDir);
      end;
      if SelectDirectory('', '', VPath) then begin
        dlgOpenFile.InitialDir := ExcludeTrailingBackslash(VPath);
        edtPath.Text := IncludeTrailingPathDelimiter(VPath);
      end;
    end;
    stFile: begin
      dlgOpenFile.Filter := 'All|*.*';
      dlgOpenFile.DefaultExt := '*.*';
      if dlgOpenFile.Execute then begin
        dlgOpenFile.InitialDir := ExtractFileDir(dlgOpenFile.FileName);
        edtPath.Text := dlgOpenFile.FileName;
      end;
    end;
  else
    Assert(False);
  end;
end;

procedure TfrmCacheManager.btnSelectDestPathClick(Sender: TObject);
var
  VPath: string;
begin
  VPath := Trim(edtDestPath.Text);
  case FDestType of
    dtArchiveZip: begin
      dlgSaveFile.Filter := 'zip|*.zip';
      dlgSaveFile.DefaultExt := '*.zip';
    end;
    dtArchiveTar: begin
      dlgSaveFile.Filter := 'tar|*.tar';
      dlgSaveFile.DefaultExt := '*.tar';
    end;
    dtFolder: begin
      if VPath = '' then begin
        VPath := IncludeTrailingBackslash(dlgSaveFile.InitialDir);
      end;
      if SelectDirectory('', '', VPath) then begin
        dlgSaveFile.InitialDir := ExcludeTrailingBackslash(VPath);
        edtDestPath.Text := IncludeTrailingPathDelimiter(VPath);
      end;
    end;
    dtFile: begin
      dlgSaveFile.Filter := 'All|*.*';
      dlgSaveFile.DefaultExt := '*.*';
    end;
  else
    Assert(False);
  end;
  if FDestType in [dtArchiveZip, dtArchiveTar, dtFile] then begin
    if dlgSaveFile.Execute then begin
      dlgSaveFile.InitialDir := ExtractFileDir(dlgSaveFile.FileName);
      edtDestPath.Text := dlgSaveFile.FileName;
    end;
  end;
end;

procedure TfrmCacheManager.btnStartClick(Sender: TObject);

  function IsValidInputForCacheConverter: Boolean;
  begin
    Result := False;

    // source
    if Trim(edtPath.Text) = '' then begin
      case FSrcType of
        stArchive, stFile: ShowErrorMessage(_('Source cache file is not specified!'));
        stFolder: ShowErrorMessage(_('Source cache path is not specified!'));
      else
        Assert(False);
      end;
      Exit;
    end;
    if FfrSrcCacheTypesList.cbbCacheType.ItemIndex = -1 then begin
      ShowErrorMessage(_('Source cache type is not selected!'));
      Exit;
    end;
    if cbbExt.ItemIndex = -1 then begin
      ShowErrorMessage(_('Tiles extension is not selected!'));
      Exit;
    end;

    // dest
    if Trim(edtDestPath.Text) = '' then begin
      case FDestType of
        dtArchiveZip, dtArchiveTar, dtFile: begin
          ShowErrorMessage(_('Dest cache file is not specified!'));
        end;
        dtFolder: ShowErrorMessage(_('Dest cache path is not specified!'));
      else
        Assert(False);
      end;
      Exit;
    end;
    if FfrDestCacheTypesList.cbbCacheType.ItemIndex = -1 then begin
      ShowErrorMessage(_('Dest cache type is not selected!'));
      Exit;
    end;

    Result := True;
  end;

begin
  if pgcMain.ActivePageIndex = 0 then begin
    if IsValidInputForCacheConverter then begin
      ProcessCacheConverter;
    end else begin
      Exit;
    end;
  end;
  if not tbtmDontClose.Checked then begin
    Self.Close;
  end;
end;

procedure TfrmCacheManager.btnCloseClick(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmCacheManager.OnSourceTypeChange(Sender: TObject);
var
  VOldIntCode: Integer;
begin
  VOldIntCode := FfrSrcCacheTypesList.IntCode;
  case cbbSourceType.ItemIndex of
    0: begin
      FSrcType := stArchive;
      lblPath.Caption := _('File:');
      chkRemove.Checked := False;
      chkRemove.Enabled := False;
      FfrSrcCacheTypesList.FilterOptions := [tstcInSeparateFiles];
    end;
    1: begin
      FSrcType := stFolder;
      lblPath.Caption := _('Path:');
      chkRemove.Enabled := True;
      FfrSrcCacheTypesList.FilterOptions := CTileStorageTypeClassAll - [tstcInMemory] - [tstcOneFile];
    end;
    2: begin
      FSrcType := stFile;
      lblPath.Caption := _('File:');
      chkRemove.Checked := False;
      chkRemove.Enabled := False;
      FfrSrcCacheTypesList.FilterOptions := [tstcOneFile];
    end;
  else
    Assert(False);
  end;
  if Assigned(Sender) then begin
    edtPath.Text := '';
  end;
  if not FfrSrcCacheTypesList.TrySetIntCode(VOldIntCode) then begin
    FfrSrcCacheTypesList.cbbCacheType.ItemIndex := -1;
  end;
end;

procedure TfrmCacheManager.OnDestTypeChange(Sender: TObject);
var
  VOldIntCode: Integer;
begin
  VOldIntCode := FfrDestCacheTypesList.IntCode;
  case cbbDestType.ItemIndex of
    0: begin
      FDestType := dtArchiveZip;
      btnArchiveWriterConfig.Visible := True;
      btnArchiveWriterConfig.Hint := _('zip archiver settings');
    end;
    1: begin
      FDestType := dtArchiveTar;
      btnArchiveWriterConfig.Visible := False;
    end;
    2: begin
      FDestType := dtFolder;
      lblDestPath.Caption := _('Path:');
      chkOverwrite.Enabled := True;
      FfrDestCacheTypesList.FilterOptions := CTileStorageTypeClassAll - [tstcInMemory];
      btnArchiveWriterConfig.Visible := False;
    end;
    3: begin
      FDestType := dtFile;
      lblDestPath.Caption := _('File:');
      chkOverwrite.Checked := False;
      chkOverwrite.Enabled := True;
      FfrDestCacheTypesList.FilterOptions := [tstcOneFile];
      btnArchiveWriterConfig.Visible := False;
    end;
  else
    Assert(False);
  end;
  if FDestType in [dtArchiveZip, dtArchiveTar] then begin
    lblDestPath.Caption := _('File:');
    chkOverwrite.Checked := False;
    chkOverwrite.Enabled := False;
    FfrDestCacheTypesList.FilterOptions := [tstcInSeparateFiles];
  end;
  if Assigned(Sender) then begin
    edtDestPath.Text := '';
  end;
  if not FfrDestCacheTypesList.TrySetIntCode(VOldIntCode) then begin
    FfrDestCacheTypesList.cbbCacheType.ItemIndex := -1;
  end;
end;

procedure TfrmCacheManager.btnArchiveWriterConfigClick(Sender: TObject);
begin
  FfrmArchiverSettings.ShowModal;
end;

procedure TfrmCacheManager.ProcessCacheConverter;

  function DetectSrcArchiveType(
    const AFileName: string;
    out AArchiveType: TArchiveType
  ): Boolean;
  var
    VExt: string;
  begin
    Result := False;

    if not FileExists(AFileName) then begin
      ShowErrorMessage(Format(_('Source file does not exists: %s'), [AFileName]));
      Exit;
    end;

    VExt := LowerCase(ExtractFileExt(AFileName));
    if VExt = '.tar' then begin
      AArchiveType := atTar;
    end else
    if VExt = '.zip' then begin
      AArchiveType := atZip;
    end else
    if VExt <> '' then begin
      ShowErrorMessage(Format(_('Unsupported archive type: %s'), [VExt]));
      Exit;
    end else
    if VExt = '' then begin
      ShowErrorMessage(Format(_('Failed detect archive type from file name: %s'), [AFileName]));
      Exit;
    end;

    Result := True;
  end;

  function PreparePath(const APath: string): string;
  begin
    Result := APath;
    if IsRelativePath(Result) then begin
      Result := RelativeToAbsolutePath(FBaseApplicationPath.FullPath, Result);
    end;
    if not ForceDirectories(Result) then begin
      RaiseLastOSError;
    end;
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
  VDestFile: string;
  VDefExtension: string;
  VArchiveType: TArchiveType;
begin
  VArchiveType := atUndef;

  VProgressInfo := TCacheConverterProgressInfo.Create;

  VCancelNotifierInternal :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'))
    );
  VOperationID := VCancelNotifierInternal.CurrentOperation;

  VProjectionSet := FProjectionSetFactory.GetProjectionSetByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);

  VDefExtension := cbbExt.Items.Strings[cbbExt.ItemIndex];

  VSourcePath := Trim(edtPath.Text);
  case FSrcType of
    stArchive: begin
      if not DetectSrcArchiveType(VSourcePath, VArchiveType) then begin
        Exit;
      end;
    end;
    stFolder: begin
      VSourcePath := IncludeTrailingPathDelimiter(VSourcePath);
    end;
    stFile: begin
      // nothing to do
    end;
  else
    Assert(False);
  end;

  VSourceStorage :=
    CreateSimpleTileStorage(
      VSourcePath,
      VDefExtension,
      (FSrcType = stArchive),
      VArchiveType,
      nil,
      TTileStorageAbilities.Create([tsatScan]),
      VProjectionSet,
      FfrSrcCacheTypesList.IntCode
    );

  VDestPath := Trim(edtDestPath.Text);
  case FDestType of
    dtArchiveZip: begin
      VArchiveType := atZip;
    end;
    dtArchiveTar: begin
      VArchiveType := atTar;
    end;
    dtFolder: begin
      if FfrDestCacheTypesList.IntCode <> c_File_Cache_Id_DBMS then begin
        VDestPath := PreparePath(VDestPath);
      end;
      VDestPath := IncludeTrailingPathDelimiter(VDestPath);
    end;
    dtFile: begin
      VDestFile := VDestPath;
      VDestPath := PreparePath(ExtractFilePath(VDestFile)) + ExtractFileName(VDestFile);
    end;
  else
    Assert(False);
  end;

  VDestStorage :=
    CreateSimpleTileStorage(
      VDestPath,
      VDefExtension,
      (FDestType in [dtArchiveZip, dtArchiveTar]),
      VArchiveType,
      FfrmArchiverSettings.GetWriterConfig,
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

procedure TfrmCacheManager.RefreshTranslation;
type
  TComboBoxState = TDictionary<string, Integer>;
var
  I: Integer;
  VValue: Integer;
  VComboBox: TComboBox;
  VComboBoxState: TComboBoxState;
begin
  VComboBoxState := TComboBoxState.Create;
  try
    // save all TComboBox state
    for I := 0 to Self.ComponentCount - 1 do begin
      if Self.Components[I] is TComboBox then begin
        VComboBox := TComboBox(Self.Components[I]);
        VComboBoxState.Add(VComboBox.Name, VComboBox.ItemIndex);
      end;
    end;

    inherited;

    // restore TComboBox states
    for I := 0 to Self.ComponentCount - 1 do begin
      if Self.Components[I] is TComboBox then begin
        VComboBox := TComboBox(Self.Components[I]);
        if not VComboBoxState.TryGetValue(VComboBox.Name, VValue) then begin
          VValue := 0;
        end;
        VComboBox.ItemIndex := VValue;
      end;
    end;
  finally
    VComboBoxState.Free;
  end;
end;

end.


