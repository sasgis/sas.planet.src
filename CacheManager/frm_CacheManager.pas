{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit frm_CacheManager;

interface

uses
  Windows,
  Messages,
  SysUtils,
  Variants,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  ComCtrls,
  i_NotifierOperation,
  i_CoordConverter,
  i_CoordConverterFactory,
  i_TileStorage,
  i_LanguageManager,
  i_NotifierTime,
  i_ContentTypeManager,
  i_TileFileNameGeneratorsList,
  i_TileFileNameParsersList,
  i_ValueToStringConverter,
  i_GlobalBerkeleyDBHelper,
  u_CommonFormAndFrameParents;

type
  TfrmCacheManager = class(TFormWitghLanguageManager)
    PageControl1: TPageControl;
    tsConverter: TTabSheet;
    pnlBottomButtons: TPanel;
    btnStart: TButton;
    btnCansel: TButton;
    chkCloseWithStart: TCheckBox;
    grpSrc: TGroupBox;
    lblPath: TLabel;
    edtPath: TEdit;
    cbbCacheTypes: TComboBox;
    lblCacheType: TLabel;
    chkIgnoreTNE: TCheckBox;
    chkRemove: TCheckBox;
    edtDefExtention: TEdit;
    lblDefExtention: TLabel;
    grpDestCache: TGroupBox;
    lblDestPath: TLabel;
    lblDestFormat: TLabel;
    edtDestPath: TEdit;
    cbbDestCacheTypes: TComboBox;
    chkOverwrite: TCheckBox;
    btnSelectSrcPath: TButton;
    btnSelectDestPath: TButton;
    procedure btnStartClick(Sender: TObject);
    procedure btnSelectSrcPathClick(Sender: TObject);
    procedure btnSelectDestPathClick(Sender: TObject);
    procedure btnCanselClick(Sender: TObject);
  private
    type
      TTileCacheInArchiveType = (atNoArch = 0, atTar = 1, atZip = 2, atUnk = 3);
  private
    FLanguageManager: ILanguageManager;
    FAppClosingNotifier: INotifierOneOperation;
    FTimerNoifier: INotifierTime;
    FGCNotifier: INotifierTime;
    FGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
    FContentTypeManager: IContentTypeManager;
    FCoordConverterFactory: ICoordConverterFactory;
    FFileNameGeneratorsList: ITileFileNameGeneratorsList;
    FFileNameParsersList: ITileFileNameParsersList;
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    procedure ProcessCacheConverter;
    function CreateSimpleTileStorage(
      const ARootPath: string;
      const ADefExtention: string;
      const AArchiveType: TTileCacheInArchiveType;
      const ACoordConverter: ICoordConverter;
      const AFormatID: Byte
    ): ITileStorage;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATimerNoifier: INotifierTime;
      const AGCNotifier: INotifierTime;
      const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
      const AContentTypeManager: IContentTypeManager;
      const ACoordConverterFactory: ICoordConverterFactory;
      const AFileNameGeneratorsList: ITileFileNameGeneratorsList;
      const AFileNameParsersList: ITileFileNameParsersList;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  c_CacheTypeCodes,
  c_CoordConverter,
  i_MapVersionConfig,
  i_ContentTypeInfo,
  i_TileFileNameGenerator,
  i_TileFileNameParser,
  i_CacheConverterProgressInfo,
  u_Notifier,
  u_NotifierOperation,
  u_ThreadCacheConverter,
  u_MapVersionFactorySimpleString,
  u_TileStorageFileSystem,
  u_TileStorageBerkeleyDB,
  u_TileStorageDBMS,
  u_TileStorageGE,
  u_TileStorageTar,
  u_CacheConverterProgressInfo,
  frm_ProgressCacheConvrter;

{$R *.dfm}

{TfrmCacheManager}

constructor TfrmCacheManager.Create(
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATimerNoifier: INotifierTime;
  const AGCNotifier: INotifierTime;
  const AGlobalBerkeleyDBHelper: IGlobalBerkeleyDBHelper;
  const AContentTypeManager: IContentTypeManager;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AFileNameGeneratorsList: ITileFileNameGeneratorsList;
  const AFileNameParsersList: ITileFileNameParsersList;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(ALanguageManager);
  FLanguageManager := ALanguageManager;
  FAppClosingNotifier := AAppClosingNotifier;
  FTimerNoifier := ATimerNoifier;
  FGCNotifier := AGCNotifier;
  FGlobalBerkeleyDBHelper := AGlobalBerkeleyDBHelper;
  FFileNameGeneratorsList := AFileNameGeneratorsList;
  FFileNameParsersList := AFileNameParsersList;
  FContentTypeManager := AContentTypeManager;
  FCoordConverterFactory := ACoordConverterFactory;
  FValueToStringConverterConfig := AValueToStringConverterConfig;

  cbbCacheTypes.ItemIndex := 1; // SAS.Planet
  cbbDestCacheTypes.ItemIndex := 5; // BerkeleyDB
end;

function TfrmCacheManager.CreateSimpleTileStorage(
  const ARootPath: string;
  const ADefExtention: string;
  const AArchiveType: TTileCacheInArchiveType;
  const ACoordConverter: ICoordConverter;
  const AFormatID: Byte
): ITileStorage;
var
  VMapVersionFactory: IMapVersionFactory;
  VContentType: IContentTypeInfoBasic;
  VFileNameGenerator: ITileFileNameGenerator;
  VFileNameParser: ITileFileNameParser;
begin
  Result := nil;

  if (AArchiveType <> atNoArch) and (AFormatID in [c_File_Cache_Id_BDB, c_File_Cache_Id_DBMS, c_File_Cache_Id_GE, c_File_Cache_Id_GC]) then begin
    Exit;
  end;

  VContentType := FContentTypeManager.GetInfoByExt(ADefExtention);
  if AFormatID = c_File_Cache_Id_BDB then begin
    VMapVersionFactory := TMapVersionFactorySimpleString.Create;
    Result :=
      TTileStorageBerkeleyDB.Create(
        FGlobalBerkeleyDBHelper,
        ACoordConverter,
        ARootPath,
        FGCNotifier,
        nil,
        FContentTypeManager,
        VMapVersionFactory,
        VContentType
      );
  end else if AFormatID = c_File_Cache_Id_DBMS then begin
    VMapVersionFactory := TMapVersionFactorySimpleString.Create;
    Result :=
      TTileStorageDBMS.Create(
        ACoordConverter,
        '', // TODO: add global DBMS Root here
        ARootPath,
        FGCNotifier,
        nil,
        FContentTypeManager,
        VMapVersionFactory,
        VContentType
      );
  end else if AFormatID = c_File_Cache_Id_GE then begin
//    Result :=
//      TTileStorageGE.Create(
//        VStorageConfig,
//        VGlobalCacheConfig,
//        FContentTypeManager
//      );
  end else if AFormatID = c_File_Cache_Id_GC then begin
//    Result :=
//      TTileStorageGC.Create(
//        VStorageConfig,
//        VGlobalCacheConfig,
//        FContentTypeManager
//      );
  end else if AFormatID in [c_File_Cache_Id_GMV, c_File_Cache_Id_SAS, c_File_Cache_Id_ES, c_File_Cache_Id_GM, c_File_Cache_Id_GM_Aux, c_File_Cache_Id_GM_Bing] then begin
    VMapVersionFactory := TMapVersionFactorySimpleString.Create;
    VFileNameGenerator := FFileNameGeneratorsList.GetGenerator(AFormatID);
    VFileNameParser := FFileNameParsersList.GetParser(AFormatID);
    case AArchiveType of
      atNoArch: begin
        Result :=
          TTileStorageFileSystem.Create(
            ACoordConverter,
            ARootPath,
            VContentType,
            VMapVersionFactory,
            VFileNameGenerator,
            VFileNameParser
          );
      end;
      atTar: begin
        Result :=
          TTileStorageTar.Create(
            ARootPath,
            VContentType,
            FContentTypeManager,
            ACoordConverter,
            VFileNameParser,
            VFileNameGenerator
          );
      end;
      atZip: begin
        // ToDo
      end;
    else
      // Error
    end;
  end;
end;

destructor TfrmCacheManager.Destroy;
begin
  FAppClosingNotifier := nil;
  inherited Destroy;
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
  if chkCloseWithStart.Checked then begin
    Self.Close;
  end;
end;

procedure TfrmCacheManager.ProcessCacheConverter;

  function GetCacheFormatFromIndex(const AIndex: Integer): Byte;
  begin
    case AIndex of
      0: Result := c_File_Cache_Id_GMV;
      1: Result := c_File_Cache_Id_SAS;
      2: Result := c_File_Cache_Id_ES;
      3: Result := c_File_Cache_Id_GM;
      4: Result := c_File_Cache_Id_GM_Aux;
      5: Result := c_File_Cache_Id_BDB;
      6: Result := c_File_Cache_Id_DBMS;
    else
      Result := c_File_Cache_Id_SAS;
    end;
  end;

  function IsTileCacheInArchive(const APath: string; out AArchType: TTileCacheInArchiveType): Boolean;
  begin
    if LowerCase(ExtractFileExt(APath)) = '.tar' then begin
      AArchType := atTar;
    end else if LowerCase(ExtractFileExt(APath)) = '.zip' then begin
      AArchType := atZip;
    end else if ExtractFileExt(APath) <> '' then begin
      AArchType := atUnk;
    end else begin
      AArchType := atNoArch;
    end;
    Result := not (AArchType in [atNoArch, atUnk]);
  end;

var
  VProgressInfo: ICacheConverterProgressInfo;
  VCancelNotifierInternal: INotifierOperationInternal;
  VOperationID: Integer;
  VConverterThread: TThreadCacheConverter;
  VCoordConverter: ICoordConverter;
  VSource: ITileStorage;
  VTarget: ITileStorage;
  VSourcePath: string;
  VDestPath: string;
  VDefExtention: string;
  VDotPos: Integer;
  VArchType: TTileCacheInArchiveType;
begin
  VProgressInfo := TCacheConverterProgressInfo.Create;

  VCancelNotifierInternal := TNotifierOperation.Create(TNotifierBase.Create);
  VOperationID := VCancelNotifierInternal.CurrentOperation;

  VCoordConverter := FCoordConverterFactory.GetCoordConverterByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);

  VDefExtention := Trim(edtDefExtention.Text);
  VDotPos := Pos('.', VDefExtention);
  if VDotPos > 0 then begin
    VDefExtention := Copy(VDefExtention, VDotPos, Length(VDefExtention) - VDotPos + 1);
  end else begin
    VDefExtention := '.' + VDefExtention;
  end;
  VDefExtention := LowerCase(VDefExtention);

  VSourcePath := Trim(edtPath.Text);
  if not IsTileCacheInArchive(VSourcePath, VArchType) then begin
    VSourcePath := IncludeTrailingPathDelimiter(VSourcePath);
  end;

  VSource :=
    CreateSimpleTileStorage(
      VSourcePath,
      VDefExtention,
      VArchType,
      VCoordConverter,
      GetCacheFormatFromIndex(cbbCacheTypes.ItemIndex)
    );

  VDestPath := Trim(edtDestPath.Text);
  if not IsTileCacheInArchive(VDestPath, VArchType) then begin
    VDestPath := IncludeTrailingPathDelimiter(VDestPath);  
    if (cbbDestCacheTypes.ItemIndex <> 6) then begin
      // кроме СУБД - создадим папку в целевом хранилище
      ForceDirectories(VDestPath);
    end;
  end;

  VTarget :=
    CreateSimpleTileStorage(
      VDestPath,
      VDefExtention,
      VArchType,
      VCoordConverter,
      GetCacheFormatFromIndex(cbbDestCacheTypes.ItemIndex)
    );

  VConverterThread := TThreadCacheConverter.Create(
    VCancelNotifierInternal,
    VOperationID,
    VSource,
    VSourcePath,
    VTarget,
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
    FValueToStringConverterConfig
  );
end;

end.


