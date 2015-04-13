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

unit frm_PascalScriptIDE;

interface

uses
  Types,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  TB2Dock,
  TB2Toolbar,
  TB2Item,
  TBX,
  SynEdit,
  SynHighlighterPas,
  SynHighlighterIni,
  SynEditHighlighter,
  frm_PascalScriptDbgOut,
  i_Listener,
  i_NotifierOperation,
  i_MapTypeSet,
  i_MapType,
  i_MapTypeGUIConfigList,
  i_ZmpInfo,
  i_InetConfig,
  i_ProjConverter,
  i_CoordConverterFactory,
  i_LocalCoordConverter,
  i_LocalCoordConverterChangeable,
  i_ArchiveReadWriteFactory,
  i_LanguageManager,
  i_MapVersionFactory,
  i_Bitmap32BufferFactory,
  i_ContentTypeManager,
  i_ZmpConfig,
  i_Downloader,
  i_MainMapsState,
  u_PSExecEx,
  u_PSPascalCompilerEx,
  u_CommonFormAndFrameParents,
  u_TileDownloadRequestBuilderPascalScriptVars;

type
  TfrmPascalScriptIDE = class(TFormWitghLanguageManager)
    pgcMain: TPageControl;
    tsParams: TTabSheet;
    tsScript: TTabSheet;
    pnlScriptEditor: TPanel;
    pnlLog: TPanel;
    splEditLog: TSplitter;
    pnlParamsTxt: TPanel;
    pnlInput: TPanel;
    splInOut: TSplitter;
    grpInput: TGroupBox;
    statEditor: TStatusBar;
    tbtlbrMain: TTBXToolbar;
    tbxtmRun: TTBXItem;
    tbxtmParams: TTBXItem;
    tbxtmScript: TTBXItem;
    tbxSep1: TTBXSeparatorItem;
    tbxtmSyntaxCheck: TTBXItem;
    tbxtmDecompile: TTBXItem;
    lstLog: TListBox;
    tbxsbmntmOpen: TTBXSubmenuItem;
    tbxSep2: TTBXSeparatorItem;
    tbxtmFromFolder: TTBXItem;
    tbxsbmntmMap: TTBXSubmenuItem;
    tbxsbmntmLayer: TTBXSubmenuItem;
    tbxtmWordWrap: TTBXItem;
    tbxsprtrtm1: TTBXSeparatorItem;
    tbxtmFromZip: TTBXItem;
    tbxsprtrtm2: TTBXSeparatorItem;
    dlgOpenZmpFile: TOpenDialog;
    edtGetX: TEdit;
    lblGetX: TLabel;
    lblGetY: TLabel;
    edtGetY: TEdit;
    lblGetZ: TLabel;
    edtGetZ: TEdit;
    btnSetXYZ: TButton;
    procedure tbxtmParamsClick(Sender: TObject);
    procedure tbxtmScriptClick(Sender: TObject);
    procedure tbxtmRunClick(Sender: TObject);
    procedure tbxtmSyntaxCheckClick(Sender: TObject);
    procedure tbxtmDecompileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tbxtmWordWrapClick(Sender: TObject);
    procedure tbxtmFromZipClick(Sender: TObject);
    procedure tbxtmFromFolderClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSetXYZClick(Sender: TObject);
  private
    FPrepared: Boolean;
    FZmpModified: Boolean;
    FLastPath: string;
    FfrmDebug: TfrmPascalScriptDbgOut;
    synedtScript: TSynEdit;
    synedtParams: TSynEdit;
    FLanguageManager: ILanguageManager;
    FPSVars: TRequestBuilderVars;
    FMainMapState: IMainMapsState;
    FZmp: IZmpInfo;
    FZmpConfig: IZmpConfig;
    FGUIConfigList: IMapTypeGUIConfigList;
    FCoordConverterFactory: ICoordConverterFactory;
    FContentTypeManager: IContentTypeManager;
    FVersionFactory: IMapVersionFactory;
    FBufferFactory: IBitmap32BufferFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FInetConfig: IInetConfig;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FArchiveStream: TMemoryStream;
    FScriptBuffer: string;
    FProjFactory: IProjConverterFactory;
    FDownloader: IDownloader;
    FAppClosingNotifier: INotifierOneOperation;
    FAppClosingListener: IListener;
    FCancelNotifierInternal: INotifierOperationInternal;
    FViewPortState: ILocalCoordConverterChangeable;
    function GetZmpFromFolder(const APath: string): IZmpInfo;
    function GetZmpFromZip(const AFileName: string): IZmpInfo;
    function GetZmpFromGUI: IZmpInfo;
    function Compile(out AByteCode: AnsiString): Boolean;
    function Execute: Boolean;
    procedure OnBeforeRunScript(const APSExec: TPSExecEx);
    procedure OnExecSuccess;
    procedure CreateSynEditTextHighlighters;
    procedure OnSynEditStatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure CreateMapUIMapsList;
    procedure CreateMapUILayersList;
    procedure OnClick(Sender: TObject);
    procedure InitByZmp(const AZmp: IZmpInfo);
    procedure OnAppClosing;
    procedure CancelOperation;
  public
    constructor Create(
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AMainMapState: IMainMapsState;
      const AZmpConfig: IZmpConfig;
      const ACoordConverterFactory: ICoordConverterFactory;
      const AContentTypeManager: IContentTypeManager;
      const AVersionFactory: IMapVersionFactory;
      const ABufferFactory: IBitmap32BufferFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AInetConfig: IInetConfig;
      const AProjFactory: IProjConverterFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const ALanguageManager: ILanguageManager;
      const AAppClosingNotifier: INotifierOneOperation;
      const AViewPortState: ILocalCoordConverterChangeable
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

uses
  {$WARN UNIT_PLATFORM OFF}
  FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  uPSUtils,
  uPSRuntime,
  uPSDisassembly,
  t_GeoTypes,
  t_PascalScript,
  i_TileRequest,
  i_ArchiveReadWrite,
  i_ConfigDataProvider,
  i_CoordConverter,
  i_SimpleHttpDownloader,
  u_PascalScriptTypes,
  u_ZmpInfo,
  u_GeoFunc,
  u_BinaryData,
  u_TileRequest,
  u_Notifier,
  u_NotifierOperation,
  u_ListenerByEvent,
  u_Synchronizer,
  u_ConfigDataProviderByZip,
  u_ConfigDataProviderByFolder,
  u_DownloaderHttp,
  u_DownloadResultFactory,
  u_SimpleHttpDownloader,
  u_MapTypeMenuItemsGeneratorSimple;

{$R *.dfm}

resourcestring
  rsSuccessfullyCompiled = 'Successfully compiled';
  rsSuccessfullyExecuted = 'Successfully executed';

function CompileTime_GetRegProcArray: TOnCompileTimeRegProcArray;
begin
  SetLength(Result, 5);
  Result[0] := @CompileTimeReg_ProjConverter;
  Result[1] := @CompileTimeReg_ProjConverterFactory;
  Result[2] := @CompileTimeReg_CoordConverterSimple;
  Result[3] := @CompileTimeReg_SimpleHttpDownloader;
  Result[4] := @CompileTimeReg_RequestBuilderVars;
end;

{ TfrmPascalScriptIDE }

constructor TfrmPascalScriptIDE.Create(
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AMainMapState: IMainMapsState;
  const AZmpConfig: IZmpConfig;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AContentTypeManager: IContentTypeManager;
  const AVersionFactory: IMapVersionFactory;
  const ABufferFactory: IBitmap32BufferFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AInetConfig: IInetConfig;
  const AProjFactory: IProjConverterFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const ALanguageManager: ILanguageManager;
  const AAppClosingNotifier: INotifierOneOperation;
  const AViewPortState: ILocalCoordConverterChangeable
);
begin
  inherited Create(ALanguageManager);

  FGUIConfigList := AGUIConfigList;
  FMainMapState := AMainMapState;
  FZmpConfig := AZmpConfig;
  FCoordConverterFactory := ACoordConverterFactory;
  FContentTypeManager := AContentTypeManager;
  FVersionFactory := AVersionFactory;
  FBufferFactory := ABufferFactory;
  FBitmapFactory := ABitmapFactory;
  FInetConfig := AInetConfig;
  FProjFactory := AProjFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
  FLanguageManager := ALanguageManager;
  FAppClosingNotifier := AAppClosingNotifier;
  FViewPortState := AViewPortState;

  FCancelNotifierInternal :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'))
    );

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    Self.OnAppClosing;
  end;

  FDownloader := TDownloaderHttp.Create(
    TDownloadResultFactory.Create
  );

  FPrepared := False;
  FZmpModified := True;
  FZmp := nil;
  FArchiveStream := TMemoryStream.Create;
  FLastPath := '';
  FScriptBuffer := '';
end;

destructor TfrmPascalScriptIDE.Destroy;
begin
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
    FAppClosingListener := nil;
  end;
  FArchiveStream.Free;
  FreeAndNil(FfrmDebug);
  inherited Destroy;
end;

procedure TfrmPascalScriptIDE.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Self.CancelOperation;
  Action := caHide;
  Application.MainForm.SetFocus;
end;

procedure TfrmPascalScriptIDE.FormShow(Sender: TObject);
var
  I: Integer;
begin
  if not FPrepared then begin
    FPrepared := True;
    FfrmDebug := TfrmPascalScriptDbgOut.Create(Self);
    for I := 0 to pgcMain.PageCount - 1 do begin
      pgcMain.Pages[I].TabVisible := False;
    end;
    pgcMain.ActivePageIndex := 1; // GetUrlScript
    btnSetXYZClick(Sender);
    CreateSynEditTextHighlighters;
    CreateMapUIMapsList;
    CreateMapUILayersList;
  end;
  InitByZmp(FMainMapState.ActiveMap.GetStatic.Zmp);
  FZmpModified := False;
end;

procedure TfrmPascalScriptIDE.InitByZmp(const AZmp: IZmpInfo);
begin
  FZmp := AZmp;
  synedtParams.Text := FZmp.DataProvider.ReadString('Params.txt', '');
  synedtScript.Text := FZmp.DataProvider.ReadString('GetUrlScript.txt', '');
  FScriptBuffer := '';
end;

function TfrmPascalScriptIDE.Compile(out AByteCode: AnsiString): Boolean;
var
  I: Integer;
  VCode: string;
  VComp: TPSPascalCompilerEx;
begin
  AByteCode := '';

  FfrmDebug.mmoDbgOut.Lines.Clear;

  if FZmpModified then begin
    InitByZmp(GetZmpFromGUI);
    FZmpModified := False;
  end;
  
  VCode := FZmp.DataProvider.ReadString('GetUrlScript.txt', '');

  VComp := TPSPascalCompilerEx.Create(CompileTime_GetRegProcArray);
  try
    Result := VComp.Compile(VCode);
    lstLog.Clear;
    for I := 0 to VComp.MsgCount -1 do begin
      lstLog.Items.Add(VComp.Msg[I].MessageToString);
    end;
    if Result then begin
      lstLog.Items.Add(rsSuccessfullyCompiled);
      Result := VComp.GetOutput(AByteCode);
    end;
  finally
    VComp.Free;
  end;
end;

function TfrmPascalScriptIDE.Execute: Boolean;
var
  VByteCode: TbtString;
  VExec: TPSExecEx;
begin
  Result := Compile(VByteCode);
  if Result then begin
    VExec := TPSExecEx.Create;
    try
      Result := VExec.LoadData(VByteCode);
      if Result then begin
        OnBeforeRunScript(VExec);
        Result := VExec.RunScript;
        if Result then begin
          lstLog.Items.Add(rsSuccessfullyExecuted);
          OnExecSuccess;
        end;
      end;
      if not Result then begin
        lstLog.Items.Add(
          '[Runtime error]: ' +
          TIFErrorToString(VExec.ExceptionCode, VExec.ExceptionString)
        );
      end;
    finally
      VExec.Free;
    end;
  end;
end;

procedure TfrmPascalScriptIDE.CreateMapUILayersList;
var
  VGenerator: TMapMenuGeneratorSimple;
  VLayersSet: IMapTypeSet;
begin
  VLayersSet := FMainMapState.LayersSet;
  if Assigned(VLayersSet) then begin
    VGenerator :=
      TMapMenuGeneratorSimple.Create(
        FGUIConfigList,
        VLayersSet,
        tbxsbmntmLayer,
        Self.OnClick
      );
    try
      VGenerator.BuildControls;
    finally
      FreeAndNil(VGenerator);
    end;
  end;
end;

procedure TfrmPascalScriptIDE.CreateMapUIMapsList;
var
  VGenerator: TMapMenuGeneratorSimple;
begin
  VGenerator :=
    TMapMenuGeneratorSimple.Create(
      FGUIConfigList,
      FMainMapState.MapsSet,
      tbxsbmntmMap,
      Self.OnClick
    );
  try
    VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
end;

function TfrmPascalScriptIDE.GetZmpFromFolder(const APath: string): IZmpInfo;
var
  VFileName: string;
  VZmpMapConfig: IConfigDataProvider;
begin
  VFileName := ExcludeTrailingPathDelimiter(APath);
  VFileName := ExtractFileName(VFileName);
  
  VZmpMapConfig := TConfigDataProviderByFolder.Create(APath);

  Result :=
    TZmpInfo.Create(
      FZmpConfig,
      FLanguageManager,
      FCoordConverterFactory,
      FContentTypeManager,
      FVersionFactory,
      FBitmapFactory,
      VFileName,
      VZmpMapConfig,
      0
    );
end;

function TfrmPascalScriptIDE.GetZmpFromZip(const AFileName: string): IZmpInfo;
var
  VZmpMapConfig: IConfigDataProvider;
begin
  VZmpMapConfig :=
    TConfigDataProviderByArchive.Create(
      AFileName,
      FArchiveReadWriteFactory.Zip.ReaderFactory.BuildByFileName(AFileName)
    );
  Result :=
    TZmpInfo.Create(
      FZmpConfig,
      FLanguageManager,
      FCoordConverterFactory,
      FContentTypeManager,
      FVersionFactory,
      FBitmapFactory,
      AFileName,
      VZmpMapConfig,
      0
    );
end;

function TfrmPascalScriptIDE.GetZmpFromGUI: IZmpInfo;
var
  VArchiveWriter: IArchiveWriter;
  VZmpMapConfig: IConfigDataProvider;
begin
  FArchiveStream.Clear;

  VArchiveWriter :=
    FArchiveReadWriteFactory.Zip.WriterFactory.BuildByStream(FArchiveStream);

  VArchiveWriter.AddFile(
    TBinaryData.CreateByString(synedtParams.Text),
    'params.txt',
    Now
  );

  VArchiveWriter.AddFile(
    TBinaryData.CreateByString(synedtScript.Text),
    'GetUrlScript.txt',
    Now
  );

  VArchiveWriter := nil;

  FArchiveStream.Position := 0;

  VZmpMapConfig :=
    TConfigDataProviderByArchive.Create(
      'ram.zmp',
      FArchiveReadWriteFactory.Zip.ReaderFactory.BuildByStream(FArchiveStream)
    );

  Result :=
    TZmpInfo.Create(
      FZmpConfig,
      FLanguageManager,
      FCoordConverterFactory,
      FContentTypeManager,
      FVersionFactory,
      FBitmapFactory,
      'ram.zmp',
      VZmpMapConfig,
      0
    );
end;

procedure TfrmPascalScriptIDE.OnClick(Sender: TObject);
var
  VMapType: IMapType;
begin
  VMapType := IMapType(TTBXItem(Sender).Tag);
  InitByZmp(VMapType.Zmp);
  FZmpModified := False;
end;

procedure TfrmPascalScriptIDE.OnBeforeRunScript(const APSExec: TPSExecEx);
var
  VUrlBase: AnsiString;
  VRequestHeader: AnsiString;
  VUseDownloader: Boolean;
  VSource: ITileRequest;
  VProjArgs: AnsiString;
  VDefProjConverter: IProjConverter;
  VSimpleDownloader: ISimpleHttpDownloader;
begin
  FPSVars.ExecTimeInit(APSExec);

  VUrlBase := FZmp.TileDownloadRequestBuilderConfig.UrlBase;
  VRequestHeader := FZmp.TileDownloadRequestBuilderConfig.RequestHeader;
  VUseDownloader := FZmp.TileDownloadRequestBuilderConfig.IsUseDownloader;
  VProjArgs := FZmp.TileDownloadRequestBuilderConfig.DefaultProjConverterArgs;

  VDefProjConverter := nil;
  if (VProjArgs <> '') and Assigned(FProjFactory) then begin
    VDefProjConverter := FProjFactory.GetByInitString(VProjArgs);
  end;

  VSimpleDownloader := nil;

  if FDownloader <> nil then begin
    if VUseDownloader then begin
      VSimpleDownloader :=
        TSimpleHttpDownloader.Create(
          FDownloader,
          FInetConfig.GetStatic,
          FCancelNotifierInternal,
          FCancelNotifierInternal.CurrentOperation
        );
    end;
  end;

  VSource :=
    TTileRequest.Create(
      Point(StrToInt(edtGetX.Text), StrToInt(edtGetY.Text)),
      StrToInt(edtGetZ.Text) - 1,
      FZmp.VersionConfig
    );

  FPSVars.ExecTimeSet(
    VUrlBase,
    VRequestHeader,
    FScriptBuffer,
    FLanguageManager.GetCurrentLanguageCode,
    FZmp.GeoConvert as ICoordConverterSimple,
    VSimpleDownloader,
    nil,  // LastResponseInfo
    VSource,
    VDefProjConverter,
    FProjFactory
  );
end;

procedure TfrmPascalScriptIDE.OnExecSuccess;

  function _VarToStr(const AStr: string): string;
  begin
    if AStr = '' then begin
      Result := '<empty>' + #13#10;
    end else begin
      Result := AStr + #13#10;
    end;
  end;

begin
  FfrmDebug.mmoDbgOut.Lines.Add('[ResultURL]');
  FfrmDebug.mmoDbgOut.Lines.Add(_VarToStr(FPSVars.ResultUrl));

  FfrmDebug.mmoDbgOut.Lines.Add('[RequestHead]');
  FfrmDebug.mmoDbgOut.Lines.Add(_VarToStr(FPSVars.RequestHead));

  FfrmDebug.mmoDbgOut.Lines.Add('[PostData]');
  FfrmDebug.mmoDbgOut.Lines.Add(_VarToStr(FPSVars.PostData));

  FfrmDebug.mmoDbgOut.Lines.Add('[ScriptBuffer]');
  FfrmDebug.mmoDbgOut.Lines.Add(_VarToStr(FPSVars.ScriptBuffer));

  FScriptBuffer := FPSVars.ScriptBuffer;

  FfrmDebug.visible := True;
end;

procedure TfrmPascalScriptIDE.tbxtmRunClick(Sender: TObject);
begin
  tbxtmScript.Checked := True;
  pgcMain.ActivePageIndex := 1;
  Execute;
end;

procedure TfrmPascalScriptIDE.tbxtmSyntaxCheckClick(Sender: TObject);
var
  VByteCode: TbtString;
begin
  tbxtmScript.Checked := True;
  pgcMain.ActivePageIndex := 1;
  Compile(VByteCode);
end;

procedure TfrmPascalScriptIDE.tbxtmDecompileClick(Sender: TObject);
var
  VByteCode: TbtString;
  VByteCodeReadable: TbtString;
begin
  tbxtmScript.Checked := True;
  pgcMain.ActivePageIndex := 1;
  if Compile(VByteCode) then begin
    IFPS3DataToText(VByteCode, VByteCodeReadable);
    FfrmDebug.mmoDbgOut.Lines.Text := VByteCodeReadable;
    FfrmDebug.visible := True;
  end;
end;

procedure TfrmPascalScriptIDE.tbxtmFromFolderClick(Sender: TObject);
begin
  if SelectDirectory('', '', FLastPath) then begin
    FLastPath := IncludeTrailingPathDelimiter(FLastPath);
    InitByZmp(GetZmpFromFolder(FLastPath));
    FZmpModified := False;
  end;
end;

procedure TfrmPascalScriptIDE.tbxtmFromZipClick(Sender: TObject);
begin
  if dlgOpenZmpFile.Execute then begin
    InitByZmp(GetZmpFromZip(dlgOpenZmpFile.FileName));
    FZmpModified := False;
  end;
end;

procedure TfrmPascalScriptIDE.CreateSynEditTextHighlighters;

const
  cNumber = $000080FF;
  cString = $00808080;
  cComment = $00008000;
  cSection = $00FF0080;
  cInstructionWord = $00FF0000;

  function NewSynEdit(
    AHighlighter: TSynCustomHighlighter;
    AParent: TWinControl
  ): TSynEdit;
  begin
    Result := TSynEdit.Create(Self);
    with Result do begin
      Parent := AParent;
      Align := alClient;
      Gutter.Visible := True;
      Gutter.ShowLineNumbers := True;
      Gutter.AutoSize := True;
      Gutter.UseFontStyle := False;
      Gutter.DigitCount := 2;
      Gutter.GradientStartColor := clBtnFace;
      Highlighter := AHighlighter;
      ReadOnly := False;
      ScrollBars := ssBoth;
      FontSmoothing := fsmNone;
      WordWrap := True;
      DoubleBuffered := True;
    end;
  end;

  function BuildSynPas: TSynPasSyn;
  begin
    Result := TSynPasSyn.Create(Self);
    with Result do begin
      CommentAttri.Foreground := cComment;
      KeyAttri.Foreground := cInstructionWord;
      NumberAttri.Foreground := cNumber;
      FloatAttri.Foreground := cNumber;
      HexAttri.Foreground := cNumber;
      StringAttri.Foreground := cString;
      CharAttri.Foreground := cString;
    end;
  end;

  function BuildSynIni: TSynIniSyn;
  begin
    Result := TSynIniSyn.Create(Self);
    with Result do begin
      CommentAttri.Foreground := cComment;
      SectionAttri.Foreground := cSection;
      NumberAttri.Foreground := cNumber;
      StringAttri.Foreground := cString;
    end;
  end;

begin
  synedtScript := NewSynEdit(BuildSynPas, pnlScriptEditor);
  synedtScript.Tag := 1;
  synedtScript.OnStatusChange := Self.OnSynEditStatusChange;

  synedtParams := NewSynEdit(BuildSynIni, pnlParamsTxt);
  synedtParams.Tag := 2;
  synedtParams.OnStatusChange := Self.OnSynEditStatusChange;
end;

procedure TfrmPascalScriptIDE.OnSynEditStatusChange(Sender: TObject;
  Changes: TSynStatusChanges);
var
  VEdit: TSynEdit;
begin
  VEdit := TSynEdit(Sender);
  if scModified in Changes then begin
    FZmpModified := True;
  end;
  statEditor.Panels[0].Text :=
    'Ln : ' + IntToStr(VEdit.CaretY) + '    ' +
    'Col : ' + IntToStr(VEdit.CaretX);
end;

procedure TfrmPascalScriptIDE.tbxtmWordWrapClick(Sender: TObject);
begin
  synedtScript.WordWrap := tbxtmWordWrap.Checked;
  synedtParams.WordWrap := tbxtmWordWrap.Checked;
end;

procedure TfrmPascalScriptIDE.tbxtmParamsClick(Sender: TObject);
begin
  pgcMain.ActivePageIndex := 0;
end;

procedure TfrmPascalScriptIDE.tbxtmScriptClick(Sender: TObject);
begin
  pgcMain.ActivePageIndex := 1;
end;

procedure TfrmPascalScriptIDE.OnAppClosing;
begin
  Self.Close;
end;

procedure TfrmPascalScriptIDE.CancelOperation;
begin
  if Assigned(FCancelNotifierInternal) then begin
    FCancelNotifierInternal.NextOperation;
  end;
end;

procedure TfrmPascalScriptIDE.btnSetXYZClick(Sender: TObject);
var
  VPoint: TPoint;
  VScreenCenter: TDoublePoint;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := FViewPortState.GetStatic;

  VScreenCenter :=
    VLocalConverter.GeoConverter.LonLat2TilePosFloat(
      VLocalConverter.GetCenterLonLat,
      VLocalConverter.Zoom
    );

  VPoint := PointFromDoublePoint(VScreenCenter, prToTopLeft);

  edtGetX.Text := IntToStr(VPoint.X);
  edtGetY.Text := IntToStr(VPoint.Y);
  edtGetZ.Text := IntToStr(VLocalConverter.Zoom + 1);
end;

end.
