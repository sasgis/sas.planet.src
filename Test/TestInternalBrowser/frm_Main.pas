unit frm_Main;

interface

uses
  Winapi.Windows,
  Winapi.ShellAPI,
  System.SysUtils,
  System.Classes,
  System.Diagnostics,
  System.IOUtils,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  i_PathConfig,
  i_InetConfig,
  i_ContentTypeManager,
  i_InternalDomainUrlHandler,
  i_InternalDomainInfoProvider,
  i_InternalBrowserLastContent,
  i_InternalBrowserFactory,
  u_InternalBrowserImpl;

type
  TfrmMain = class(TForm)
    pnlTop: TPanel;
    pnlBrowser: TPanel;
    pnlBottom: TPanel;
    cbbTestMethod: TComboBox;
    cbbSource: TComboBox;
    btnRun: TButton;
    mmoLog: TMemo;
    rbIE: TRadioButton;
    rbEdgeSystem: TRadioButton;
    rbEdgePortable: TRadioButton;
    lnklblDownloadEdgeRuntime: TLinkLabel;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OnEngineChange(Sender: TObject);
    procedure cbbSourceKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lnklblDownloadEdgeRuntimeLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
  private
    FBrowser: TInternalBrowserImpl;
    FBrowserId: string;

    FInetConfig: IInetConfig;
    FInternalUrlHandler: IInternalDomainUrlHandler;
    FInternalDomainInfoProviderList: IInternalDomainInfoProviderList;

    FInternalBrowserFactory: IInternalBrowserFactory;

    FTestDataPath: string;
    FMediaDataPath: IPathConfig;
    FInternalBrowserContent: IInternalBrowserLastContent;

    function CreateContentTypeManager: IContentTypeManager;
    function CreateInternalDomainInfoProviderList(const AContentTypeManager: IContentTypeManager): IInternalDomainInfoProviderList;

    procedure DoCreateBrowser;
    procedure DoCreateBrowserFactory;

    procedure OnBrowserKeyDown(ASender: TObject; const AKey: Word; var AHandled: Boolean);
    procedure OnBrowserTitleChange(ASender: TObject; const AText: string);

    procedure ToLogFmt(const AFmt: string; AArgs: array of const);
  public
    destructor Destroy; override;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  c_InternalBrowser,
  u_GlobalDllName,
  u_PathConfig,
  u_Synchronizer,
  u_NotifierTime,
  u_HashFunctionByImpl,
  u_HashFunctionCityHash,
  u_Bitmap32BufferFactory,
  u_Bitmap32StaticFactory,
  u_BitmapTileSaveLoadFactory,
  u_GeometryLonLatFactory,
  u_ProjConverterFactory,
  u_ArchiveReadWriteFactory,
  u_AppearanceOfMarkFactory,
  u_VectorItemSubsetBuilder,
  u_VectorDataFactorySimple,
  u_ContentTypeManagerSimple,
  u_ConfigDataProviderByPathConfig,
  u_InetConfig,
  u_InternalPerformanceCounterFake,
  u_InternalDomainInfoProviderList,
  u_InternalDomainInfoProviderByLastContent,
  u_InternalDomainInfoProviderByDataProvider,
  u_InternalDomainUrlHandler,
  u_InternalDomainUrlHandlerConfig,
  u_InternalBrowserLastContent,
  u_InternalBrowserFactory;

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  GDllName.Init;

  FTestDataPath := ExtractFilePath(ParamStr(0)) + '..\..\Test\TestInternalBrowser\Data\';

  rbEdgePortable.Checked := True;

  cbbSource.Items.Clear;
  cbbSource.Items.Add(CEmptyDocument);

  for var VFileName in TDirectory.GetFiles(FTestDataPath + 'Html\', '*.html') do begin
    cbbSource.Items.Add(ExtractFileName(VFileName));
  end;

  cbbSource.ItemIndex := 0;

  FMediaDataPath := TPathConfig.Create('PathToMediaData', FTestDataPath + 'MediaData\', nil);

  FInternalBrowserContent := TInternalBrowserLastContent.Create;

  FInternalUrlHandler :=
    TInternalDomainUrlHandler.Create(
      TInternalDomainUrlHandlerConfig.Create,
      FMediaDataPath
    );

  var VContentTypeManager := CreateContentTypeManager;
  FInternalDomainInfoProviderList := CreateInternalDomainInfoProviderList(VContentTypeManager);

  FInetConfig := TInetConfig.Create;
end;

destructor TfrmMain.Destroy;
begin
  FreeAndNil(FBrowser);
  FInternalBrowserFactory := nil;
  inherited;
end;

function TfrmMain.CreateContentTypeManager: IContentTypeManager;
begin
  var VCounter := TInternalPerformanceCounterFake.Create;
  var VHashFunction := THashFunctionByImpl.Create(THashFunctionCityHash.Create);
  var VVectorGeometryLonLatFactory := TGeometryLonLatFactory.Create(VCounter, VHashFunction);
  var VVectorDataFactory := TVectorDataFactorySimple.Create(VHashFunction);
  var VAppearanceOfMarkFactory := TAppearanceOfMarkFactory.Create(VCounter, VHashFunction);
  var VVectorItemSubsetBuilderFactory := TVectorItemSubsetBuilderFactory.Create(VHashFunction);
  var VProjConverterFactory := TProjConverterFactory.Create;
  var VArchiveReadWriteFactory := TArchiveReadWriteFactory.Create;

  var VBGTimerNotifier := TNotifierTime.Create(GSync.SyncVariable.Make('BGTimerNotifier'));
  var VBufferFactory := TBitmap32BufferFactory.Create(VBGTimerNotifier, GSync.SyncVariable.Make(Self.ClassName));
  var VBitmap32StaticFactory := TBitmap32StaticFactory.Create(VHashFunction, VBufferFactory);
  var VBitmapTileSaveLoadFactory := TBitmapTileSaveLoadFactory.Create(VBitmap32StaticFactory);
  var VContentTypeManagerBitmapInternal := TContentTypeManagerBitmap.Create(VBitmapTileSaveLoadFactory, VCounter);

  Result :=
    TContentTypeManagerSimple.Create(
      VVectorGeometryLonLatFactory,
      VVectorDataFactory,
      VAppearanceOfMarkFactory,
      nil, // IMarkPictureList
      VVectorItemSubsetBuilderFactory,
      VContentTypeManagerBitmapInternal,
      VArchiveReadWriteFactory,
      VProjConverterFactory
    );
end;

function TfrmMain.CreateInternalDomainInfoProviderList(const AContentTypeManager: IContentTypeManager): IInternalDomainInfoProviderList;
var
  VInternalDomainInfoProviderList: TInternalDomainInfoProviderList;
  VInternalDomainInfoProvider: IInternalDomainInfoProvider;
//  VTextProivder: ITextByVectorItem;
//  VTextProviderList: TStringList;
begin
  Result := TInternalDomainInfoProviderList.Create;

  VInternalDomainInfoProviderList := TInternalDomainInfoProviderList(Result);

//  VInternalDomainInfoProvider :=
//    TInternalDomainInfoProviderByMapTypeList.Create(
//      FZmpInfoSet,
//      AContentTypeManager
//    );
//
//  VInternalDomainInfoProviderList.Add(
//    CZmpInfoInternalDomain,
//    VInternalDomainInfoProvider
//  );

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByDataProvider.Create(
      TConfigDataProviderByPathConfig.Create(FMediaDataPath),
      AContentTypeManager
    );
  VInternalDomainInfoProviderList.Add(
    CMediaDataInternalDomain,
    VInternalDomainInfoProvider
  );

//  VTextProviderList := TStringList.Create;
//  VTextProviderList.Sorted := True;
//  VTextProviderList.Duplicates := dupError;
//  VTextProivder := TTextByVectorItemHTMLByDescription.Create;
//
//  VTextProviderList.AddObject(CVectorItemInfoSuffix, Pointer(VTextProivder));
//  VTextProivder._AddRef;
//
//  VTextProviderList.AddObject(CVectorItemDescriptionSuffix, Pointer(VTextProivder));
//  VTextProivder._AddRef;
//
//  VInternalDomainInfoProvider :=
//    TInternalDomainInfoProviderByMarksSystem.Create(
//      FMarkSystem,
//      VTextProivder,
//      VTextProviderList
//    );
//  VInternalDomainInfoProviderList.Add(
//    CMarksSystemInternalDomain,
//    VInternalDomainInfoProvider
//  );
//
//  VInternalDomainInfoProvider :=
//    TInternalDomainInfoProviderByLastSearchResults.Create(
//      FLastSearchResult,
//      VTextProivder,
//      nil
//    );
//  VInternalDomainInfoProviderList.Add(
//    CLastSearchResultsInternalDomain,
//    VInternalDomainInfoProvider
//  );

  VInternalDomainInfoProvider :=
    TInternalDomainInfoProviderByLastContent.Create(
      FInternalBrowserContent
    );
  VInternalDomainInfoProviderList.Add(
    CShowMessageDomain,
    VInternalDomainInfoProvider
  );

//  VInternalDomainInfoProvider :=
//    TInternalDomainInfoProviderByMapData.Create(
//      FMainMapsList.FullMapsSetChangeable,
//      VTextProivder,
//      CVectorItemDescriptionSuffix
//    );
//
//  VInternalDomainInfoProviderList.Add(
//    CMapDataInternalDomain,
//    VInternalDomainInfoProvider
//  );
//
//  VInternalDomainInfoProvider :=
//    TInternalDomainInfoProviderByTileStorageOptions.Create(
//      FMainMapsList.FullMapsSetChangeable
//    );
//
//  VInternalDomainInfoProviderList.Add(
//    CTileStorageOptionsInternalDomain,
//    VInternalDomainInfoProvider
//  );
end;

procedure TfrmMain.DoCreateBrowserFactory;
begin
  if Assigned(FInternalBrowserFactory) then begin
    raise Exception.Create('FInternalBrowserFactory already assigned!');
  end;

  FInternalBrowserFactory :=
    TInternalBrowserFactory.Create(
      FInetConfig,
      FInternalUrlHandler,
      FInternalDomainInfoProviderList
    );
end;

procedure TfrmMain.DoCreateBrowser;
begin
  if Assigned(FBrowser) then begin
    Exit;
  end;

  DoCreateBrowserFactory;

  if rbIE.Checked then begin
    FBrowserId := 'IE';
    FInetConfig.BrowserEngineType := beInternetExplorer;
  end else
  if rbEdgeSystem.Checked then begin
    FBrowserId := 'Edge Sys';
    FInetConfig.BrowserEngineType := beEdgeSystem;
  end else
  if rbEdgePortable.Checked then begin
    FBrowserId := 'Edge Port';
    FInetConfig.BrowserEngineType := beEdgePortable;
  end else begin
    Assert(False);
    FBrowserId := '';
  end;

  FInetConfig.PreInitBrowserEngine := True;

  var VStopwatch := TStopwatch.StartNew;

  FBrowser :=
    FInternalBrowserFactory.CreateBrowser(
      pnlBrowser,
      Self.OnBrowserKeyDown,
      Self.OnBrowserTitleChange
    );

  VStopwatch.Stop;
  ToLogFmt('Init in %.6f sec', [VStopwatch.ElapsedMilliseconds / 1000]);
end;

procedure TfrmMain.btnRunClick(Sender: TObject);

  function GetSourceUrl: string;
  begin
    Result := cbbSource.Text;
    if SameText(Result, CEmptyDocument) then Exit;
    if Pos('://', Result) > 0 then Exit;
    if SameText(ExtractFileExt(Result), '.html') then begin
      Result := ExpandFileName(FTestDataPath + 'Html\' + Result);
    end;
  end;

  procedure DoPrepareBrowserContent;
  begin
    var VFileName := GetSourceUrl;
    if FileExists(VFileName) then begin
      FInternalBrowserContent.Content := TFile.ReadAllText(VFileName);
    end else begin
      FInternalBrowserContent.Content := 'File not found: ' + VFileName;
    end;
  end;

begin
  DoCreateBrowser;

  if cbbTestMethod.ItemIndex = 2 then begin
    DoPrepareBrowserContent;
  end;

  FBrowser.SetVisible(True);

  var VStopwatch := TStopwatch.StartNew;

  case cbbTestMethod.ItemIndex of
    0: FBrowser.Navigate(GetSourceUrl);
    1: FBrowser.NavigateWait(GetSourceUrl, INFINITE);
    2: FBrowser.Navigate(CShowMessageInternalURL);
  end;

  VStopwatch.Stop;
  ToLogFmt('Run in %.6f sec', [VStopwatch.ElapsedMilliseconds / 1000]);
end;

procedure TfrmMain.OnBrowserKeyDown(ASender: TObject; const AKey: Word; var AHandled: Boolean);
begin
  ToLogFmt('OnKeyDown: Key = %d', [AKey]);
end;

procedure TfrmMain.OnBrowserTitleChange(ASender: TObject; const AText: string);
begin
  Self.Caption := AText;
end;

procedure TfrmMain.OnEngineChange(Sender: TObject);
begin
  FreeAndNil(FBrowser);
  FInternalBrowserFactory := nil;
end;

procedure TfrmMain.cbbSourceKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_RETURN then btnRunClick(Sender);
end;

procedure TfrmMain.lnklblDownloadEdgeRuntimeLinkClick(Sender: TObject; const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(0, 'Open', PChar(Link), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmMain.ToLogFmt(const AFmt: string; AArgs: array of const);
begin
  var VTimeStamp := FormatDateTime('hh:nn:ss.zzz', Now);
  mmoLog.Lines.Add(Format('%s [%s]', [VTimeStamp, FBrowserId]) + ' ' + Format(AFmt,  AArgs) );
end;

end.
