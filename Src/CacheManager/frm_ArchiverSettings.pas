unit frm_ArchiverSettings;

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
  fr_ArchiveWriteZipConfig,
  i_ArchiveReadWriteConfig,
  i_LanguageManager,
  u_CommonFormAndFrameParents;

type
  TfrmArchiverSettings = class(TCommonFormParent)
    btnApply: TButton;
    btnCancel: TButton;
    pnlMain: TPanel;
    pnlBottom: TPanel;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FWriterConfig: IArchiveWriteConfig;
    FfrArchiveWriteZipConfig: TfrArchiveWriteZipConfig;
  public
    function GetWriterConfig: IArchiveWriteConfig;
  public
    constructor Create(
      AOwner: TComponent;
      const ALanguageManager: ILanguageManager
    ); reintroduce;
    destructor Destroy; override;
  end;

implementation

{$R *.dfm}

{ TfrmArchiverSettings }

constructor TfrmArchiverSettings.Create(
  AOwner: TComponent;
  const ALanguageManager: ILanguageManager
);
begin
  inherited Create(AOwner);
  FfrArchiveWriteZipConfig := TfrArchiveWriteZipConfig.Create(ALanguageManager);
  FfrArchiveWriteZipConfig.Parent := pnlMain;
end;

destructor TfrmArchiverSettings.Destroy;
begin
  FfrArchiveWriteZipConfig.Free;
  inherited;
end;

procedure TfrmArchiverSettings.FormShow(Sender: TObject);
begin
  (FfrArchiveWriteZipConfig as IArchiveWriteConfigFrame).Reset(FWriterConfig);
end;

procedure TfrmArchiverSettings.btnApplyClick(Sender: TObject);
begin
  FWriterConfig := (FfrArchiveWriteZipConfig as IArchiveWriteConfigFrame).GetWriteConfig;
  Close;
end;

procedure TfrmArchiverSettings.btnCancelClick(Sender: TObject);
begin
  Close;
end;

function TfrmArchiverSettings.GetWriterConfig: IArchiveWriteConfig;
begin
  Result := FWriterConfig;
end;

end.
