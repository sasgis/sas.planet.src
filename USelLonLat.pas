unit USelLonLat;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  Forms,
  Dialogs,
  StdCtrls,
  ExtCtrls,
  u_CommonFormAndFrameParents,
  t_GeoTypes,
  fr_LonLat;

type
  TFSelLonLat = class(TCommonFormParent)
    Button1: TButton;
    Button2: TButton;
    grpTopLeft: TGroupBox;
    grpBottomRight: TGroupBox;
    pnlBottomButtons: TPanel;
    grdpnlMain: TGridPanel;
    procedure FormShow(Sender: TObject);
  private
    FfrLonLatTopLeft: TfrLonLat;
    FfrLonLatBottomRight: TfrLonLat;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(var ALonLatRect: TExtendedRect): Boolean;
    procedure RefreshTranslation; override;
  end;

implementation

uses
  UResStrings;

{$R *.dfm}

constructor TFSelLonLat.Create(AOwner: TComponent);
begin
  inherited;
  FfrLonLatTopLeft := TfrLonLat.Create(nil);
  FfrLonLatBottomRight := TfrLonLat.Create(nil);
end;

destructor TFSelLonLat.Destroy;
begin
  FreeAndNil(FfrLonLatTopLeft);
  FreeAndNil(FfrLonLatBottomRight);
  inherited;
end;

function TFSelLonLat.Execute(var ALonLatRect: TExtendedRect): Boolean;
begin
  FfrLonLatTopLeft.LonLat := ALonLatRect.TopLeft;
  FfrLonLatBottomRight.LonLat := ALonLatRect.BottomRight;
  Result := ShowModal = mrOK;
  if Result then begin
    ALonLatRect.TopLeft := FfrLonLatTopLeft.LonLat;
    ALonLatRect.BottomRight := FfrLonLatBottomRight.LonLat;
    if (ALonLatRect.Left>ALonLatRect.Right)then begin
      ShowMessage(SAS_ERR_LonLat2);
      result:=false;
    end else if (ALonLatRect.Top < ALonLatRect.Bottom)then begin
      ShowMessage(SAS_ERR_LonLat1);
      result:=false;
    end;
  end;
end;

procedure TFSelLonLat.FormShow(Sender: TObject);
begin
  FfrLonLatTopLeft.Parent := grpTopLeft;
  FfrLonLatBottomRight.Parent := grpBottomRight;
end;

procedure TFSelLonLat.RefreshTranslation;
begin
  inherited;
  FfrLonLatTopLeft.RefreshTranslation;
  FfrLonLatBottomRight.RefreshTranslation;
end;

end.
