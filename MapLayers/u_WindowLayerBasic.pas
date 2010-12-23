unit u_WindowLayerBasic;

interface

uses
  Windows,
  SyncObjs,
  GR32,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider;

type
  TWindowLayerAbstract = class
  private
    FCS: TCriticalSection;
    FRedrawCounter: Cardinal;
    FRedrawTime: TDateTime;
  protected
    procedure IncRedrawCounter(ATime: TDateTime);
    function GetVisible: Boolean; virtual; abstract;
  public
    constructor Create();
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); virtual; abstract;
    procedure StartThreads; virtual; abstract;
    procedure SendTerminateToThreads; virtual; abstract;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); virtual; abstract;
    procedure Redraw; virtual; abstract;
    property Visible: Boolean read GetVisible;
    property RedrawCounter: Cardinal read FRedrawCounter;
    property RedrawTime: TDateTime read FRedrawTime;
  end;

implementation

uses
  SysUtils,
  Forms,
  Types;

{ TWindowLayerAbstract }

constructor TWindowLayerAbstract.Create;
begin
  FCS := TCriticalSection.Create;
  FRedrawCounter := 0;
  FRedrawTime  := 0;
end;

destructor TWindowLayerAbstract.Destroy;
begin
  FreeAndNil(FCS);
  inherited;
end;

procedure TWindowLayerAbstract.IncRedrawCounter(ATime: TDateTime);
begin
  FCS.Acquire;
  try
    Inc(FRedrawCounter);
    FRedrawTime := FRedrawTime + ATime;
  finally
    FCS.Release;
  end;
end;

end.
