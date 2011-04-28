unit u_BackgroundTaskLayerDrawBase;

interface

uses
  GR32,
  i_LocalCoordConverter,
  i_BackgroundTaskLayerDraw,
  u_BackgroundTask;

type
  TBackgroundTaskLayerDrawBase = class(TBackgroundTask, IBackgroundTaskLayerDraw)
  private
    FBitmap: TCustomBitmap32;
    FConverter: ILocalCoordConverter;
  protected
    procedure DrawBitmap; virtual; abstract;
    procedure ExecuteTask; override;
    property Converter: ILocalCoordConverter read FConverter;
    property Bitmap: TCustomBitmap32 read FBitmap;
  protected
    procedure ChangePos(AConverter: ILocalCoordConverter);
  public
    constructor Create(ABitmap: TCustomBitmap32);
  end;

implementation

uses
  Types;

{ TBackgroundTaskLayerDrawBase }

constructor TBackgroundTaskLayerDrawBase.Create(ABitmap: TCustomBitmap32);
begin
  inherited Create;
  FBitmap := ABitmap;
end;

procedure TBackgroundTaskLayerDrawBase.ChangePos(
  AConverter: ILocalCoordConverter);
begin
  StopExecute;
  FConverter := AConverter;
end;

procedure TBackgroundTaskLayerDrawBase.ExecuteTask;
begin
  inherited;
  if FConverter <> nil then begin
    DrawBitmap;
  end;
end;

end.
