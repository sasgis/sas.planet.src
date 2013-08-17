unit u_MarkSystemImplChangeable;

interface

uses
  i_PathConfig,
  i_Listener,
  i_MarkPicture,
  i_HashFunction,
  i_VectorItemsFactory,
  i_NotifierOperation,
  i_InternalPerformanceCounter,
  i_HtmlToHintTextConverter,
  i_ReadWriteState,
  i_MarkSystemImpl,
  i_MarkSystemImplChangeable,
  u_ConfigDataElementBase,
  u_ReadWriteStateInternalByOther;

type
  TMarkSystemImplChangeable = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IMarkSystemImplChangeable)
  private
    FBasePath: IPathConfig;
    FMarkPictureList: IMarkPictureList;
    FHashFunction: IHashFunction;
    FVectorItemsFactory: IVectorItemsFactory;
    FHintConverter: IHtmlToHintTextConverter;
    FLoadDbCounter: IInternalPerformanceCounter;
    FSaveDbCounter: IInternalPerformanceCounter;
    FAppStartedNotifier: INotifierOneOperation;

    FState: IReadWriteStateChangeble;
    FStateInternal: IReadWriteStateInternalByOther;

    FPathChangeListener: IListener;
    FAppStartedListener: IListener;
  private
    procedure OnPathChanged;
    procedure OnAppStarted;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetState: IReadWriteStateChangeble;
    function GetStatic: IMarkSystemImpl;
  public
    constructor Create(
      const ABasePath: IPathConfig;
      const AMarkPictureList: IMarkPictureList;
      const AHashFunction: IHashFunction;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ALoadDbCounter: IInternalPerformanceCounter;
      const ASaveDbCounter: IInternalPerformanceCounter;
      const AAppStartedNotifier: INotifierOneOperation;
      const AHintConverter: IHtmlToHintTextConverter
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_MarkSystemSml;

{ TMarksSystemImplChangeable }

constructor TMarkSystemImplChangeable.Create(
  const ABasePath: IPathConfig;
  const AMarkPictureList: IMarkPictureList;
  const AHashFunction: IHashFunction;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ALoadDbCounter: IInternalPerformanceCounter;
  const ASaveDbCounter: IInternalPerformanceCounter;
  const AAppStartedNotifier: INotifierOneOperation;
  const AHintConverter: IHtmlToHintTextConverter
);
begin
  Assert(ABasePath <> nil);
  inherited Create;
  FBasePath := ABasePath;
  FMarkPictureList := AMarkPictureList;
  FHashFunction := AHashFunction;
  FVectorItemsFactory := AVectorItemsFactory;
  FLoadDbCounter := ALoadDbCounter;
  FSaveDbCounter := ASaveDbCounter;
  FHintConverter := AHintConverter;
  FAppStartedNotifier := AAppStartedNotifier;
  FStateInternal := TReadWriteStateInternalByOther.Create;
  FState := FStateInternal;

  FPathChangeListener := TNotifyNoMmgEventListener.Create(Self.OnPathChanged);
  FBasePath.ChangeNotifier.Add(FPathChangeListener);

  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppStartedNotifier.Add(FAppStartedListener);
  OnAppStarted;
end;

destructor TMarkSystemImplChangeable.Destroy;
begin
  if Assigned(FBasePath) and Assigned(FPathChangeListener) then begin
    FBasePath.ChangeNotifier.Remove(FPathChangeListener);
    FBasePath := nil;
  end;
  if Assigned(FAppStartedNotifier) and Assigned(FAppStartedListener) then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedNotifier := nil;
  end;
  inherited;
end;

function TMarkSystemImplChangeable.CreateStatic: IInterface;
var
  VStatic: IMarkSystemImpl;
begin
  VStatic := nil;
  if FAppStartedNotifier.IsExecuted then begin
    VStatic :=
      TMarkSystemSml.Create(
        FBasePath.FullPath,
        FMarkPictureList,
        FHashFunction,
        FVectorItemsFactory,
        FLoadDbCounter,
        FSaveDbCounter,
        FHintConverter
      );
  end;
  if VStatic <> nil then begin
    FStateInternal.SetOther(VStatic.State);
  end else begin
    FStateInternal.SetOther(nil);
  end;
  Result := VStatic;
end;

function TMarkSystemImplChangeable.GetState: IReadWriteStateChangeble;
begin
  Result := FState;
end;

function TMarkSystemImplChangeable.GetStatic: IMarkSystemImpl;
begin
  Result := IMarkSystemImpl(GetStaticInternal);
end;

procedure TMarkSystemImplChangeable.OnAppStarted;
begin
  if (FAppStartedNotifier <> nil) and FAppStartedNotifier.IsExecuted then begin
    LockWrite;
    try
      SetChanged;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TMarkSystemImplChangeable.OnPathChanged;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
