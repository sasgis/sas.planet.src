unit u_TileDownloaderStateInternal;

interface

uses
  i_TileDownloaderState,
  i_ConfigDataElement,
  u_ConfigDataElementBase;

type
  ITileDownloaderStateInternal = interface(IConfigDataElement)
    ['{BD4A155B-92AC-4E61-8A1D-A0F8516E1340}']
    function GetEnabled: Boolean;
    property Enabled: Boolean read GetEnabled;

    function GetDisableReason: string;
    property DisableReason: string read GetDisableReason;

    procedure Disable(AReason: string);
    procedure Enable;

    function GetStatic: ITileDownloaderStateStatic;
  end;

type
  TTileDownloaderStateInternal = class(TConfigDataElementBaseEmptySaveLoad, ITileDownloaderStateInternal, ITileDownloaderStateChangeble)
  private
    FEnabled: Boolean;
    FReason: string;

    FStatic: ITileDownloaderStateStatic;
    function CreateStatic: ITileDownloaderStateStatic;
  protected
    procedure DoBeforeChangeNotify; override;
  protected
    function GetEnabled: Boolean;
    function GetDisableReason: string;

    procedure Disable(AReason: string);
    procedure Enable;

    function GetStatic: ITileDownloaderStateStatic;
  public
    constructor Create;
  end;


implementation

uses
  u_TileDownloaderStateStatic;

{ TTileDownloaderStateInternal }

constructor TTileDownloaderStateInternal.Create;
begin
  inherited;
  FEnabled := True;
  FStatic := CreateStatic;
end;

function TTileDownloaderStateInternal.CreateStatic: ITileDownloaderStateStatic;
begin
  Result :=
    TTileDownloaderStateStatic.Create(
      FEnabled,
      FReason
    );
end;

procedure TTileDownloaderStateInternal.Disable(AReason: string);
begin
  LockWrite;
  try
    if FEnabled then begin
      FEnabled := False;
      FReason := AReason;
      SetChanged;
    end;
  finally
    UnlockWrite
  end;
end;

procedure TTileDownloaderStateInternal.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TTileDownloaderStateInternal.Enable;
begin
  LockWrite;
  try
    if not FEnabled then begin
      FEnabled := True;
      FReason := '';
      SetChanged;
    end;
  finally
    UnlockWrite
  end;
end;

function TTileDownloaderStateInternal.GetDisableReason: string;
begin
  LockRead;
  try
    if FEnabled then begin
      Result := '';
    end else begin
      Result := FReason;
    end;
  finally
    UnlockRead;
  end;
end;

function TTileDownloaderStateInternal.GetEnabled: Boolean;
begin
  LockRead;
  try
    Result := FEnabled;
  finally
    UnlockRead;
  end;
end;

function TTileDownloaderStateInternal.GetStatic: ITileDownloaderStateStatic;
begin
  Result := FStatic
end;

end.
