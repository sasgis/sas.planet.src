unit u_ShortCutModalEditByForm;

interface

uses
  Windows,
  i_LanguageManager,
  i_ShortCutSingleConfig,
  i_ShortCutModalEdit,
  u_BaseInterfacedObject,
  frm_ShortCutEdit;

type
  TShortCutModalEditByForm = class(TBaseInterfacedObject, IShortCutModalEdit)
  private
    FLanguageManager: ILanguageManager;
    FEditCounter: Longint;
    FfrmShortCutEdit: TfrmShortCutEdit;
  private
    function EditShortCut(const AShortCutInfo: IShortCutSingleConfig): Boolean;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TShortCutModalEditByForm }

constructor TShortCutModalEditByForm.Create(
  const ALanguageManager: ILanguageManager
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FEditCounter := 0;
end;

destructor TShortCutModalEditByForm.Destroy;
begin
  Assert(FEditCounter = 0);
  if FfrmShortCutEdit <> nil then begin
    FreeAndNil(FfrmShortCutEdit);
  end;

  inherited;
end;

function TShortCutModalEditByForm.EditShortCut(
  const AShortCutInfo: IShortCutSingleConfig
): Boolean;
var
  VCounter: Longint;
begin
  VCounter := InterlockedIncrement(FEditCounter);
  try
    if VCounter = 1 then begin
      if FfrmShortCutEdit = nil then begin
        FfrmShortCutEdit := TfrmShortCutEdit.Create(FLanguageManager);
      end;
      Result := FfrmShortCutEdit.EditHotKeyModal(AShortCutInfo);
    end else begin
      Result := False;
    end;
  finally
    InterlockedDecrement(FEditCounter);
  end;
end;

end.
