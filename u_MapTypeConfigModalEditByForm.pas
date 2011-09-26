unit u_MapTypeConfigModalEditByForm;

interface

uses
  Windows,
  i_MapTypeConfigModalEdit,
  u_MapType,
  frm_MapTypeEdit;

type
  TMapTypeConfigModalEditByForm = class(TInterfacedObject, IMapTypeConfigModalEdit)
  private
    FEditCounter: Longint;
    FfrmMapTypeEdit: TfrmMapTypeEdit;
  protected
    function EditMap(AMapType: TMapType): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TMapTypeConfigModalEditByForm }

constructor TMapTypeConfigModalEditByForm.Create;
begin
  FEditCounter := 0;
end;

destructor TMapTypeConfigModalEditByForm.Destroy;
begin
  Assert(FEditCounter = 0);
  if FfrmMapTypeEdit <> nil then begin
    FreeAndNil(FfrmMapTypeEdit);
  end;

  inherited;
end;

function TMapTypeConfigModalEditByForm.EditMap(AMapType: TMapType): Boolean;
var
  VCounter: Longint;
begin
  VCounter := InterlockedIncrement(FEditCounter);
  try
    if VCounter = 1 then begin
      if FfrmMapTypeEdit = nil then begin
        FfrmMapTypeEdit := TfrmMapTypeEdit.Create(nil);
      end;
      Result := FfrmMapTypeEdit.EditMapModadl(AMapType);
    end else begin
      Result := False;
    end;
  finally
    InterlockedDecrement(FEditCounter);
  end;
end;

end.
