unit u_ListOfObjectsWithTTL;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_IObjectWithTTL,
  i_IListOfObjectsWithTTL;

type
  TListOfObjectsWithTTL = class(TInterfacedObject, IListOfObjectsWithTTL)
  private
    FList: TList;
    FSync: IReadWriteSync;
    FNextCheck: Cardinal;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddObject(AObj: IObjectWithTTL);
    procedure RemoveObject(AObj: IObjectWithTTL);
    procedure ProcessObjectsTrim;
  end;

implementation

{ TListOfObjectsWithTTL }

procedure TListOfObjectsWithTTL.AddObject(AObj: IObjectWithTTL);
begin
  FSync.BeginWrite;
  try
    AObj._AddRef;
    FList.Add(Pointer(AObj));
  finally
    FSync.EndWrite;
  end;
end;

constructor TListOfObjectsWithTTL.Create;
begin
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FList := TList.Create;
end;

destructor TListOfObjectsWithTTL.Destroy;
var
  i: integer;
begin
  FSync := nil;
  for i:= 0 to FList.Count - 1 do begin
    IObjectWithTTL(FList.Items[i])._Release;
  end;
  FreeAndNil(FList);
  inherited;
end;

procedure TListOfObjectsWithTTL.ProcessObjectsTrim;
var
  i: integer;
  VNow: Cardinal;
  VObj: IObjectWithTTL;
  VNextCheck: Cardinal;
  VObjNextCheck: Cardinal;
begin
  VNow := GetTickCount;
  VNextCheck := 0;
  FSync.BeginRead;
  try
    for i := 0 to FList.Count - 1 do begin
      VObj := IObjectWithTTL(FList.Items[i]);
      VObjNextCheck := VObj.GetNextCheckTime;
      if (VObjNextCheck <= VNow) or ((VNow < 1 shl 29) and (VObjNextCheck > 1 shl 30)) then begin
        VObj.TrimByTTL;
        VObjNextCheck := VObj.GetNextCheckTime;
      end;
      if (VNextCheck <= 0) or (VNextCheck > VObjNextCheck) then begin
        VNextCheck := VObjNextCheck;
      end;
    end;
    FNextCheck := VNextCheck;
  finally
    FSync.BeginWrite;
  end;
end;

procedure TListOfObjectsWithTTL.RemoveObject(AObj: IObjectWithTTL);
begin
  FSync.BeginWrite;
  try
    FList.Remove(Pointer(AObj));
    AObj._Release;
  finally
    FSync.EndWrite;
  end;
end;

end.
