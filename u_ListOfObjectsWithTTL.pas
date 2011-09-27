{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_ListOfObjectsWithTTL;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_ObjectWithTTL,
  i_ListOfObjectsWithTTL;

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
    function GetNextCheck: Cardinal;
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
  for i := 0 to FList.Count - 1 do begin
    IObjectWithTTL(FList.Items[i])._Release;
  end;
  FreeAndNil(FList);
  inherited;
end;

function TListOfObjectsWithTTL.GetNextCheck: Cardinal;
begin
  Result := FNextCheck;
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
    FSync.EndRead;
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
