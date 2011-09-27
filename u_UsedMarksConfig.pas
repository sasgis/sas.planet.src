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

unit u_UsedMarksConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_UsedMarksConfig,
  u_ConfigDataElementBase;

type
  TUsedMarksConfig = class(TConfigDataElementBase, IUsedMarksConfig)
  private
    FIsUseMarks: Boolean;
    FIgnoreMarksVisible: Boolean;
    FIgnoreCategoriesVisible: Boolean;
    FStatic: IUsedMarksConfigStatic;
    function CreateStatic: IUsedMarksConfigStatic;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetIsUseMarks: Boolean;
    procedure SetIsUseMarks(AValue: Boolean);

    function GetIgnoreMarksVisible: Boolean;
    procedure SetIgnoreMarksVisible(AValue: Boolean);

    function GetIgnoreCategoriesVisible: Boolean;
    procedure SetIgnoreCategoriesVisible(AValue: Boolean);

    function GetStatic: IUsedMarksConfigStatic;
  public
    constructor Create();
  end;

implementation

uses
  u_UsedMarksConfigStatic;

{ TUsedMarksConfig }

constructor TUsedMarksConfig.Create;
begin
  inherited;
  FIsUseMarks := True;
  FIgnoreMarksVisible := False;
  FIgnoreCategoriesVisible := False;
  SetChanged;
end;

function TUsedMarksConfig.CreateStatic: IUsedMarksConfigStatic;
begin
  Result :=
    TUsedMarksConfigStatic.Create(
      FIsUseMarks,
      FIgnoreMarksVisible,
      FIgnoreCategoriesVisible
    );
end;

procedure TUsedMarksConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TUsedMarksConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsUseMarks := AConfigData.ReadBool('IsUseMarks', FIsUseMarks);
    FIgnoreCategoriesVisible := AConfigData.ReadBool('IgnoreCategoriesVisible', FIgnoreCategoriesVisible);
    FIgnoreMarksVisible := AConfigData.ReadBool('IgnoreMarksVisible', FIgnoreMarksVisible);
    SetChanged;
  end;
end;

procedure TUsedMarksConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('IsUseMarks', FIsUseMarks);
  AConfigData.WriteBool('IgnoreCategoriesVisible', FIgnoreCategoriesVisible);
  AConfigData.WriteBool('IgnoreMarksVisible', FIgnoreMarksVisible);
end;

function TUsedMarksConfig.GetIgnoreCategoriesVisible: Boolean;
begin
  LockRead;
  try
    Result := FIgnoreCategoriesVisible;
  finally
    UnlockRead;
  end;
end;

function TUsedMarksConfig.GetIgnoreMarksVisible: Boolean;
begin
  LockRead;
  try
    Result := FIgnoreMarksVisible;
  finally
    UnlockRead;
  end;
end;

function TUsedMarksConfig.GetIsUseMarks: Boolean;
begin
  LockRead;
  try
    Result := FIsUseMarks;
  finally
    UnlockRead;
  end;
end;

function TUsedMarksConfig.GetStatic: IUsedMarksConfigStatic;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TUsedMarksConfig.SetIgnoreCategoriesVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FIgnoreCategoriesVisible <> AValue then begin
      FIgnoreCategoriesVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TUsedMarksConfig.SetIgnoreMarksVisible(AValue: Boolean);
begin
  LockWrite;
  try
    if FIgnoreMarksVisible <> AValue then begin
      FIgnoreMarksVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TUsedMarksConfig.SetIsUseMarks(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsUseMarks <> AValue then begin
      FIsUseMarks := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
