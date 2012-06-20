unit i_MainFormState;

interface

uses
  i_Changeable;

type
  TStateEnum = (
    ao_movemap,
    ao_edit_point,
    ao_edit_line,
    ao_edit_poly,
    ao_calc_line,
    ao_select_rect,
    ao_select_poly,
    ao_select_line
  );

type
  IMainFormState = interface(IChangeable)
    ['{0CB21E1F-BBFC-4517-A328-40F36E6C1457}']
    function GetState: TStateEnum;
    procedure SetState(AValue: TStateEnum);
    property State: TStateEnum read GetState write SetState;
  end;

implementation

end.
