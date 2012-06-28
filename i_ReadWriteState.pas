unit i_ReadWriteState;

interface
uses
  t_CommonTypes,
  i_Changeable;

type
  IReadWriteStateStatic = interface
    ['{E6BE17B1-BF3C-4313-B733-7B66A63EF684}']
    function GetReadAccess: TAccesState;
    property ReadAccess: TAccesState read GetReadAccess;

    function GetWriteAccess: TAccesState;
    property WriteAccess: TAccesState read GetWriteAccess;
  end;

  IReadWriteStateChangeble = interface(IChangeable)
    ['{6202AB73-00A2-4711-87F4-D195CEFD9C3F}']
    function GetStatic: IReadWriteStateStatic;
  end;

implementation

end.
