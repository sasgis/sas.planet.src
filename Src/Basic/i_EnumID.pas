unit i_EnumID;

interface

type
  IEnumID = interface(IUnknown)
    ['{3467515E-78E8-4F90-9578-879F3EB734FB}']
    function Next(
      celt: LongWord;
      out rgelt: Integer;
      out pceltFetched: LongWord
    ): HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out ppenum: IEnumID): HResult; stdcall;
  end;

implementation

end.
