unit i_MarkCategoryFactory;

interface

uses
  i_MarkCategory;

type
  IMarkCategoryFactory = interface
    ['{A5AC81FF-F5FE-4085-8E9A-29318FDBBBA3}']
    function CreateNew(AName: string): IMarkCategory;
    function Modify(
      ASource: IMarkCategory;
      AName: string;
      AVisible: Boolean;
      AAfterScale: integer;
      ABeforeScale: integer
    ): IMarkCategory;
    function ModifyVisible(ASource: IMarkCategory; AVisible: Boolean): IMarkCategory;
  end;

implementation

end.
