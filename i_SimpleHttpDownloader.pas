unit i_SimpleHttpDownloader;

interface

type
  ISimpleHttpDownloader = interface
    ['{535D6FCC-4CD1-4ACD-B716-7385AF206AC6}']
    function DoHttpRequest(
      const ARequestUrl, ARequestHeader, APostData: string;
      out AResponseHeader, AResponseData: string
    ): Cardinal;
  end;

implementation

end.
