unit t_CommonTypes;

interface

type
  TTileSource = (tsInternet,tsCache,tsCacheInternet);

  { Способ отображения расстояний
  dsfKmAndM - в виде 12 км 299 м
  dsfSimpleKM - в виед 12.299 км
  }
  TDistStrFormat = (dsfKmAndM = 0, dsfSimpleKM = 1);

  TDegrShowFormat = (dshCharDegrMinSec = 0, dshCharDegrMin = 1, dshCharDegr = 2, dshSignDegrMinSec = 3, dshSignDegrMin = 4, dshSignDegr = 5);

implementation

end.
