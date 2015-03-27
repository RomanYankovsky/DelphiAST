unit deprecatedtype;

interface

type
  TFoo = record
  end deprecated 'Use TBar';

  TBar = record
  end deprecated;

implementation

end.