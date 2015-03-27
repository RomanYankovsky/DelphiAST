unit deprecatedtype;

interface

type
  TFoo = record
  end deprecated 'Use TBar';

  TBar = record
  end deprecated;

  TFooClass = class
  end deprecated 'Use TBarClass';

  TBarClass = class
  end deprecated;

implementation

end.
