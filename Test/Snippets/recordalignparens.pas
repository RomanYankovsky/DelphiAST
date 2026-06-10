unit recordalignparens;

interface

const
  REC_ALIGN = 16;

type
  TParenAligned = record
    Value: Integer;
  end align(16);

  TIdentAligned = record
    Value: Integer;
  end align(REC_ALIGN);

implementation

end.