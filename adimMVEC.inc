{$IFDEF INTF_QUANTITY}
{$UNDEF INTF_QUANTITY}
type
  TQuantity = record
  private
    FValue: TMultivector;
  public
    function Value: TMultivector;
    function Value(const APrefixes: TPrefixes): TMultivector;
    function ToString: string;
    function ToString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function ToVerboseString: string;
    function ToVerboseString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function IsSame(const AValue: TQuantity): boolean;
    function IsNull: boolean;
    function Inverse: TQuantity;
    function Reverse: TQuantity;
    function Conjugate: TQuantity;
    function Projection(const AValue: TMultivector): TQuantity;
    function Rejection(const AValue: TMultivector): TQuantity;
    function Extract(AComponents: TMultivectorComponents): TQuantity;
    {$IFNDEF NOP}
    class operator + (const AValue: TQuantity): TQuantity;
    class operator - (const AValue: TQuantity): TQuantity;
    class operator + (const ALeft, ARight: TQuantity): TQuantity;
    class operator - (const ALeft, ARight: TQuantity): TQuantity;
    class operator * (const AValue: double; const ASelf: TQuantity): TQuantity;
    class operator * (const ASelf: TQuantity; const AValue: double): TQuantity;
    class operator / (const ASelf: TQuantity; const AValue: double): TQuantity;
    class operator = (const ALeft, ARight: TQuantity): boolean;
    class operator <>(const ALeft, ARight: TQuantity): boolean;
    {$ENDIF}{$UNDEF NOP}
{$ENDIF}

{$IFDEF INTF_END}
{$UNDEF INTF_END}
end;
{$ENDIF}

{$IFDEF IMPL_QUANTITY}
{$UNDEF IMPL_QUANTITY}

function TQuantity.Value: TMultivector;
begin
  result := FValue;
end;

function TQuantity.Value(const APrefixes: TPrefixes): TMultivector;
var
  I: longint;
  Exponent: longint;
  PrefixCount: longint;
begin
  PrefixCount := Length(APrefixes);
  if PrefixCount = Length(CPREFIXES) then
  begin
    Exponent := 0;
    for I := 0 to PrefixCount -1 do
      Inc(Exponent, PrefixTable[CPREFIXES[I]].Exponent * CEXPONENTS[I]);

    for I := 0 to PrefixCount -1 do
      Dec(Exponent, PrefixTable[APrefixes[I]].Exponent * CEXPONENTS[I]);

    if Exponent <> 0 then
      result := FValue * IntPower(10, Exponent)
    else
      result := FValue;
  end else
    if PrefixCount = 0 then
      result := FValue
    else
      raise Exception.Create('Wrong number of prefixes.');
end;

function TQuantity.ToString: string;
begin
  result := FloatToStr(FValue.Norm) + ' ' + GetSymbol(CSYMBOL, CPREFIXES);
end;

function TQuantity.ToVerboseString: string;
begin
  result := '(' + FValue.ToString + ') ' + GetSymbol(CSYMBOL, CPREFIXES);
end;

function TQuantity.ToString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
begin
  if Length(APrefixes) = 0 then
    result := FloatToStrF(Value           .Norm, ffGeneral, APrecision, ADigits) + ' ' + GetSymbol(CSYMBOL, CPREFIXES)
  else
    result := FloatToStrF(Value(APrefixes).Norm, ffGeneral, APrecision, ADigits) + ' ' + GetSymbol(CSYMBOL, APrefixes);
end;

function TQuantity.ToVerboseString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
begin
  if Length(APrefixes) = 0 then
    result := '(' + Value           .ToVerboseString(APrecision, ADigits) + ') ' + GetSymbol(CSYMBOL, CPREFIXES)
  else
    result := '(' + Value(APrefixes).ToVerboseString(APrecision, ADigits) + ') ' + GetSymbol(CSYMBOL, APrefixes);
end;

function TQuantity.IsSame(const AValue: TQuantity): boolean;
begin
  result := FValue.SameValue(AValue.FValue);
end;

function TQuantity.IsNull: boolean;
begin
  result := FValue.SameValue(NullMultivector);
end;

function TQuantity.Inverse: TQuantity;
begin
  result.FValue := FValue.Inverse;
end;

function TQuantity.Reverse: TQuantity;
begin
  result.FValue := FValue.Reverse;
end;

function TQuantity.Conjugate: TQuantity;
begin
  result.FValue := FValue.Conjugate;
end;

function TQuantity.Projection(const AValue: TMultivector): TQuantity;
begin
  result.FValue := FValue.Projection(AValue);
end;

function TQuantity.Rejection(const AValue: TMultivector): TQuantity;
begin
  result.FValue := FValue.Rejection(AValue);
end;

function TQuantity.Extract(AComponents: TMultivectorComponents): TQuantity;
begin
  result.FValue := FValue.Extract(AComponents);
end;

{$IFNDEF NOP}
class operator TQuantity.+(const AValue: TQuantity): TQuantity;
begin
  result.FValue := AValue.FValue;
end;

class operator TQuantity.-(const AValue: TQuantity): TQuantity;
begin
  result.FValue := -AValue.FValue;
end;

class operator TQuantity.+(const ALeft, ARight: TQuantity): TQuantity;
begin
  result.FValue := ALeft.FValue + ARight.FValue;
end;

class operator TQuantity.-(const ALeft, ARight: TQuantity): TQuantity;
begin
  result.FValue := ALeft.FValue - ARight.FValue;
end;

class operator TQuantity.*(const AValue: double; const ASelf: TQuantity): TQuantity;
begin
  result.FValue := AValue * ASelf.FValue;
end;

class operator TQuantity.*(const ASelf: TQuantity; const AValue: double): TQuantity;
begin
  result.FValue := ASelf.FValue * AValue;
end;

class operator TQuantity./(const ASelf: TQuantity; const AValue: double): TQuantity;
begin
  result.FValue := ASelf.FValue / AValue;
end;

class operator TQuantity.=(const ALeft, ARight: TQuantity): boolean;
begin
  result := ALeft.FValue = ARight.FValue;
end;

class operator TQuantity.<>(const ALeft, ARight: TQuantity): boolean;
begin
  result := ALeft.FValue <> ARight.FValue;
end;
{$ENDIF}{$UNDEF NOP}
{$ENDIF}

{$IFDEF INTF_UNIT}
{$UNDEF INTF_UNIT}
type
  TUnit = record
{$ENDIF}

{$IFDEF IMPL_UNIT}
{$UNDEF IMPL_UNIT}
{$ENDIF}
