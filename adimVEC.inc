{$IFDEF INTF_QUANTITY}
{$UNDEF INTF_QUANTITY}
type
  TQuantity = record
  private
    FValue: TVector;
  public
    function Value: TVector;
    function Value(const APrefixes: TPrefixes): TVector;
    function ToString: string;
    function ToString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function ToVerboseString: string;
    function ToVerboseString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function IsSame(const AValue: TQuantity): boolean;
    function IsZero: boolean;
    class operator + (const AValue: TQuantity): TQuantity;
    class operator - (const AValue: TQuantity): TQuantity;
    class operator + (const ALeft, ARight: TQuantity): TQuantity;
    class operator - (const ALeft, ARight: TQuantity): TQuantity;
    class operator * (const AValue: double; const ASelf: TQuantity): TQuantity;
    class operator * (const ASelf: TQuantity; const AValue: double): TQuantity;
    class operator / (const ASelf: TQuantity; const AValue: double): TQuantity;
    class operator = (const ALeft, ARight: TQuantity): boolean;
{$ENDIF}

{$IFDEF INTF_END}
{$UNDEF INTF_END}
end;
{$ENDIF}

{$IFDEF IMPL_QUANTITY}
{$UNDEF IMPL_QUANTITY}

function TQuantity.Value: TVector;
begin
  result := FValue;
end;

function TQuantity.Value(const APrefixes: TPrefixes): TVector;
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

function TQuantity.IsZero: boolean;
begin
  result := FValue.SameValue(NullVector);
end;  

class operator TQuantity.+(const AValue: TQuantity): TQuantity;
begin
  result := AValue;
end;

class operator TQuantity.-(const AValue: TQuantity): TQuantity;
begin
  result := -AValue;
end;

class operator TQuantity.+(const ALeft, ARight: TQuantity): TQuantity;
begin
  result := ALeft + ARight;
end;

class operator TQuantity.-(const ALeft, ARight: TQuantity): TQuantity;
begin
  result := ALeft - ARight;
end;

class operator TQuantity.*(const AValue: double; const ASelf: TQuantity): TQuantity;
begin
  result := AValue * ASelf;
end;

class operator TQuantity.*(const ASelf: TQuantity; const AValue: double): TQuantity;
begin
  result := ASelf * AValue;
end;

class operator TQuantity./(const ASelf: TQuantity; const AValue: double): TQuantity;
begin
  result := ASelf / AValue;
end;

class operator TQuantity.=(const ALeft, ARight: TQuantity): boolean;
begin
  result := ALeft = ARight;
end;
{$ENDIF}

{$IFDEF INTF_UNIT}
{$UNDEF INTF_UNIT}
type
  TUnit = record
{$ENDIF}

{$IFDEF IMPL_UNIT}
{$UNDEF IMPL_UNIT}
{$ENDIF}
