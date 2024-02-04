{$IFDEF INTF_QUANTITY}
{$UNDEF INTF_QUANTITY}
type
  TQuantity = record
  private
    FValue: TVector;
  public       
    function Norm: double;
    function Norm(const APrefixes: TPrefixes): double;
  //function Value(Index: longint): double;
  //function Value(Index: longint; const APrefixes: TPrefixes): double;
    function ToString: string;
    function ToString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
  //function ToString(const ATolerance: TQuantity; APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
    function ToVerboseString: string;
    function ToVerboseString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
  //function ToVerboseString(const ATolerance: TQuantity; APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
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
function TQuantity.Norm: double;
begin
  result := FValue.Norm;
end;

function TQuantity.Norm(const APrefixes: TPrefixes): double;
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
      result := FValue.Norm * IntPower(10, Exponent)
    else  
      result := FValue.Norm;
  end else   
    if PrefixCount = 0 then 
      result := FValue.Norm
    else  
      raise Exception.Create('Wrong number of prefixes.');   
end;

function TQuantity.ToString: string;
begin
  result := FloatToStr(FValue.Norm) + ' ' + GetSymbol(CSYMBOL, CPREFIXES);
end;

function TQuantity.ToVerboseString: string;
var
  Return: double;
begin
  Return := FValue.Norm;
  if (Return < -1) or (Return > 1) then
    result := FloatToStr(Return) + ' ' + GetName(CPLURALNAME, CPREFIXES)
  else
    result := FloatToStr(Return) + ' ' + GetName(CSINGULARNAME, CPREFIXES);
end;

function TQuantity.ToString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var
  Return: double;
begin
  Return := Norm(APrefixes);
  if Length(APrefixes) = 0 then
    result := FloatToStrF(Return, ffGeneral, APrecision, ADigits) + ' ' + GetSymbol(CSYMBOL, CPREFIXES)
  else
    result := FloatToStrF(Return, ffGeneral, APrecision, ADigits) + ' ' + GetSymbol(CSYMBOL, APrefixes);
end;

function TQuantity.ToVerboseString(APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var
  Return: double;
begin
  Return := Norm(APrefixes);
  if (Return < -1) or (Return > 1) then
  begin
    if Length(APrefixes) = 0 then
      result := FloatToStrF(Return, ffGeneral, APrecision, ADigits) + ' ' + GetName(CPLURALNAME, CPREFIXES)
    else
      result := FloatToStrF(Return, ffGeneral, APrecision, ADigits) + ' ' + GetName(CPLURALNAME, APrefixes);
  end else
  begin
    if Length(APrefixes) = 0 then
      result := FloatToStrF(Return, ffGeneral, APrecision, ADigits) + ' ' + GetName(CSINGULARNAME, CPREFIXES)
    else
      result := FloatToStrF(Return, ffGeneral, APrecision, ADigits) + ' ' + GetName(CSINGULARNAME, APrefixes)
  end; 
end;      
(*
function TQuantity.ToString(const ATolerance: TQuantity; APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var  
  ReturnTol: double;
  ReturnValue: double;
begin
  ReturnValue := Norm(APrefixes);
  ReturnTol   := ATolerance.Value(APrefixes);

  if Length(APrefixes) = 0 then
  begin
    result := FloatToStrF(ReturnValue, ffGeneral, APrecision, ADigits) + ' ± ' +
              FloatToStrF(ReturnTol,   ffGeneral, APrecision, ADigits) + ' '   + GetSymbol(CSYMBOL, CPREFIXES)
  end else
  begin
    result := FloatToStrF(ReturnValue, ffGeneral, APrecision, ADigits) + ' ± ' +
              FloatToStrF(ReturnTol,   ffGeneral, APrecision, ADigits) + ' '   + GetSymbol(CSYMBOL, APrefixes);
  end;                                   
end;

function TQuantity.ToVerboseString(const ATolerance: TQuantity; APrecision, ADigits: longint; const APrefixes: TPrefixes): string;
var  
  ReturnTol: double;
  ReturnValue: double;
begin
  ReturnValue := Norm(APrefixes);
  ReturnTol   := ATolerance.Value(APrefixes);

  if Length(APrefixes) = 0 then
    result := FloatToStrF(ReturnValue, ffGeneral, APrecision, ADigits) + ' ± ' +
              FloatToStrF(ReturnTol,   ffGeneral, APrecision, ADigits) + ' '   + GetName(CPLURALNAME, CPREFIXES)
  else
    result := FloatToStrF(ReturnValue, ffGeneral, APrecision, ADigits) + ' ± ' +
              FloatToStrF(ReturnTol,   ffGeneral, APrecision, ADigits) + ' '   + GetName(CPLURALNAME, APrefixes);
end;
*)
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

