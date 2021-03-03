unit PyLernKartenUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GHashSet;

type

hashlli=class
     public
     class function hash(a:longint; b:SizeUInt):SizeUInt;
end;

TIntSet = specialize THashSet<longint, hashlli>;

implementation

class function hashlli.hash(a:longint; b:SizeUInt):SizeUInt;
begin
  hash:= ((a+1) mod b);
end;


end.

