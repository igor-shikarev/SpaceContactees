unit uSolar;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse,
  CastleTransform, CastleScene, CastleViewport;

type
  TSolar = class(TCastleTransform)
  private
    FSolar: TCastleScene;
    FSphere: TCastleSphere;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
	end;

implementation

uses
  CastleColors;

{ TSolar }

constructor TSolar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSolar := TCastleScene.Create(Self);

  FSphere := TCastleSphere.Create(Self);
  FSphere.Color := Fuchsia;

  FSolar.Add(FSphere);
  Self.Add(FSolar);

  FSolar.Translation := Vector3(0, 0, 0);
  FSphere.Translation := Vector3(0, 0, 0);
end;

procedure TSolar.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
begin
  inherited Update(SecondsPassed, RemoveMe);
end;

end.

