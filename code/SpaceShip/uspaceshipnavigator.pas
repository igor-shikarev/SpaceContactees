unit uSpaceShipNavigator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleTransform, CastleScene,
  CastleViewport, CastleCameras, CastleKeysMouse, CastleInputs,
  uSpaceShip;

type

	{ TSpaceShipNavigator }

  TSpaceShipNavigator = class(TCastleMouseLookNavigation)
  private
    FInput_Forward: TInputShortcut;
    FInput_Backward: TInputShortcut;
    FInput_Right: TInputShortcut;
    FInput_Left: TInputShortcut;
    FInput_Up: TInputShortcut;
    FInput_Down: TInputShortcut;
    FInput_SetHorizont: TInputShortcut;
		FSpaceShip: TSpaceShip;
  private
    const
      DefaultRotationHorizontalSpeed = Pi * 45 / 180;
      DefaultRotationVerticalSpeed = Pi * 45 / 180;
      DefaultMoveSpeed = 2;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

    property SpaceShip: TSpaceShip read FSpaceShip write FSpaceShip;
	end;

implementation

uses
  Math;

{ TSpaceShipNavigator }

constructor TSpaceShipNavigator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FInput_Forward := TInputShortcut.Create(Self);
  FInput_Backward := TInputShortcut.Create(Self);
  FInput_Right := TInputShortcut.Create(Self);
  FInput_Left := TInputShortcut.Create(Self);
  FInput_Up := TInputShortcut.Create(Self);
  FInput_Down := TInputShortcut.Create(Self);
  FInput_SetHorizont := TInputShortcut.Create(Self);

  FInput_Forward.Assign(keyW);
  FInput_Backward.Assign(keyS);
  FInput_Right.Assign(keyArrowRight);
  FInput_Left.Assign(keyArrowLeft);
  FInput_Up.Assign(keyArrowUp);
  FInput_Down.Assign(keyArrowDown);
  FInput_SetHorizont.Assign(keyH);
end;

procedure TSpaceShipNavigator.Update(const SecondsPassed: Single;
  var HandleInput: Boolean);
var
  vVect: TVector3;
begin
  inherited Update(SecondsPassed, HandleInput);

  if FInput_Left.IsPressed(Container) then
  begin
    vVect := Vector3(0, 0, 1);
    if Sign(FSpaceShip.Up.Z) < 0 then vVect := Vector3(0, 0, -1);
    FSpaceShip.Direction := RotatePointAroundAxisRad(DefaultRotationHorizontalSpeed * SecondsPassed, FSpaceShip.Direction, vVect);
    FSpaceShip.Up := RotatePointAroundAxisRad(DefaultRotationHorizontalSpeed * SecondsPassed, FSpaceShip.Up, vVect);
	end;

  if FInput_Right.IsPressed(Container) then
  begin
    vVect := Vector3(0, 0, 1);
    if Sign(FSpaceShip.Up.Z) < 0 then vVect := Vector3(0, 0, -1);
    FSpaceShip.Direction := RotatePointAroundAxisRad(-DefaultRotationHorizontalSpeed * SecondsPassed, FSpaceShip.Direction, vVect);
    FSpaceShip.Up := RotatePointAroundAxisRad(-DefaultRotationHorizontalSpeed * SecondsPassed, FSpaceShip.Up, vVect);
	end;

  if FInput_Up.IsPressed(Container) then
  begin
    vVect := TVector3.CrossProduct(FSpaceShip.Direction, FSpaceShip.Up);
    FSpaceShip.Direction := RotatePointAroundAxisRad(-DefaultRotationVerticalSpeed * SecondsPassed, FSpaceShip.Direction, vVect);
    FSpaceShip.Up := RotatePointAroundAxisRad(-DefaultRotationVerticalSpeed * SecondsPassed, FSpaceShip.Up, vVect);
	end;

  if FInput_Down.IsPressed(Container) then
  begin
    vVect := TVector3.CrossProduct(FSpaceShip.Direction, FSpaceShip.Up);
    FSpaceShip.Direction := RotatePointAroundAxisRad(DefaultRotationVerticalSpeed * SecondsPassed, FSpaceShip.Direction, vVect);
    FSpaceShip.Up := RotatePointAroundAxisRad(DefaultRotationVerticalSpeed * SecondsPassed, FSpaceShip.Up, vVect);
	end;

  if FInput_Forward.IsPressed(Container) then
  begin
    vVect := TVector3.Zero;
    vVect := vVect + FSpaceShip.Direction * DefaultMoveSpeed * SecondsPassed;
    FSpaceShip.Move(vVect, False, False);
	end;

  if FInput_Backward.IsPressed(Container) then
  begin
    vVect := TVector3.Zero;
    vVect := vVect - FSpaceShip.Direction * DefaultMoveSpeed * SecondsPassed;
    FSpaceShip.Move(vVect, False, False);
	end;

  if FInput_SetHorizont.IsPressed(Container) then
  begin
    FSpaceShip.Up := Vector3(0, 1, 0);
	end;

end;

end.

