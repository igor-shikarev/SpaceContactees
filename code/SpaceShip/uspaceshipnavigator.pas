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
  CastleQuaternions;

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

  FInput_Forward.Assign(keyW);
  FInput_Backward.Assign(keyS);
  FInput_Right.Assign(keyArrowRight);
  FInput_Left.Assign(keyArrowLeft);
  FInput_Up.Assign(keyArrowUp);
  FInput_Down.Assign(keyArrowDown);

  //Gravity := False;
  //MoveSpeed := 10;
end;

procedure TSpaceShipNavigator.Update(const SecondsPassed: Single;
  var HandleInput: Boolean);
var
  vRes: TVector3;
begin
  inherited Update(SecondsPassed, HandleInput);

  vRes := TVector3.Zero;

  if FInput_Left.IsPressed(Container) then
  begin
    FSpaceShip.Direction := RotatePointAroundAxisRad(DefaultRotationHorizontalSpeed * SecondsPassed, FSpaceShip.Direction, FSpaceShip.Up.One[1]);
	end;

  if FInput_Right.IsPressed(Container) then
  begin
    FSpaceShip.Direction := RotatePointAroundAxisRad(-DefaultRotationHorizontalSpeed * SecondsPassed, FSpaceShip.Direction, FSpaceShip.Up.One[1]);
	end;

  if FInput_Up.IsPressed(Container) then
  begin
    FSpaceShip.Direction := RotatePointAroundAxisRad(DefaultRotationVerticalSpeed * SecondsPassed, FSpaceShip.Direction, FSpaceShip.Up.One[0]);
	end;

  if FInput_Down.IsPressed(Container) then
  begin
    FSpaceShip.Direction := RotatePointAroundAxisRad(-DefaultRotationVerticalSpeed * SecondsPassed, FSpaceShip.Direction, FSpaceShip.Up.One[0]);
	end;

  if FInput_Forward.IsPressed(Container) then
  begin
    vRes := vRes - FSpaceShip.Direction * DefaultMoveSpeed * SecondsPassed;
    FSpaceShip.Move(vRes, False, False);
	end;

  if FInput_Backward.IsPressed(Container) then
  begin
    vRes := vRes + FSpaceShip.Direction * DefaultMoveSpeed * SecondsPassed;
    FSpaceShip.Move(vRes, False, False);
	end;

end;

end.

