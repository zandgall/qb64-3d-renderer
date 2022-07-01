Type vector
    x As Double
    y As Double
    z As Double
    w As Double
End Type

Dim Shared As Integer Width, Height
Dim Shared As Single triX1, triX2, triX3, triY1, triY2, triY3, tri1OverZ1, tri1OverZ2, tri1OverZ3, triArea, tri1OverArea, triU, triV, triW
Dim Shared vector as vector
Dim Shared projection_data(4) As Double ' Store FOV * 0.5, FOV * Width * 0.5, Zm, and Za
Dim Shared As Single lslope, rslope, lx, rx, sy, ey, dudx, dvdx, dwdx, dudy, dvdy, dwdy, pixel_index, depth
Dim Shared pixel_x As Integer
Dim Shared pixel_y As Integer
Dim Shared current_frame As Integer
current_frame = 1

Width = 800
Height = 800

Dim Shared pixel_depth(Width * Height) As Double
Dim Shared pixel_time(Width * Height) As Double

display = _NewImage(Width, Height, 32)
Screen display

Call setProjection(90, 10000, 0.01)

positionx = 0
positiony = 0
positionz = -200

rotationx = 0.0
rotationy = 0.0

Do
    Cls
    If _KeyDown(19200) Then
        rotationy = rotationy - 0.03
    End If
    If _KeyDown(19712) Then
        rotationy = rotationy + 0.03
    End If
    If _KeyDown(18432) Then
        rotationx = rotationx + 0.03
    End If
    If _KeyDown(20480) Then
        rotationx = rotationx - 0.03
    End If

    Call cube(positionx, positiony, positionz, 50, rotationx, rotationy)

    current_frame = current_frame + 1
    _Display
Loop

Color _RGB(255, 255, 255)
End

Sub cube (positionx As Single, positiony As Single, positionz As Single, size As Single, rotationx As Single, rotationy As Single)
    Dim As vector lft, rft, lbt, rbt, lfb, rfb, lbb, rbb

    setVector -size, size, -size, 0
    rotateVector rotationx, 0, rotationy
    lft.x = vector.x + positionx: lft.y = vector.y + positiony: lft.z = vector.z + positionz

    setVector size, size, -size, 0
    rotateVector rotationx, 0, rotationy
    rft.x = vector.x + positionx: rft.y = vector.y + positiony: rft.z = vector.z + positionz

    setVector -size, size, size, 0
    rotateVector rotationx, 0, rotationy
    lbt.x = vector.x + positionx: lbt.y = vector.y + positiony: lbt.z = vector.z + positionz

    setVector size, size, size, 0
    rotateVector rotationx, 0, rotationy
    rbt.x = vector.x + positionx: rbt.y = vector.y + positiony: rbt.z = vector.z + positionz

    setVector -size, -size, -size, 0
    rotateVector rotationx, 0, rotationy
    lfb.x = vector.x + positionx: lfb.y = vector.y + positiony: lfb.z = vector.z + positionz

    setVector size, -size, -size, 0
    rotateVector rotationx, 0, rotationy
    rfb.x = vector.x + positionx: rfb.y = vector.y + positiony: rfb.z = vector.z + positionz

    setVector -size, -size, size, 0
    rotateVector rotationx, 0, rotationy
    lbb.x = vector.x + positionx: lbb.y = vector.y + positiony: lbb.z = vector.z + positionz

    setVector size, -size, size, 0
    rotateVector rotationx, 0, rotationy
    rbb.x = vector.x + positionx: rbb.y = vector.y + positiony: rbb.z = vector.z + positionz

    tri_vector lft, rft, rfb
    tri_vector rfb, lfb, lft

    tri_vector rbt, lbt, lbb
    tri_vector lbb, rbb, rbt

    tri_vector lbt, lft, lfb
    tri_vector lfb, lbb, lbt

    tri_vector rft, rbt, rbb
    tri_vector rbb, rfb, rft

    tri_vector rft, lbt, rbt
    tri_vector lbt, rft, lft

    tri_vector lfb, rbb, lbb
    tri_vector rbb, lfb, rfb

End Sub

' Returns the max between two numbers
Function max (ma_x, ma_y)
    max = ma_x
    If ma_y > ma_x Then max = ma_y
End Function

' Returns the min between two numbers
Function min (mi_x, mi_y)
    min = mi_x
    If mi_y < mi_x Then min = mi_y
End Function

Sub setVector (x As Double, y As Double, z As Double, w As Double)
    vector.x = x
    vector.y = y
    vector.z = z
    vector.w = w
End Sub

Sub setProjection (FOVAngle As Double, Far As Double, Near As Double)
    projection_data(0) = 0.5 / Tan(FOVAngle * 3.14159267 / 360.0)
    projection_data(1) = Width * projection_data(0)
    projection_data(2) = -Far / (Far - Near)
    projection_data(3) = -(Far * Near) / (Far - Near)
End Sub

Sub projectVector ' Leaves z as 1/z
    vector.z = 1 / (vector.z * projection_data(2) + vector.w * projection_data(3))
    vector.x = vector.x * vector.z * projection_data(1) + vector.w * 0.5 * Width
    vector.y = vector.y * vector.z * projection_data(1) + vector.w * 0.5 * Height
End Sub

Sub rotateVector (rx As Double, ry As Double, rz As Double)
    Dim nvector As vector
    nvector.x = (vector.x * Cos(rz) * Cos(ry)) + (vector.y * Sin(rz) * Cos(ry)) - vector.z * Sin(ry)
    nvector.y = (vector.x * (Cos(rz) * Sin(ry) * Sin(rx) - Sin(rz) * Cos(rx))) + (vector.y * (Sin(rz) * Sin(ry) * Sin(rx) + Cos(rz) * Cos(rx))) + (vector.z * Cos(ry) * Sin(rx))
    nvector.z = (vector.x * (Cos(rz) * Sin(ry) * Cos(rx) + Sin(rz) * Sin(rx))) + (vector.y * (Sin(rz) * Sin(ry) * Cos(rx) - Cos(rz) * Sin(rx))) + (vector.z * Cos(ry) * Cos(rx))
    nvector.w = 1
    vector.x = nvector.x: vector.y = nvector.y: vector.z = nvector.z
End Sub

Sub tri_flat_term (x1, y1, x2, x3, term)
    lx = x2 ' Set left x that will be walking line  (x2,term)-(x1,y1)
    rx = x3 ' Set right x that will be walking line (x3,term)-(x1,y1)
    If (x2 > x3) Then rx = x2: lx = x3
    lslope = (lx - x1) / (term - y1) ' Negative left inverse slope depending on if y will be increasing or decreasing
    rslope = (rx - x1) / (term - y1) ' Negative right inverse slope for same reason

    ' Set starting y and ending y
    sy = term
    ey = y1
    If (y1 < term) Then sy = y1: ey = term: lx = x1: rx = x1

    ' Set uvw
    triU = ((triX3 - lx) * (triY2 - sy) - (triY3 - sy) * (triX2 - lx)) * 0.5 * tri1OverArea
    triV = ((triX1 - lx) * (triY3 - sy) - (triY1 - sy) * (triX3 - lx)) * 0.5 * tri1OverArea
    triW = ((triX2 - lx) * (triY1 - sy) - (triY2 - sy) * (triX1 - lx)) * 0.5 * tri1OverArea
    ' Set duvw/dx
    dudx = (triY3 - triY2) * 0.5 * tri1OverArea
    dvdx = (triY1 - triY3) * 0.5 * tri1OverArea
    dwdx = (triY2 - triY1) * 0.5 * tri1OverArea
    ' Set duvw/dy
    dudy = (triX2 - triX3) * 0.5 * tri1OverArea
    dvdy = (triX3 - triX1) * 0.5 * tri1OverArea
    dwdy = (triX1 - triX2) * 0.5 * tri1OverArea
    For pixel_y = sy To ey - 1
        uvwxinctimes = 0
        If pixel_y < 0 Or pixel_y >= Height Then GoTo tri_flat_term_yskip
        For pixel_x = lx To rx
            triU = triU + dudx
            triV = triV + dvdx
            triW = triW + dwdx

            If pixel_x < 0 Or pixel_x >= Width Then GoTo tri_flat_term_xskip

            pixel_index = pixel_y * Width + pixel_x
            depth = 1 / (triU * tri1OverZ1 + triV * tri1OverZ2 + triW * tri1OverZ3)
            If pixel_depth(pixel_index) > depth Or pixel_time(pixel_index) < current_frame Then
                pixel_depth(pixel_index) = depth
                pixel_time(pixel_index) = current_frame
                Color _RGB(triU * 256, triV * 256, triW * 256)
                PSet (pixel_x, pixel_y)
            End If
            tri_flat_term_xskip:
        Next

        tri_flat_term_yskip:
        triU = triU + dudy + lslope * dudx - (CInt(rx) - CInt(lx) + 1) * dudx
        triV = triV + dvdy + lslope * dvdx - (CInt(rx) - CInt(lx) + 1) * dvdx
        triW = triW + dwdy + lslope * dwdx - (CInt(rx) - CInt(lx) + 1) * dwdx

        lx = lx + lslope
        rx = rx + rslope
    Next
End Sub

Sub tri (x1, y1, z1, x2, y2, z2, x3, y3, z3)
    'Project all 3 vertices to pixel-coordinates
    setVector x1, y1, z1, 1
    projectVector
    triX1 = vector.x: triY1 = vector.y: tri1OverZ1 = vector.z
    setVector x2, y2, z2, 1
    projectVector
    triX2 = vector.x: triY2 = vector.y: tri1OverZ2 = vector.z
    setVector x3, y3, z3, 1
    projectVector
    triX3 = vector.x: triY3 = vector.y: tri1OverZ3 = vector.z

    ' Compute extra triangle data
    triArea = ((triX3 - triX1) * (triY2 - triY1) - (triY3 - triY1) * (triX2 - triX1)) * 0.5
    tri1OverArea = 1 / triArea

    ' if any of the y coordinates are the same, draw a flat terminating triangle instead of splitting it
    If triY1 = triY2 Then tri_flat_term triX3, triY3, triX1, triX2, triY1: GoTo tri_skip
    If triY2 = triY3 Then tri_flat_term triX1, triY1, triX2, triX3, triY2: GoTo tri_skip
    If triY3 = triY1 Then tri_flat_term triX2, triY2, triX3, triX1, triY3: GoTo tri_skip
    Dim m
    ' Check which vertice is in the middle of the triangle in terms of y
    If (triY1 < triY2 And triY1 > triY3) Or (triY1 > triY2 And triY1 < triY3) Then
        m = triX2 + (triY1 - triY2) * (triX3 - triX2) / (triY3 - triY2)
        tri_flat_term triX3, triY3, triX1, m, triY1: tri_flat_term triX2, triY2, triX1, m, triY1: GoTo tri_skip
    ElseIf (triY2 < triY1 And triY2 > triY3) Or (triY2 > triY1 And triY2 < triY3) Then
        m = triX1 + (triY2 - triY1) * (triX3 - triX1) / (triY3 - triY1)
        tri_flat_term triX3, triY3, triX2, m, triY2: tri_flat_term triX1, triY1, triX2, m, triY2: GoTo tri_skip
    Else
        m = triX1 + (triY3 - triY1) * (triX2 - triX1) / (triY2 - triY1)
        tri_flat_term triX2, triY2, triX3, m, triY3: tri_flat_term triX1, triY1, triX3, m, triY3
    End If

    tri_skip:
End Sub

Sub tri_vector (a As vector, b As vector, c As vector)
    tri a.x, a.y, a.z, b.x, b.y, b.z, c.x, c.y, c.z
End Sub
