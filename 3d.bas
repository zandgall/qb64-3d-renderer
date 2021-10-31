Dim Shared SIZE As Integer
SIZE = 800 ' Set to 320 if you want to use Screen 13

bscreen = _NewImage(SIZE, SIZE, 32)
Screen bscreen

' FOV is a calculated value that gets multiplied with X and Y to deliver a field of view
' ZMul and ZAdd both transform the Z axis based on a Near and Far value
Dim Shared FOV As Double, ZMul As Double, ZAdd As Double
' x y and z are used in this program to simulate the camera moving
Dim Shared x As Double, y As Double, z As Double
' useXY and Z are static variables used by "applyRotation" to save writing three rotation functions
' Write your Call applyRotation(x, y, z)
Dim Shared useX As Double, useY As Double, useZ As Double

' Shared triangle variables. Used to store variables across functions and subs]
Dim Shared tri_u As Double, tri_v As Double, tri_w As Double
Dim Shared tri_x1 As Double, tri_x2 As Double, tri_x3 As Double
Dim Shared tri_y1 As Double, tri_y2 As Double, tri_y3 As Double
Dim Shared tri_d1 As Double, tri_d2 As Double, tri_d3 As Double
Dim Shared tri_area As Double, tri_one_over_area As Double

' time is incremented by 0.1 every frame
' Resolution is the step of the triangle; ex a resolution of 4 will draw pixels 4x4 wide
Dim Shared time As Double, resolution As Double, one_over_resolution As Double
resolution = 1
one_over_resolution = 1 / resolution

' The 20.00 in this statement is the FOV Angle
FOV = 1.0 / Tan(20.0 * 0.5 * 3.14159265359 / 180.0)
Far = 100
Near = 0.1
ZMul = -Far / (Far - Near)
ZAdd = -Far * Near / (Far - Near)


time = 0

x = 0
y = 0
z = 0
RX = 0
RY = 0
RZ = 0

' Two depth related arrays, enveloping the whole (square) screen.
' Depth stores distance from the camera
' DepthTime stores the "time" that a pixel was last updated. So that I can continue using Depth without having to reset it every time
Dim Shared Depth((SIZE / resolution) ^ 2) As Double
Dim Shared DepthTime((SIZE / resolution) ^ 2) As Double

' Rotation in X, Y, and Z
Dim RX As Double, RY As Double, RZ As Double

Do
    Cls
    ' Avoided using DepthTime
    'For i = 0 To (SIZE/resolution) ^ 2
    'Depth(i) = 0
    'Next
    Print time ' Optional, was used to show how fast each frame was happening

    ' Color _RGB(255, 120, 0) 'Comment out colors in triangles in order to have single solid-color cubes
    Call cube(-10 + x, -10 + y, -50 + z, 20, 20, 20, RX, RY, RZ)
    Call cube(-10, -10, -50, 20, 20, 20, 0, 0, 0)
    'Call fast_tri_2d(0, 0, 100, 0, 0, 100)

    ' Controls
    If _KeyDown(119) Then z = z + 1
    If _KeyDown(115) Then z = z - 1
    If _KeyDown(100) Then x = x - 1
    If _KeyDown(97) Then x = x + 1
    If _KeyDown(19200) Then RY = RY - 0.03
    If _KeyDown(19712) Then RY = RY + 0.03
    If _KeyDown(18432) Then RX = RX - 0.03
    If _KeyDown(20480) Then RX = RX + 0.03
    If _KeyDown(101) Then y = y - 1
    If _KeyDown(113) Then y = y + 1

    ' time increment, display, and frame limit. Limit not necessary with low resolution (resolution VARIABLE, not screen resolution) as it's slow enough
    time = time + 1
    _Display
    _Limit 60
    ' Sleep 10

Loop

' A function that snaps a number to the resolution grid
Function snap (snapnum)
    snap = Fix(snapnum * one_over_resolution) * resolution
End Function

' Can be used to draw lines between 2 3d points
Sub Line3D (x1, y1, z1, x2, y2, z2)
    Line (proj(x1, z1), proj(y1, z1))-(proj(x2, z2), proj(y2, z2))
End Sub

' A clunky cube sub. Takes in position, size, and rotation.
Sub cube (x, y, z, w, h, d, rx, ry, rz)

    ' Individual number calculation that makes me miss OOP vector x matrix math

    ' Calculate all points with rotation
    '' Left front top
    centerX = x + w / 2
    centerY = y + h / 2
    centerZ = z + d / 2
    'Print centerX, centerY, centerZ
    useX = x - centerX
    useZ = z - centerZ
    useY = y - centerY
    applyRotation rx, ry, rz
    xlft = useX + centerX
    zlft = useZ + centerZ
    ylft = useY + centerY

    '' Right front top
    useX = x + w - centerX
    useZ = z - centerZ
    useY = y - centerY
    applyRotation rx, ry, rz
    xrft = useX + centerX
    zrft = useZ + centerZ
    yrft = useY + centerY

    '' Left back top
    useX = x - centerX
    useZ = z + d - centerZ
    useY = y - centerY
    applyRotation rx, ry, rz
    xlbt = useX + centerX
    zlbt = useZ + centerZ
    ylbt = useY + centerY

    '' Right back top
    useX = x + w - centerX
    useZ = z + d - centerZ
    useY = y - centerY
    applyRotation rx, ry, rz
    xrbt = useX + centerX
    zrbt = useZ + centerZ
    yrbt = useY + centerY

    '' Left front bottom
    useX = x - centerX
    useZ = z - centerZ
    useY = y + h - centerY
    applyRotation rx, ry, rz
    xlfb = useX + centerX
    zlfb = useZ + centerZ
    ylfb = useY + centerY

    '' Right front bottom
    useX = x + w - centerX
    useZ = z - centerZ
    useY = y + h - centerY
    applyRotation rx, ry, rz
    xrfb = useX + centerX
    zrfb = useZ + centerZ
    yrfb = useY + centerY

    '' Left back bottom
    useX = x - centerX
    useZ = z + d - centerZ
    useY = y + h - centerY
    applyRotation rx, ry, rz
    xlbb = useX + centerX
    zlbb = useZ + centerZ
    ylbb = useY + centerY

    '' Right back bottom
    useX = x + w - centerX
    useZ = z + d - centerZ
    useY = y + h - centerY
    applyRotation rx, ry, rz
    xrbb = useX + centerX
    zrbb = useZ + centerZ
    yrbb = useY + centerY

    '' Line functions that were used to draw an outline
    ' Front face
    'Color _RGB(255, 255, 255)
    'Line3D xlft, ylft, zlft, xrft, yrft, zrft
    'Line3D xrft, yrft, zrft, xrfb, yrfb, zrfb
    'Line3D xrfb, yrfb, zrfb, xlfb, ylfb, zlfb
    'Line3D xlfb, ylfb, zlfb, xlft, ylft, zlft
    ' Back face
    'Line3D xlbt, ylbt, zlbt, xrbt, yrbt, zrbt
    'Line3D xrbt, yrbt, zrbt, xrbb, yrbb, zrbb
    'Line3D xrbb, yrbb, zrbb, xlbb, ylbb, zlbb
    'Line3D xlbb, ylbb, zlbb, xlbt, ylbt, zlbt
    ' Connecting front and back
    'Line3D xlft, ylft, zlft, xlbt, ylbt, zlbt
    'Line3D xrft, yrft, zrft, xrbt, yrbt, zrbt
    'Line3D xlfb, ylfb, zlfb, xlbb, ylbb, zlbb
    'Line3D xrfb, yrfb, zrfb, xrbb, yrbb, zrbb
    'Color _RGB(172, 78, 127)

    'Color 1 ' If you wanna do per-face coloring instead of per pixel or per cube, uncomment these colors and comment out the colors in Triangle function
    'Print xlft, ylft, zlft
    'Print xrft, yrft, zrft
    'Print xrfb, yrfb, zrfb

    Call fast_tri(xlft, ylft, zlft, xrft, yrft, zrft, xrfb, yrfb, zrfb)
    'Color 2
    Call fast_tri(xlfb, ylfb, zlfb, xrfb, yrfb, zrfb, xlft, ylft, zlft)
    'Color 3
    Call fast_tri(xrbt, yrbt, zrbt, xlbt, ylbt, zlbt, xlbb, ylbb, zlbb)
    'Color 4
    Call fast_tri(xrbb, yrbb, zrbb, xlbb, ylbb, zlbb, xrbt, yrbt, zrbt)
    'Color 5
    Call fast_tri(xlbt, ylbt, zlbt, xlft, ylft, zlft, xlfb, ylfb, zlfb)
    'Color 6
    Call fast_tri(xlbb, ylbb, zlbb, xlfb, ylfb, zlfb, xlbt, ylbt, zlbt)
    'Color 7
    Call fast_tri(xrft, yrft, zrft, xrbt, yrbt, zrbt, xrbb, yrbb, zrbb)
    'Color 8
    Call fast_tri(xrfb, yrfb, zrfb, xrbb, yrbb, zrbb, xrft, yrft, zrft)
    'Color 9
    Call fast_tri(xrft, yrft, zrft, xlbt, ylbt, zlbt, xrbt, yrbt, zrbt)
    'Color 10
    Call fast_tri(xrft, yrft, zrft, xlbt, ylbt, zlbt, xlft, ylft, zlft)
    'Color 11
    Call fast_tri(xlfb, ylfb, zlfb, xrbb, yrbb, zrbb, xlbb, ylbb, zlbb)
    'Color 12
    Call fast_tri(xlfb, ylfb, zlfb, xrbb, yrbb, zrbb, xrfb, yrfb, zrfb)

End Sub

' Returns a screenspace position of a physical position
Function proj (pr_x, pr_z)
    za = pr_z * ZMul + ZAdd
    za = max(za, 0.0001)
    xa = (pr_x * FOV) / za
    proj = xa * SIZE / 10.0 + SIZE / 2.0 ' Translates output to center of the screen, and scales it a bit
End Function

' Applies rotation to useX, Y, and Z
Sub applyRotation (rx, ry, rz)
    ' Rotation
    outX = (useX * Cos(rz) * Cos(ry)) + (useY * Sin(rz) * Cos(ry)) - (useZ * Sin(ry))
    outY = (useX * (Cos(rz) * Sin(ry) * Sin(rx) - Sin(rz) * Cos(rx))) + (useY * (Sin(rz) * Sin(ry) * Sin(rx) + Cos(rz) * Cos(rx))) + (useZ * Cos(ry) * Sin(rx))
    outZ = (useX * (Cos(rz) * Sin(ry) * Cos(rx) + Sin(rz) * Sin(rx))) + (useY * (Sin(rz) * Sin(ry) * Cos(rx) - Cos(rz) * Sin(rx))) + (useZ * Cos(ry) * Cos(rx))

    ' Output
    useX = outX
    useY = outY
    useZ = outZ
End Sub

' Returns the max between two numbers
Function max (ma_x, ma_y)
    If ma_y > ma_x Then
        max = ma_y
    Else max = ma_x
    End If
End Function

' Returns the min between two numbers
Function min (mi_x, mi_y)
    If mi_y < mi_x Then
        min = mi_y
    Else min = mi_x
    End If
End Function

' Was used in setUVW to calculate areas of sub-triangles.
Function edge (e_x1, e_y1, e_x2, e_y2, e_x3, e_y3)
    edge = ((e_x3 - e_x1) * (e_y2 - e_y1)) - ((e_y3 - e_y1) * (e_x2 - e_x1))
End Function

' Sets the barycentric coords of a point
Sub setUVW (uvwx, uvwy)
    'tri_u = edge(x, y, tri_x2, tri_y2, tri_x3, tri_y3) * tri_one_over_area
    tri_u = (((tri_x3 - uvwx) * (tri_y2 - uvwy)) - ((tri_y3 - uvwy) * (tri_x2 - uvwx))) * tri_one_over_area
    'tri_v = edge(x, y, tri_x3, tri_y3, tri_x1, tri_y1) * tri_one_over_area
    tri_v = (((tri_x1 - uvwx) * (tri_y3 - uvwy)) - ((tri_y1 - uvwy) * (tri_x3 - uvwx))) * tri_one_over_area
    'tri_w = edge(x, y, tri_x1, tri_y1, tri_x2, tri_y2) * tri_one_over_area
    tri_w = (((tri_x2 - uvwx) * (tri_y1 - uvwy)) - ((tri_y2 - uvwy) * (tri_x1 - uvwx))) * tri_one_over_area
End Sub

' Draws a triangle with a flat top, from (x1, y1) to (x2, bottom) to (x3, bottom) ; yes bottom is actually the "top" but it's named bottom okay
Sub ft_tri (x1, y1, x2, x3, bottom)
    ' Calculating slopses of lines from (x2, bottom) to (x1, y1) (line a) and from (x3, bottom) to (x1, y1) (line b)
    a_m = (x2 - x1) / (bottom - y1)
    b_m = (x3 - x1) / (bottom - y1)

    ' fty is the current y position of the rasterer. It's incremented every scanline by resolution
    fty = bottom
    ' lx is thought of as the left-most x position in a scanline, but really it could be the left. and rx is the length of any given scanline, (can be negative)
    ' Whether lx is the left or the right, the x will always start at lx and progress to lx+rx
    lx = x2
    rx = (x3 - x2)
    ' Sign dictates whether rx is in the positive-x or negative-x direction from lx
    sign = Sgn(rx)

    Do
        ' Increment y by resolution
        fty = fty + resolution
        ' Travel lx down line a, (x2, bottom)->(x1, y1)
        lx = lx + a_m * resolution
        ' Increment rx by the change in the distance between line a and line b. Resulting in the next scanline width
        rx = rx + (b_m - a_m) * resolution
        ' Start ftx (current x position) at lx, offset by sign to reduce holes in triangles
        ftx = Fix(lx / resolution - sign) * resolution
        Do
            ' Calculate the depth index of the pixel at (ftx, fty), everything is divided by resolution as that
            index = Fix(fty * one_over_resolution) * (SIZE * one_over_resolution) + Fix(ftx * one_over_resolution)
            ' If index of given pixel is outside of draw area, then skip this pixel
            If index > (SIZE * one_over_resolution) ^ 2 Or index < 0 Then GoTo ftendloop

            ' Set the barycentric coordinates of the current (ftx, fty)
            Call setUVW(ftx, fty)
            ' Calculate the distance to the camera by blending the barycentric coordinates and their matching vertice's distance
            current_distance = tri_u * tri_d1 + tri_v * tri_d2 + tri_w * tri_d3
            ' Set the current color to the barycentric coords to show how they change (and to look cool honestly)
            Color _RGB(255 * tri_u, 255 * tri_v, 255 * tri_w)

            ' If the depth of any previous pixel at this spot is further from the camera, or is from an older frame, then draw the pixel
            If Depth(index) < current_distance Or DepthTime(index) < time Then
                ' Update depth and frame-number of current pixel
                Depth(index) = current_distance
                DepthTime(index) = time
                ' Fill in rectangle from (ftx, fty) to (ftx+resolution, fty+resolution)
                For xoff = 0 To resolution
                    For yoff = 0 To resolution
                        PSet (ftx + xoff, fty + yoff) ' Set the pixel!
                    Next
                Next
            End If

            ftendloop:
            ' Increment ftx by resolution, in the direction of rx
            ftx = ftx + sign * resolution
            ' Check that ftx hasn't reached lx + rx
        Loop While ((sign = 1 And ftx < snap(lx + rx)) Or (sign = -1 And ftx > snap(lx + rx)))
    Loop While (fty < y1)
End Sub

' Check notes for ft_tri, same code, with some switched signs
Sub fb_tri (x1, y1, x2, x3, bottom)
    ' Adding resolution in a_m, b_m, and fby to prevent a gap between a flat-top triangle and a flat-bottom triangle
    a_m = (x2 - x1) / (bottom + resolution - y1)
    b_m = (x3 - x1) / (bottom + resolution - y1)

    fby = bottom + resolution
    lx = x2
    rx = (x3 - x2)
    sign = Sgn(rx)
    Do
        fby = fby - resolution
        lx = lx - a_m * resolution
        rx = rx - (b_m - a_m) * resolution
        fbx = Fix(lx / resolution - sign) * resolution
        Do
            index = Fix(fby * one_over_resolution) * (SIZE * one_over_resolution) + Fix(fbx * one_over_resolution)
            If index > (SIZE * one_over_resolution) ^ 2 Or index < 0 Then GoTo fbendloop

            Call setUVW(fbx, fby)
            current_distance = tri_u * tri_d1 + tri_v * tri_d2 + tri_w * tri_d3
            Color _RGB(255 * tri_u, 255 * tri_v, 255 * tri_w)
            If Depth(index) < current_distance Or DepthTime(index) < time Then
                Depth(index) = current_distance
                DepthTime(index) = time
                For xoff = 0 To resolution
                    For yoff = 0 To resolution
                        PSet (fbx + xoff, fby + yoff) ' Set the pixel!
                    Next
                Next
            End If
            fbendloop:
            fbx = fbx + sign * resolution
        Loop While ((sign = 1 And fbx < snap(lx + rx)) Or (sign = -1 And fbx > snap(lx + rx)))
    Loop While (fby > y1)
End Sub

Sub fast_tri_2d (x1, y1, x2, y2, x3, y3)
    ' Set tri_xs and tri_ys to be used in UVW calculation
    tri_x1 = x1
    tri_y1 = y1
    tri_x2 = x2
    tri_y2 = y2
    tri_x3 = x3
    tri_y3 = y3

    ' Pre-calculate the area of the triangle, and 1 / area
    tri_area = edge(x1, y1, x2, y2, x3, y3)
    tri_one_over_area = 1 / tri_area

    ' If any two y coordinates are equal, then draw a flat-top or flat-bottom triangle respectively
    If y1 = y2 Then
        If y3 > y1 Then Call ft_tri(x3, y3, x1, x2, y1): GoTo ft2dskip Else Call fb_tri(x3, y3, x1, x2, y1): GoTo ft2dskip
    ElseIf y2 = y3 Then
        If y1 > y2 Then Call ft_tri(x1, y1, x2, x3, y2): GoTo ft2dskip Else Call fb_tri(x1, y1, x2, x3, y2): GoTo ft2dskip
    ElseIf y3 = y1 Then
        If y2 > y1 Then Call ft_tri(x2, y2, x1, x3, y1): GoTo ft2dskip Else Call fb_tri(x2, y2, x1, x3, y1): GoTo ft2dskip
    End If

    ' Find the middle y-coordinate and use it to split the triangle into a flat-top and flat-bottom triangle
    ' As one point will be in the middle, we know what one of the x values will be, but we need to calculate the x-value at the middle-y on the line opposite of our middle point
    ' m is our second x position
    If y1 < y2 And y1 > y3 Then
        m = x2 + (y1 - y2) * (x3 - x2) / (y3 - y2)
        Call ft_tri(x2, y2, x1, m, y1)
        Call fb_tri(x3, y3, x1, m, y1)
    ElseIf y1 > y2 And y1 < y3 Then
        m = x2 + (y1 - y2) * (x3 - x2) / (y3 - y2)
        Call ft_tri(x3, y3, x1, m, y1)
        Call fb_tri(x2, y2, x1, m, y1)
    ElseIf y2 < y1 And y2 > y3 Then
        m = x1 + (y2 - y1) * (x3 - x1) / (y3 - y1)
        Call ft_tri(x1, y1, x2, m, y2)
        Call fb_tri(x3, y3, x2, m, y2)
    ElseIf y2 > y1 And y2 < y3 Then
        m = x1 + (y2 - y1) * (x3 - x1) / (y3 - y1)
        Call ft_tri(x3, y3, x2, m, y2)
        Call fb_tri(x1, y1, x2, m, y2)
    ElseIf y3 < y1 And y3 > y2 Then
        m = x1 + (y3 - y1) * (x2 - x1) / (y2 - y1)
        Call ft_tri(x1, y1, x3, m, y3)
        Call fb_tri(x2, y2, x3, m, y3)
    ElseIf y3 > y1 And y3 < y2 Then
        m = x1 + (y3 - y1) * (x2 - x1) / (y2 - y1)
        Call ft_tri(x2, y2, x3, m, y3)
        Call fb_tri(x1, y1, x3, m, y3)
    End If
    ft2dskip:
End Sub

Sub fast_tri (x1, y1, z1, x2, y2, z2, x3, y3, z3)
    ' Set the distances for points 1, 2, and 3  as their z values
    tri_d1 = z1
    tri_d2 = z2
    tri_d3 = z3

    ' Run 2d triangle rasterization at the projected points given, snapped to the resolution grid
    Call fast_tri_2d(snap(proj(x1, z1)), snap(proj(y1, z1)), snap(proj(x2, z2)), snap(proj(y2, z2)), snap(proj(x3, z3)), snap(proj(y3, z3)))
End Sub
