Public Class Form1

    Private Sub btn1_Click(sender As System.Object, e As System.EventArgs) Handles btn1.Click
        Select Case btn1.Text
            Case ""
                btn1.Text = "X"
            Case "X"
                btn1.Text = "O"
            Case "O"
                btn1.Text = "O"
        End Select

        TiraMaquina()

    End Sub

    Private Sub btn2_Click(sender As System.Object, e As System.EventArgs) Handles btn2.Click

        Select Case btn2.Text
            Case ""
                btn2.Text = "X"
            Case "X"
                btn2.Text = "O"
            Case "O"
                btn2.Text = "O"
        End Select

        TiraMaquina()

    End Sub

    Private Sub btn3_Click(sender As System.Object, e As System.EventArgs) Handles btn3.Click
        Select Case btn3.Text
            Case ""
                btn3.Text = "X"
            Case "X"
                btn3.Text = "O"
            Case "O"
                btn3.Text = "O"
        End Select

        TiraMaquina()

    End Sub

    Private Sub btn4_Click(sender As System.Object, e As System.EventArgs) Handles btn4.Click
        Select Case btn4.Text
            Case ""
                btn4.Text = "X"
            Case "X"
                btn4.Text = "O"
            Case "O"
                btn4.Text = "O"
        End Select
        TiraMaquina()

    End Sub

    Private Sub btn5_Click(sender As System.Object, e As System.EventArgs) Handles btn5.Click
        Select Case btn5.Text
            Case ""
                btn5.Text = "X"
            Case "X"
                btn5.Text = "O"
            Case "O"
                btn5.Text = "O"
        End Select
        TiraMaquina()

    End Sub

    Private Sub btn6_Click(sender As System.Object, e As System.EventArgs) Handles btn6.Click
        Select Case btn6.Text
            Case ""
                btn6.Text = "X"
            Case "X"
                btn6.Text = "O"
            Case "O"
                btn6.Text = "O"
        End Select
        TiraMaquina()

    End Sub

    Private Sub btn7_Click(sender As System.Object, e As System.EventArgs) Handles btn7.Click
        Select Case btn7.Text
            Case ""
                btn7.Text = "X"
            Case "X"
                btn7.Text = "O"
            Case "O"
                btn7.Text = "O"
        End Select
        TiraMaquina()

    End Sub

    Private Sub btn8_Click(sender As System.Object, e As System.EventArgs) Handles btn8.Click
        Select Case btn8.Text
            Case ""
                btn8.Text = "X"
            Case "X"
                btn8.Text = "O"
            Case "O"
                btn8.Text = "O"
        End Select
        TiraMaquina()

    End Sub

    Private Sub btn9_Click(sender As System.Object, e As System.EventArgs) Handles btn9.Click
        Select Case btn9.Text
            Case ""
                btn9.Text = "X"
            Case "X"
                btn9.Text = "O"
            Case "O"
                btn9.Text = "O"
        End Select
        TiraMaquina()

    End Sub

    Private Sub btnReset_Click(sender As System.Object, e As System.EventArgs) Handles btnReset.Click
        For Each boton As Control In gb1.Controls
            If boton.Name.Substring(0, 3) = "btn" Then
                boton.Text = ""
            End If
            If boton.Name.Substring(0, 2) = "rd" Then
                If boton.Name.Substring(2, 1) = "Y" Then
                    Dim y As RadioButton = boton
                    y.Checked = True
                End If
            End If
        Next
    End Sub

    Private Sub Button1_Click(sender As System.Object, e As System.EventArgs) Handles Button1.Click
        Dim p As New cPosicio
        p.llegir(gb1)
        Dim cl As New cPosicio
        cl.clone(p)
        Dim fills() As cPosicio
        fills = cl.fills
        MsgBox(fills.Length)
    End Sub

    Private Function factorial(n As Integer) As Integer
        If n = 1 Then Return 1
        Return n * factorial(n - 1)
    End Function

    Private Function f(n As Integer) As Integer
        If n = 0 Then Return 0
        If n = 1 Then Return 1
        Return f(n - 1) + f(n - 2)
    End Function

    Public Sub borrar()
        Dim x As New cPosicio
        x.tira = Tira.tiraJugador
        x.tauler(0, 2) = 1
    End Sub

    Private Enum Tira
        tiraJugador
        tiraMaquina
    End Enum

    Private Structure Valoracio
        Dim valor As Integer
        Dim tirada As Integer 'Numero d'ordre
    End Structure

    Private Class cPosicio
        Public tira As Tira
        Public tauler(,) As Integer = {{0, 0, 0}, {0, 0, 0}, {0, 0, 0}}

        Sub New()
        End Sub

        Sub New(ByVal gb As GroupBox)
            llegir(gb)
        End Sub

        Public Function peces() As Integer
            Dim num As Integer = 0
            For i = 0 To 2
                For j = 0 To 2
                    If tauler(i, j) <> 0 Then
                        num += 1
                    End If
                Next
            Next
            Return num
        End Function

        Public Sub presenta(ByVal gb As GroupBox)
            For Each boton As Control In gb.Controls
                If boton.Name.Substring(0, 3) = "btn" Then
                    Dim i As Integer = boton.Name.Substring(3, 1) - 1
                    Dim x As Integer = i Mod 3
                    Dim y As Integer = (i - x) / 3
                    'y = i \ 3   'Divisio sense resta
                    Select Case tauler(x, y)
                        Case 0
                            boton.Text = ""
                        Case 1
                            boton.Text = "X"
                        Case 2
                            boton.Text = "O"
                    End Select
                End If
            Next
        End Sub

        Public Sub llegir(ByVal gb As GroupBox)
            For Each boton As Control In gb.Controls
                If boton.Name.Substring(0, 3) = "btn" Then
                    Dim i As Integer = boton.Name.Substring(3, 1) - 1
                    Dim x As Integer = i Mod 3
                    Dim y As Integer = (i - x) / 3
                    Select Case boton.Text
                        Case ""
                            tauler(x, y) = 0
                        Case "X"
                            tauler(x, y) = 1
                        Case "O"
                            tauler(x, y) = 2
                    End Select
                End If
                If boton.Name.Substring(0, 2) = "rd" Then
                    Dim x As RadioButton = boton
                    If boton.Name.Substring(2, 1) = "X" Then
                        If x.Checked = True Then
                            tira = Form1.Tira.tiraJugador
                        Else
                            tira = Form1.Tira.tiraMaquina
                        End If
                    End If
                End If
            Next
        End Sub

        Public Function evaluacio(ByVal nivellQueda As Integer, ByRef nivellEstic As Integer) As Valoracio
            Dim retorn As New Valoracio
            If nivellQueda = 0 Then Return retorn
            If (tauler(0, 0) = tauler(0, 1) And tauler(0, 1) = tauler(0, 2)) And (tauler(0, 0) = 1) Or
                (tauler(1, 0) = tauler(1, 1) And tauler(1, 1) = tauler(1, 2)) And (tauler(1, 0) = 1) Or
                (tauler(2, 0) = tauler(2, 1) And tauler(2, 1) = tauler(2, 2)) And (tauler(2, 0) = 1) Or
                (tauler(0, 0) = tauler(1, 0) And tauler(1, 0) = tauler(2, 0)) And (tauler(0, 0) = 1) Or
                (tauler(0, 1) = tauler(1, 1) And tauler(1, 1) = tauler(2, 1)) And (tauler(0, 1) = 1) Or
                (tauler(0, 2) = tauler(1, 2) And tauler(1, 2) = tauler(2, 2)) And (tauler(0, 2) = 1) Or
                (tauler(0, 0) = tauler(1, 1) And tauler(1, 1) = tauler(2, 2)) And (tauler(0, 0) = 1) Or
                (tauler(2, 0) = tauler(1, 1) And tauler(1, 1) = tauler(0, 2)) And (tauler(2, 0) = 1) Then
                retorn.valor = 100 - nivellEstic + 1
                retorn.tirada = 0
            End If
            If (tauler(0, 0) = tauler(0, 1) And tauler(0, 1) = tauler(0, 2)) And (tauler(0, 0) = 2) Or
                (tauler(1, 0) = tauler(1, 1) And tauler(1, 1) = tauler(1, 2)) And (tauler(1, 0) = 2) Or
                (tauler(2, 0) = tauler(2, 1) And tauler(2, 1) = tauler(2, 2)) And (tauler(2, 0) = 2) Or
                (tauler(0, 0) = tauler(1, 0) And tauler(1, 0) = tauler(2, 0)) And (tauler(0, 0) = 2) Or
                (tauler(0, 1) = tauler(1, 1) And tauler(1, 1) = tauler(2, 1)) And (tauler(0, 1) = 2) Or
                (tauler(0, 2) = tauler(1, 2) And tauler(1, 2) = tauler(2, 2)) And (tauler(0, 2) = 2) Or
                (tauler(0, 0) = tauler(1, 1) And tauler(1, 1) = tauler(2, 2)) And (tauler(0, 0) = 2) Or
                (tauler(2, 0) = tauler(1, 1) And tauler(1, 1) = tauler(0, 2)) And (tauler(2, 0) = 2) Then
                retorn.valor = -100 + nivellEstic - 1
                retorn.tirada = 0
            End If

            If retorn.valor = 0 And nivellQueda > 1 Then
                Dim f() As cPosicio = Me.fills
                If f Is Nothing Then
                    Return retorn
                End If
                Dim maxmin As Integer = -999
                If Me.tira = Form1.Tira.tiraJugador Then
                    maxmin = -999
                Else
                    maxmin = 999
                End If
                Dim tirada As Integer = 0
                For i As Integer = 0 To f.Length - 1
                    Dim eval As Valoracio = f(i).evaluacio(nivellQueda - 1, nivellEstic + 1)
                    If Me.tira = Form1.Tira.tiraJugador Then
                        If eval.valor > maxmin Then
                            maxmin = eval.valor
                            tirada = i + 1
                        End If
                    Else
                        If eval.valor < maxmin Then
                            maxmin = eval.valor
                            tirada = i + 1
                        End If
                    End If
                Next
                If maxmin <> -999 Then
                    retorn.valor = maxmin
                    retorn.tirada = tirada
                End If
            End If
            Return retorn

        End Function

        Public Function fills() As cPosicio()
            Dim x As Integer = 0
            Dim y As Integer = 0
            Dim sortida() As cPosicio = Nothing
            Dim num As Integer = 0
            For x = 0 To 2
                For y = 0 To 2
                    If tauler(x, y) = 0 Then
                        ReDim Preserve sortida(num)
                        Dim fill As New cPosicio
                        fill.clone(Me)
                        If tira = Form1.Tira.tiraJugador Then
                            fill.tira = Form1.Tira.tiraMaquina
                        Else
                            fill.tira = Form1.Tira.tiraJugador
                        End If
                        If tira = Form1.Tira.tiraJugador Then
                            fill.tauler(x, y) = 1
                        Else
                            fill.tauler(x, y) = 2
                        End If
                        sortida(num) = fill
                        num += 1
                    End If
                Next
            Next
            Return sortida
        End Function

        Public Sub clone(c As cPosicio)
            tira = c.tira
            For x As Integer = 0 To 2
                For y As Integer = 0 To 2
                    tauler(x, y) = c.tauler(x, y)
                Next
            Next
        End Sub

        Protected Overrides Sub Finalize()
            MyBase.Finalize()
        End Sub
    End Class

    Private Sub Button2_Click(sender As System.Object, e As System.EventArgs) Handles Button2.Click
        Dim pr As Integer = ComboBox1.Text
        Dim p As New cPosicio(gb1)
        Dim q As Valoracio = p.evaluacio(pr, 0)
        MsgBox(q.valor & " " & q.tirada)
    End Sub

    Public Sub TiraMaquina()
        'Return

        Dim Execution_Start As New Stopwatch
        Execution_Start.Start()

        Dim p As New cPosicio(gb1)
        Dim q As Valoracio = p.evaluacio(9, 0)
        Dim i As Integer = q.tirada
        If i = 0 Then
            Guanyador()
            Exit Sub
        End If
        For x As Integer = 0 To 2
            For y As Integer = 0 To 2
                If p.tauler(x, y) = 0 Then
                    i = i - 1
                    If i = 0 Then
                        p.tauler(x, y) = 2
                        Exit For
                    End If
                End If
            Next
        Next
        Dim a As Valoracio = p.evaluacio(1, 0)
        p.presenta(gb1)
        If a.valor < -90 Or p.peces = 9 Then
            Guanyador()
        End If

        MsgBox("Hours: " & Execution_Start.Elapsed.Hours & " Minutes: " & Execution_Start.Elapsed.Minutes & " Seconds: " & Execution_Start.Elapsed.Seconds & " Miliseconds: " & Execution_Start.Elapsed.Milliseconds)
    End Sub

    Private Sub Guanyador()
        Dim p As New cPosicio(gb1)
        Dim i As Valoracio = p.evaluacio(1, 0)
        Select Case i.valor
            Case 0
                MsgBox("Empate")
            Case Is > 90
                MsgBox("Guanya Jugador")
            Case Is < -90
                MsgBox("Guanya Maquina")
        End Select
    End Sub

    Private Sub btnComençaOrdinador_Click(sender As System.Object, e As System.EventArgs) Handles btnComençaOrdinador.Click
        Dim p As New cPosicio
        Randomize()
        Dim x As Integer = CInt(Int(3 * Rnd()))
        Dim y As Integer = CInt(Int(3 * Rnd()))
        p.tauler(x, y) = 2
        p.presenta(gb1)
    End Sub
End Class
