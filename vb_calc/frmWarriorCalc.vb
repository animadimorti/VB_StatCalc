Imports System.IO

Public Class frmWarriorCalc

    Public gLevel As Double
    Public FileName As String = ""
    Public MaxBase As Integer = 100

    Private Function setEquipMod(ByVal EquipVal As Integer, ByVal MaxEquipVal As Integer, ByVal BlessValue As Integer) As Integer
        If EquipVal + BlessValue = 0 Then Return 0

        If EquipVal + BlessValue > Math.Floor(MaxEquipVal * 0.5) Then
            Return Math.Floor(MaxEquipVal * 0.5)
        Else
            Return EquipVal + BlessValue
        End If
    End Function

    Private Function setStatVal(ByVal BaseStat As Integer, ByVal WIAS As Integer) As Integer
        Dim MaxStat As Integer
        'set silly low stats
        If BaseStat <= 7 Then
            MaxStat = 15
        Else
            MaxStat = BaseStat * 2
        End If
        'set IAS mods
        If WIAS > MaxStat Then
            Return MaxStat
        Else
            Return WIAS
        End If
    End Function

    Private Function GetExpValues(ByVal TxtBx As Integer, ByVal StartBase As Integer, ByVal StatFactor As Integer, ByVal ClassFactor As Double, ByVal PTMFactor As Integer) As Integer
        Dim Exp As Integer

        Exp = 0
        Exp = Int((((TxtBx - StartBase) + 6) ^ 3) * (StatFactor * ClassFactor))
        If TxtBx >= MaxBase Then Exp += 3000000 * PTMFactor
        Return Exp

    End Function

    Private Function GetLevelExp(ByVal TxtBx As Integer, ByVal StartBase As Integer, ByVal StatFactor As Integer, ByVal ClassFactor As Double, ByVal PTMFactor As Integer) As Integer
        Dim sVal, Exp As Integer

        Exp = 0
        sVal = StartBase
        Do Until sVal = TxtBx
            Exp += GetExpValues(sVal, StartBase, StatFactor, ClassFactor, PTMFactor)
            sVal += 1
        Loop
        Return Exp

    End Function

    Private Sub SetExpValues()
        Dim StartBase, StatFactor, PTMFactor As Integer
        Dim ClassFactor As Double

        If Hardcore_Chk.Checked = True Then MaxBase = 122 Else MaxBase = 115

        StartBase = 10
        StatFactor = 3
        ClassFactor = 0.1
        PTMFactor = 0


        HP_ExpLbl.Text = GetExpValues(Int(HP_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        End_ExpLbl.Text = GetExpValues(Int(End_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        ' WIAS EXP Calculations
        StatFactor = 2
        PTMFactor = 2
        Wis_ExpLbl.Text = GetExpValues(Int(Wis_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Int_ExpLbl.Text = GetExpValues(Int(Int_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Agi_ExpLbl.Text = GetExpValues(Int(Agi_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Str_ExpLbl.Text = GetExpValues(Int(Str_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        ' Skills and Spells EXP Calulations
        StartBase = 1
        StatFactor = 1
        PTMFactor = 1

        Dagger_ExpLbl.Text = GetExpValues(Int(Dagger_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        H2H_ExpLbl.Text = GetExpValues(Int(H2H_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Sword_ExpLbl.Text = GetExpValues(Int(Sword_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TwoHand_ExpLbl.Text = GetExpValues(Int(TwoHand_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Rage_ExpLbl.Text = GetExpValues(Int(Rage_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        AS_ExpLbl.Text = GetExpValues(Int(AS_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Attack_ExpLbl.Text = GetExpValues(Int(Attack_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Parry_ExpLbl.Text = GetExpValues(Int(Parry_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Tactics_ExpLbl.Text = GetExpValues(Int(Tactics_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Warcry_ExpLbl.Text = GetExpValues(Int(Warcry_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Surround_ExpLbl.Text = GetExpValues(Int(Surround_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Body_ExpLbl.Text = GetExpValues(Int(Body_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Speed_ExpLbl.Text = GetExpValues(Int(Speed_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        Barter_ExpLbl.Text = GetExpValues(Int(Barter_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Perc_ExpLbl.Text = GetExpValues(Int(Perc_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Stealth_ExpLbl.Text = GetExpValues(Int(Stealth_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Regen_ExpLbl.Text = GetExpValues(Int(Regen_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        Immunity_ExpLbl.Text = GetExpValues(Int(Immunity_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        ' Profession EXP Calculations
        StatFactor = 3
        PTMFactor = 0
        Prof_ExpLbl.Text = GetExpValues(Int(Prof_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        SetMods()

    End Sub

    Private Sub SetMods()
        Dim WisMod, IntMod, AgiMod, StrMod As Integer
        Dim IAS, SSA, WIS, IIW, SSS As Integer
        Dim ImmTactBonus, WCTactBonus, RageBonus, BlessBonus As Integer

        If ShowRage_Chk.Checked = True Then RageBonus = Int(RageBonus_TxtBx.Text) Else RageBonus = 0

        If Max_Bless_Chk.Checked = True Then BlessBonus = 1000 Else BlessBonus = 0

        HP_Mod_Lbl.Text = Int(HP_TxtBx.Text) + setEquipMod(Int(HP_Mod_TxtBx.Text), Int(HP_TxtBx.Text), 0)
        End_Mod_Lbl.Text = Int(End_TxtBx.Text) + setEquipMod(Int(End_Mod_TxtBx.Text), Int(End_TxtBx.Text), 0)

        Wis_Mod_Lbl.Text = Int(Wis_TxtBx.Text) + setEquipMod(Int(Wis_Mod_TxtBx.Text), Int(Wis_TxtBx.Text), BlessBonus) + Int(LDW_TxtBx.Text / 2) + Int(CW_TxtBx.Text)
        Int_Mod_Lbl.Text = Int(Int_TxtBx.Text) + setEquipMod(Int(Int_Mod_TxtBx.Text), Int(Int_TxtBx.Text), BlessBonus) + Int(LDW_TxtBx.Text / 2) + Int(CW_TxtBx.Text)
        Agi_Mod_Lbl.Text = Int(Agi_TxtBx.Text) + setEquipMod(Int(Agi_Mod_TxtBx.Text), Int(Agi_TxtBx.Text), BlessBonus) + Int(LDW_TxtBx.Text / 2) + Int(CW_TxtBx.Text)
        Str_Mod_Lbl.Text = Int(Str_TxtBx.Text) + setEquipMod(Int(Str_Mod_TxtBx.Text), Int(Str_TxtBx.Text), BlessBonus) + Int(LDW_TxtBx.Text / 2) + Int(CW_TxtBx.Text)

        WisMod = Int(Wis_Mod_Lbl.Text)
        IntMod = Int(Int_Mod_Lbl.Text)
        AgiMod = Int(Agi_Mod_Lbl.Text)
        StrMod = Int(Str_Mod_Lbl.Text)

        IAS = Math.Floor((IntMod + AgiMod + StrMod) / 5)
        SSA = Math.Floor((StrMod + StrMod + AgiMod) / 5)
        WIS = Math.Floor((WisMod + IntMod + StrMod) / 5)
        IIW = Math.Floor((IntMod + IntMod + WisMod) / 5)
        SSS = Math.Floor((StrMod + StrMod + StrMod) / 5)

        Dagger_Mod_Lbl.Text = Int(Dagger_TxtBx.Text) + setStatVal(Int(Dagger_TxtBx.Text), IAS) + setEquipMod(Int(Dagger_Mod_TxtBx.Text), Int(Dagger_TxtBx.Text), 0)
        H2H_Mod_Lbl.Text = Int(H2H_TxtBx.Text) + setStatVal(Int(H2H_TxtBx.Text), SSA) + setEquipMod(Int(H2H_Mod_TxtBx.Text), Int(H2H_TxtBx.Text), 0)
        Sword_Mod_Lbl.Text = Int(Sword_TxtBx.Text) + setStatVal(Int(Sword_TxtBx.Text), IAS) + setEquipMod(Int(Sword_Mod_TxtBx.Text), Int(Sword_TxtBx.Text), 0)
        TwoHand_Mod_Lbl.Text = Int(TwoHand_TxtBx.Text) + setStatVal(Int(TwoHand_TxtBx.Text), SSA) + setEquipMod(Int(TwoHand_Mod_TxtBx.Text), Int(TwoHand_TxtBx.Text), 0)
        Rage_Mod_Lbl.Text = Int(Rage_TxtBx.Text) + setStatVal(Int(Rage_TxtBx.Text), SSA) + setEquipMod(Int(Rage_Mod_TxtBx.Text), Int(Rage_TxtBx.Text), 0)

        Attack_Mod_Lbl.Text = Int(Attack_TxtBx.Text) + setStatVal(Int(Attack_TxtBx.Text), IAS) + setEquipMod(Int(Attack_Mod_TxtBx.Text), Int(Attack_TxtBx.Text), 0)
        Parry_Mod_Lbl.Text = Int(Parry_TxtBx.Text) + setStatVal(Int(Parry_TxtBx.Text), IAS) + setEquipMod(Int(Parry_Mod_TxtBx.Text), Int(Parry_TxtBx.Text), 0)
        Tactics_Mod_Lbl.Text = Int(Tactics_TxtBx.Text) + setStatVal(Int(Tactics_TxtBx.Text), IAS) + setEquipMod(Int(Tactics_Mod_TxtBx.Text), Int(Tactics_TxtBx.Text), 0)
        If ShowTact_Chk.Checked = True Then ImmTactBonus = Int((Tactics_Mod_Lbl.Text + 14) * 0.125) Else ImmTactBonus = 0
        If ShowTact_Chk.Checked = True Then WCTactBonus = Int(Tactics_Mod_Lbl.Text * 0.125) Else WCTactBonus = 0
        Warcry_Mod_Lbl.Text = Int(Warcry_TxtBx.Text) + setStatVal(Int(Warcry_TxtBx.Text), IAS) + setEquipMod(Int(Warcry_Mod_TxtBx.Text), Int(Warcry_TxtBx.Text), 0) + WCTactBonus
        Surround_Mod_Lbl.Text = Int(Surround_TxtBx.Text) + setStatVal(Int(Surround_TxtBx.Text), IAS) + setEquipMod(Int(Surround_Mod_TxtBx.Text), Int(Surround_TxtBx.Text), 0)
        Body_Mod_Lbl.Text = Int(Body_TxtBx.Text) + setStatVal(Int(Body_TxtBx.Text), IAS) + setEquipMod(Int(Body_Mod_TxtBx.Text), Int(Body_TxtBx.Text), 0)
        Speed_Mod_Lbl.Text = Int(Speed_TxtBx.Text) + setStatVal(Int(Speed_TxtBx.Text), IAS) + setEquipMod(Int(Speed_Mod_TxtBx.Text), Int(Speed_TxtBx.Text), 0)

        Barter_Mod_Lbl.Text = Int(Barter_TxtBx.Text) + setStatVal(Int(Barter_TxtBx.Text), IIW) + setEquipMod(Int(Barter_Mod_TxtBx.Text), Int(Barter_TxtBx.Text), 0)
        Perc_Mod_Lbl.Text = Int(Perc_TxtBx.Text) + setStatVal(Int(Perc_TxtBx.Text), IIW) + setEquipMod(Int(Perc_Mod_TxtBx.Text), Int(Perc_TxtBx.Text), 0) + Int(Thief_TxtBx.Text / 2)
        Stealth_Mod_Lbl.Text = Int(Stealth_TxtBx.Text) + setStatVal(Int(Stealth_TxtBx.Text), IIW) + setEquipMod(Int(Stealth_Mod_TxtBx.Text), Int(Perc_TxtBx.Text), 0) + Int(Thief_TxtBx.Text)
        Regen_Mod_Lbl.Text = Int(Regen_TxtBx.Text) + setStatVal(Int(Regen_TxtBx.Text), SSS) + setEquipMod(Int(Regen_Mod_TxtBx.Text), Int(Regen_TxtBx.Text), 0)
        Immunity_Mod_Lbl.Text = Int(Immunity_TxtBx.Text) + setStatVal(Int(Immunity_TxtBx.Text), WIS) + setEquipMod(Int(Immunity_Mod_TxtBx.Text), Int(Immunity_TxtBx.Text), 0) + ImmTactBonus

        SetLevel()

    End Sub

    Private Sub SetLevel()
        Dim TotalExp, ReqLevel As Double
        Dim StartBase, StatFactor, PTMFactor As Integer
        Dim ClassFactor As Double

        TotalExp = 0
        StartBase = 10
        StatFactor = 3
        ClassFactor = 0.1
        PTMFactor = 0

        TotalExp += GetLevelExp(Int(HP_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(End_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        ' WIAS EXP Calculations
        StatFactor = 2
        PTMFactor = 2
        TotalExp += GetLevelExp(Int(Wis_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Int_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Agi_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Str_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        ' Skills and Spells EXP Calulations
        StartBase = 1
        StatFactor = 1
        PTMFactor = 1

        TotalExp += GetLevelExp(Int(Dagger_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(H2H_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Sword_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(TwoHand_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Rage_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        TotalExp += GetLevelExp(Int(AS_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Attack_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Parry_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Tactics_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Warcry_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Surround_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Body_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Speed_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        TotalExp += GetLevelExp(Int(Barter_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Perc_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Stealth_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Regen_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)
        TotalExp += GetLevelExp(Int(Immunity_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        ' Profession EXP Calculations
        StatFactor = 3
        PTMFactor = 0
        TotalExp += GetLevelExp(Int(Prof_TxtBx.Text), StartBase, StatFactor, ClassFactor, PTMFactor)

        ReqLevel = TotalExp ^ 0.25
        If ReqLevel = 0 Then Level_TxtBx.Text = 1 Else Level_TxtBx.Text = ReqLevel

        'Whitestar Mod
        gLevel = ReqLevel
        SetExtraValues()

    End Sub

    Private Sub SetExtraValues()
        Dim wv, av As Double
        Dim ws1, ws2, ws_max, ws_type As Integer
        Dim RageBonus, Duration As Integer

        'Calculate Armour Value
        If Int(AS_TxtBx.Text) >= 110 Then av = 96.5 Else av = 6.5 + (Math.Floor(Int(AS_TxtBx.Text) / 10 + 1) * 7.5)

        AV_TxtBx.Text = av + (Int(Body_Mod_Lbl.Text) + Int(AS_TxtBx.Text)) * 0.25

        'Calculate tactics 
        ImmTactBonus_TxtBx.Text = Int(Int(Tactics_Mod_Lbl.Text + 14) * 0.125)
        WCTactBonus_TxtBx.Text = Int(Int(Tactics_Mod_Lbl.Text) * 0.125)
        OffDefTactBonus_TxtBx.Text = Int(Int(Tactics_Mod_Lbl.Text) * 0.375)
        ImmNxtTact.Text = Int(((Int(ImmTactBonus_TxtBx.Text) + 1) / 0.125) + 0.99) - 14
        WCNxtTact.Text = Int(((Int(WCTactBonus_TxtBx.Text) + 1) / 0.125) + 0.99)
        OffDefNxtTact.Text = Int(((Int(OffDefTactBonus_TxtBx.Text) + 1) / 0.375) + 0.99)

        'Determine Highest Weapon Skill
        ws1 = Math.Max(Int(Sword_Mod_Lbl.Text), Int(TwoHand_Mod_Lbl.Text))
        ws2 = Math.Max(Int(Dagger_Mod_Lbl.Text), Int(H2H_Mod_Lbl.Text))
        ws_max = Math.Max(ws1, ws2)

        If ws_max = Int(Dagger_Mod_Lbl.Text) Then
            ws_type = 0
        ElseIf ws_max = Int(H2H_Mod_Lbl.Text) Then
            ws_type = 1
        ElseIf ws_max = Int(Sword_Mod_Lbl.Text) Then
            ws_type = 2
        ElseIf ws_max = Int(TwoHand_Mod_Lbl.Text) Then
            ws_type = 3
        End If

        'Calculate Weapon Value
        If ws_type = 0 Then
            If Int(Dagger_TxtBx.Text) >= 110 Then
                wv = 121
            Else
                wv = 1 + (Math.Floor(Int(Dagger_TxtBx.Text) / 10 + 1) * 10)
            End If
        ElseIf ws_type = 1 Then
            If Int(Body_Mod_Lbl.Text) >= 180 Then wv = 90 Else wv = Int(Body_Mod_Lbl.Text) * 0.5
        ElseIf ws_type = 2 Then
            If Int(Sword_TxtBx.Text) >= 110 Then
                wv = 124
            Else
                wv = 4 + (Math.Floor(Int(Sword_TxtBx.Text) / 10 + 1) * 10)
            End If
        ElseIf ws_type = 3 Then
            If Int(TwoHand_TxtBx.Text) >= 110 Then
                wv = 127
            Else
                wv = 7 + (Math.Floor(Int(TwoHand_TxtBx.Text) / 10 + 1) * 10)
            End If
        End If

        WV_TxtBx.Text = Int(wv + Int(Body_Mod_Lbl.Text) * 0.25)

        'Calculate Rage
        RageBonus_TxtBx.Text = Int(Int(Rage_Mod_Lbl.Text) / 5)
        NxtRage.Text = Int((Int(RageBonus_TxtBx.Text) + 1) * 5)

        'Offence/Defence Calculations
        If ShowTact_Chk.Checked = True Then
            Offense_TxtBx.Text = Int((Int(Attack_Mod_Lbl.Text) * 2) + Int(ws_max)) + Int(OffDefTactBonus_TxtBx.Text)
            Defense_TxtBx.Text = Int((Int(Parry_Mod_Lbl.Text) * 2) + Int(ws_max)) + Int(OffDefTactBonus_TxtBx.Text)
        Else
            Int(OffDefTactBonus_TxtBx.Text)
            Offense_TxtBx.Text = Int((Int(Attack_Mod_Lbl.Text) * 2) + Int(ws_max))
            Defense_TxtBx.Text = Int((Int(Parry_Mod_Lbl.Text) * 2) + Int(ws_max))
        End If
        If ShowRage_Chk.Checked = True Then Offense_TxtBx.Text = Int(Offense_TxtBx.Text) + RageBonus
        If ShowRage_Chk.Checked = True Then Defense_TxtBx.Text = Int(Defense_TxtBx.Text) + RageBonus

        'Speed Calculations
        RawSpeed_TxtBx.Text = Int((Int(Agi_Mod_Lbl.Text) + Int(Agi_Mod_Lbl.Text) + Int(Str_Mod_Lbl.Text)) / 5) + (Int(Ath_TxtBx.Text) * 3) + Int(Speed_Mod_Lbl.Text / 2)

        'Calculate Warcry
        If ShowTact_Chk.Checked = True Then
            WCShield_TxtBx.Text = (Int(Warcry_Mod_Lbl.Text) - Int(WCTactBonus_TxtBx.Text)) / 2
            WCCost_TxtBx.Text = (Int(Warcry_Mod_Lbl.Text) - Int(WCTactBonus_TxtBx.Text)) / 3
        Else
            WCShield_TxtBx.Text = Int(Warcry_Mod_Lbl.Text) / 2
            WCCost_TxtBx.Text = Int(Warcry_Mod_Lbl.Text) / 3
        End If

        'Calculate Warcry Duration
        If Arch_Chk.Checked = True Then Duration = Int(Level_TxtBx.Text) / 2 Else Duration = 0
        WCDur_TxtBx.Text = Math.Round((4 + (4 * Duration / 35)) * 100) / 100

        ValidateData()

    End Sub

    Private Sub ValidateData()
        If Int(HP_Mod_TxtBx.Text) > Math.Floor(Int(HP_TxtBx.Text) / 2) Then HP_Mod_TxtBx.ForeColor = Color.Red Else HP_Mod_TxtBx.ForeColor = Color.Black
        If Int(End_Mod_TxtBx.Text) > Math.Floor(Int(End_TxtBx.Text) / 2) Then End_Mod_TxtBx.ForeColor = Color.Red Else End_Mod_TxtBx.ForeColor = Color.Black
        If Int(Wis_Mod_TxtBx.Text) > Math.Floor(Int(Wis_TxtBx.Text) / 2) Then Wis_Mod_TxtBx.ForeColor = Color.Red Else Wis_Mod_TxtBx.ForeColor = Color.Black
        If Int(Int_Mod_TxtBx.Text) > Math.Floor(Int(Int_TxtBx.Text) / 2) Then Int_Mod_TxtBx.ForeColor = Color.Red Else Int_Mod_TxtBx.ForeColor = Color.Black
        If Int(Agi_Mod_TxtBx.Text) > Math.Floor(Int(Agi_TxtBx.Text) / 2) Then Agi_Mod_TxtBx.ForeColor = Color.Red Else Agi_Mod_TxtBx.ForeColor = Color.Black
        If Int(Str_Mod_TxtBx.Text) > Math.Floor(Int(Str_TxtBx.Text) / 2) Then Str_Mod_TxtBx.ForeColor = Color.Red Else Str_Mod_TxtBx.ForeColor = Color.Black
        If Int(Dagger_Mod_TxtBx.Text) > Math.Floor(Int(Dagger_TxtBx.Text) / 2) Then Dagger_Mod_TxtBx.ForeColor = Color.Red Else Dagger_Mod_TxtBx.ForeColor = Color.Black
        If Int(H2H_Mod_TxtBx.Text) > Math.Floor(Int(H2H_TxtBx.Text) / 2) Then H2H_Mod_TxtBx.ForeColor = Color.Red Else H2H_Mod_TxtBx.ForeColor = Color.Black
        If Int(Sword_Mod_TxtBx.Text) > Math.Floor(Int(Sword_TxtBx.Text) / 2) Then Sword_Mod_TxtBx.ForeColor = Color.Red Else Sword_Mod_TxtBx.ForeColor = Color.Black
        If Int(TwoHand_Mod_TxtBx.Text) > Math.Floor(Int(TwoHand_TxtBx.Text) / 2) Then TwoHand_Mod_TxtBx.ForeColor = Color.Red Else TwoHand_Mod_TxtBx.ForeColor = Color.Black
        If Int(Rage_Mod_TxtBx.Text) > Math.Floor(Int(Rage_TxtBx.Text) / 2) Then Rage_Mod_TxtBx.ForeColor = Color.Red Else Rage_Mod_TxtBx.ForeColor = Color.Black
        If Int(Attack_Mod_TxtBx.Text) > Math.Floor(Int(Attack_TxtBx.Text) / 2) Then Attack_Mod_TxtBx.ForeColor = Color.Red Else Attack_Mod_TxtBx.ForeColor = Color.Black
        If Int(Parry_Mod_TxtBx.Text) > Math.Floor(Int(Parry_TxtBx.Text) / 2) Then Parry_Mod_TxtBx.ForeColor = Color.Red Else Parry_Mod_TxtBx.ForeColor = Color.Black
        If Int(Tactics_Mod_TxtBx.Text) > Math.Floor(Int(Tactics_TxtBx.Text) / 2) Then Tactics_Mod_TxtBx.ForeColor = Color.Red Else Tactics_Mod_TxtBx.ForeColor = Color.Black
        If Int(Warcry_Mod_TxtBx.Text) > Math.Floor(Int(Warcry_TxtBx.Text) / 2) Then Warcry_Mod_TxtBx.ForeColor = Color.Red Else Warcry_Mod_TxtBx.ForeColor = Color.Black
        If Int(Surround_Mod_TxtBx.Text) > Math.Floor(Int(Surround_TxtBx.Text) / 2) Then Surround_Mod_TxtBx.ForeColor = Color.Red Else Surround_Mod_TxtBx.ForeColor = Color.Black
        If Int(Speed_Mod_TxtBx.Text) > Math.Floor(Int(Speed_TxtBx.Text) / 2) Then Speed_Mod_TxtBx.ForeColor = Color.Red Else Speed_Mod_TxtBx.ForeColor = Color.Black
        If Int(Immunity_Mod_TxtBx.Text) > Math.Floor(Int(Immunity_TxtBx.Text) / 2) Then Immunity_Mod_TxtBx.ForeColor = Color.Red Else Immunity_Mod_TxtBx.ForeColor = Color.Black
        If Int(Barter_Mod_TxtBx.Text) > Math.Floor(Int(Barter_TxtBx.Text) / 2) Then Barter_Mod_TxtBx.ForeColor = Color.Red Else Barter_Mod_TxtBx.ForeColor = Color.Black
        If Int(Perc_Mod_TxtBx.Text) > Math.Floor(Int(Perc_TxtBx.Text) / 2) Then Perc_Mod_TxtBx.ForeColor = Color.Red Else Perc_Mod_TxtBx.ForeColor = Color.Black
        If Int(Stealth_Mod_TxtBx.Text) > Math.Floor(Int(Stealth_TxtBx.Text) / 2) Then Stealth_Mod_TxtBx.ForeColor = Color.Red Else Stealth_Mod_TxtBx.ForeColor = Color.Black
        If Int(Regen_Mod_TxtBx.Text) > Math.Floor(Int(Regen_TxtBx.Text) / 2) Then Regen_Mod_TxtBx.ForeColor = Color.Red Else Regen_Mod_TxtBx.ForeColor = Color.Black
        If Int(Body_Mod_TxtBx.Text) > Math.Floor(Int(Body_TxtBx.Text) / 2) Then Body_Mod_TxtBx.ForeColor = Color.Red Else Body_Mod_TxtBx.ForeColor = Color.Black


    End Sub

    Private Sub HP_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles HP_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(HP_TxtBx.Text) >= MaxBase Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                HP_TxtBx.Text = Int(HP_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(HP_TxtBx.Text) = 10 Then
                    MsgBox("Can not reduce below 10")
                    Exit Sub
                End If

                HP_TxtBx.Text = Int(HP_TxtBx.Text) - 1

        End Select

        SetExpValues()
    End Sub

    Private Sub End_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles End_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(End_TxtBx.Text) >= MaxBase Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                End_TxtBx.Text = Int(End_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(End_TxtBx.Text) = 10 Then
                    MsgBox("Can not reduce below 10")
                    Exit Sub
                End If

                End_TxtBx.Text = Int(End_TxtBx.Text) - 1

        End Select

        SetExpValues()
    End Sub

    Private Sub Wis_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Wis_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Wis_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Wis_TxtBx.Text = Int(Wis_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Wis_TxtBx.Text) = 10 Then
                    MsgBox("Can not reduce below 10")
                    Exit Sub
                End If

                Wis_TxtBx.Text = Int(Wis_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Int_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Int_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Int_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Int_TxtBx.Text = Int(Int_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Int_TxtBx.Text) = 10 Then
                    MsgBox("Can not reduce below 10")
                    Exit Sub
                End If

                Int_TxtBx.Text = Int(Int_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Agi_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Agi_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Agi_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Agi_TxtBx.Text = Int(Agi_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Agi_TxtBx.Text) = 10 Then
                    MsgBox("Can not reduce below 10")
                    Exit Sub
                End If

                Agi_TxtBx.Text = Int(Agi_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Str_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Str_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Str_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Str_TxtBx.Text = Int(Str_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Str_TxtBx.Text) = 10 Then
                    MsgBox("Can not reduce below 10")
                    Exit Sub
                End If

                Str_TxtBx.Text = Int(Str_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Dagger_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Dagger_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Dagger_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Dagger_TxtBx.Text = Int(Dagger_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Dagger_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Dagger_TxtBx.Text = Int(Dagger_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub H2H_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles H2H_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(H2H_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                H2H_TxtBx.Text = Int(H2H_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(H2H_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                H2H_TxtBx.Text = Int(H2H_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Sword_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Sword_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Sword_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Sword_TxtBx.Text = Int(Sword_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Sword_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Sword_TxtBx.Text = Int(Sword_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub TwoHand_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles TwoHand_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(TwoHand_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                TwoHand_TxtBx.Text = Int(TwoHand_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(TwoHand_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                TwoHand_TxtBx.Text = Int(TwoHand_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Rage_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Rage_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Rage_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Rage_TxtBx.Text = Int(Rage_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Rage_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Rage_TxtBx.Text = Int(Rage_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Attack_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Attack_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Attack_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Attack_TxtBx.Text = Int(Attack_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Attack_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Attack_TxtBx.Text = Int(Attack_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Parry_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Parry_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Parry_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Parry_TxtBx.Text = Int(Parry_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Parry_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Parry_TxtBx.Text = Int(Parry_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Tactics_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Tactics_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Tactics_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Tactics_TxtBx.Text = Int(Tactics_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Tactics_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Tactics_TxtBx.Text = Int(Tactics_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Warcry_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Warcry_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Warcry_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Warcry_TxtBx.Text = Int(Warcry_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Warcry_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Warcry_TxtBx.Text = Int(Warcry_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Surround_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Surround_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Surround_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Surround_TxtBx.Text = Int(Surround_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Surround_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Surround_TxtBx.Text = Int(Surround_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Speed_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Speed_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Speed_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Speed_TxtBx.Text = Int(Speed_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Speed_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Speed_TxtBx.Text = Int(Speed_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Immunity_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Immunity_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Immunity_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Immunity_TxtBx.Text = Int(Immunity_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Immunity_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Immunity_TxtBx.Text = Int(Immunity_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Barter_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Barter_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Barter_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Barter_TxtBx.Text = Int(Barter_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Barter_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Barter_TxtBx.Text = Int(Barter_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Perc_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Perc_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Perc_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Perc_TxtBx.Text = Int(Perc_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Perc_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Perc_TxtBx.Text = Int(Perc_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Stealth_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Stealth_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Stealth_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Stealth_TxtBx.Text = Int(Stealth_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Stealth_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Stealth_TxtBx.Text = Int(Stealth_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Regen_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Regen_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Regen_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Regen_TxtBx.Text = Int(Regen_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Regen_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Regen_TxtBx.Text = Int(Regen_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub AS_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles AS_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(AS_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                AS_TxtBx.Text = Int(AS_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(AS_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                AS_TxtBx.Text = Int(AS_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Body_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Body_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Body_TxtBx.Text) >= 250 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                Body_TxtBx.Text = Int(Body_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Body_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Body_TxtBx.Text = Int(Body_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub Prof_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Prof_Btn.MouseDown

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Prof_TxtBx.Text) = 100 Then
                    MsgBox("Can not raise beyond 100")
                    Exit Sub
                End If

                Prof_TxtBx.Text = Int(Prof_TxtBx.Text) + 1

            Case Windows.Forms.MouseButtons.Right
                If Int(Prof_TxtBx.Text) = 1 Then
                    MsgBox("Can not reduce below 1")
                    Exit Sub
                End If

                Prof_TxtBx.Text = Int(Prof_TxtBx.Text) - 1

        End Select

        SetExpValues()

    End Sub

    Private Sub LDW_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles LDW_Btn.MouseDown
        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(LDW_TxtBx.Text) >= 30 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                If LDW_TxtBx.Text < 6 Then LDW_TxtBx.Text = 6 Else LDW_TxtBx.Text = (Int(LDW_TxtBx.Text / 3) + 1) * 3

            Case Windows.Forms.MouseButtons.Right
                If Int(LDW_TxtBx.Text) = 0 Then
                    MsgBox("Can not reduce below 0")
                    Exit Sub
                End If

                If LDW_TxtBx.Text <= 6 Then LDW_TxtBx.Text = 0 Else LDW_TxtBx.Text = (Math.Ceiling(LDW_TxtBx.Text / 3) - 1) * 3

        End Select

        SetExpValues()
    End Sub

    Private Sub CW_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles CW_Btn.MouseDown
        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(CW_TxtBx.Text) >= 30 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                If CW_TxtBx.Text < 6 Then CW_TxtBx.Text = 6 Else CW_TxtBx.Text = (Int(CW_TxtBx.Text / 3) + 1) * 3

            Case Windows.Forms.MouseButtons.Right
                If Int(CW_TxtBx.Text) = 0 Then
                    MsgBox("Can not reduce below 0")
                    Exit Sub
                End If

                If CW_TxtBx.Text <= 6 Then CW_TxtBx.Text = 0 Else CW_TxtBx.Text = (Math.Ceiling(CW_TxtBx.Text / 3) - 1) * 3

        End Select

        SetExpValues()
    End Sub

    Private Sub Ath_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Ath_Btn.MouseDown
        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Ath_TxtBx.Text) >= 30 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                If Ath_TxtBx.Text < 6 Then Ath_TxtBx.Text = 6 Else Ath_TxtBx.Text = (Int(Ath_TxtBx.Text / 3) + 1) * 3

            Case Windows.Forms.MouseButtons.Right
                If Int(Ath_TxtBx.Text) = 0 Then
                    MsgBox("Can not reduce below 0")
                    Exit Sub
                End If

                If Ath_TxtBx.Text <= 6 Then Ath_TxtBx.Text = 0 Else Ath_TxtBx.Text = (Math.Ceiling(Ath_TxtBx.Text / 3) - 1) * 3

        End Select

        SetExpValues()
    End Sub

    Private Sub Thief_Btn_MouseDown(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Thief_Btn.MouseDown
        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left
                If Int(Thief_TxtBx.Text) >= 30 Then
                    MsgBox("Cannot raise stats beyond MaxBase")
                    Exit Sub
                End If

                If Thief_TxtBx.Text < 6 Then Thief_TxtBx.Text = 6 Else Thief_TxtBx.Text = (Int(Thief_TxtBx.Text / 3) + 1) * 3

            Case Windows.Forms.MouseButtons.Right
                If Int(Thief_TxtBx.Text) = 0 Then
                    MsgBox("Can not reduce below 0")
                    Exit Sub
                End If

                If Thief_TxtBx.Text <= 6 Then Thief_TxtBx.Text = 0 Else Thief_TxtBx.Text = (Math.Ceiling(Thief_TxtBx.Text / 3) - 1) * 3

        End Select

        SetExpValues()
    End Sub

    Private Sub HP_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HP_TxtBx.Leave

        If Int(HP_TxtBx.Text) > MaxBase Then HP_TxtBx.Text = MaxBase
        If Int(HP_TxtBx.Text) < 10 Then HP_TxtBx.Text = 10

        SetExpValues()
    End Sub

    Private Sub End_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles End_TxtBx.Leave

        If Int(End_TxtBx.Text) > MaxBase Then End_TxtBx.Text = MaxBase
        If Int(End_TxtBx.Text) < 10 Then End_TxtBx.Text = 10

        SetExpValues()
    End Sub

    Private Sub Wis_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Wis_TxtBx.Leave
        If Int(Wis_TxtBx.Text) > 250 Then Wis_TxtBx.Text = 250
        If Int(Wis_TxtBx.Text) < 10 Then Wis_TxtBx.Text = 10

        SetExpValues()
    End Sub

    Private Sub Int_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Int_TxtBx.Leave
        If Int(Int_TxtBx.Text) > 250 Then Int_TxtBx.Text = 250
        If Int(Int_TxtBx.Text) < 10 Then Int_TxtBx.Text = 10

        SetExpValues()
    End Sub

    Private Sub Agi_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Agi_TxtBx.Leave
        If Int(Agi_TxtBx.Text) > 250 Then Agi_TxtBx.Text = 250
        If Int(Agi_TxtBx.Text) < 10 Then Agi_TxtBx.Text = 10

        SetExpValues()
    End Sub

    Private Sub Str_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Str_TxtBx.Leave
        If Int(Str_TxtBx.Text) > 250 Then Str_TxtBx.Text = 250
        If Int(Str_TxtBx.Text) < 10 Then Str_TxtBx.Text = 10

        SetExpValues()
    End Sub

    Private Sub Dagger_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Dagger_TxtBx.Leave
        If Int(Dagger_TxtBx.Text) > 250 Then Dagger_TxtBx.Text = 250
        If Int(Dagger_TxtBx.Text) < 1 Then Dagger_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub H2H_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles H2H_TxtBx.Leave
        If Int(H2H_TxtBx.Text) > 250 Then H2H_TxtBx.Text = 250
        If Int(H2H_TxtBx.Text) < 1 Then H2H_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Sword_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Sword_TxtBx.Leave
        If Int(Sword_TxtBx.Text) > 250 Then Sword_TxtBx.Text = 250
        If Int(Sword_TxtBx.Text) < 1 Then Sword_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub TwoHand_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TwoHand_TxtBx.Leave
        If Int(TwoHand_TxtBx.Text) > 250 Then TwoHand_TxtBx.Text = 250
        If Int(TwoHand_TxtBx.Text) < 1 Then TwoHand_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Rage_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Rage_TxtBx.Leave
        If Int(Rage_TxtBx.Text) > 250 Then Rage_TxtBx.Text = 250
        If Int(Rage_TxtBx.Text) < 1 Then Rage_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Attack_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Attack_TxtBx.Leave
        If Int(Attack_TxtBx.Text) > 250 Then Attack_TxtBx.Text = 250
        If Int(Attack_TxtBx.Text) < 1 Then Attack_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Parry_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Parry_TxtBx.Leave
        If Int(Parry_TxtBx.Text) > 250 Then Parry_TxtBx.Text = 250
        If Int(Parry_TxtBx.Text) < 1 Then Parry_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Tactics_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Tactics_TxtBx.Leave
        If Int(Tactics_TxtBx.Text) > 250 Then Tactics_TxtBx.Text = 250
        If Int(Tactics_TxtBx.Text) < 1 Then Tactics_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Warcry_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Warcry_TxtBx.Leave
        If Int(Warcry_TxtBx.Text) > 250 Then Warcry_TxtBx.Text = 250
        If Int(Warcry_TxtBx.Text) < 1 Then Warcry_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Surround_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Surround_TxtBx.Leave
        If Int(Surround_TxtBx.Text) > 250 Then Surround_TxtBx.Text = 250
        If Int(Surround_TxtBx.Text) < 1 Then Surround_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Speed_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Speed_TxtBx.Leave
        If Int(Speed_TxtBx.Text) > 250 Then Speed_TxtBx.Text = 250
        If Int(Speed_TxtBx.Text) < 1 Then Speed_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Immunity_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Immunity_TxtBx.Leave
        If Int(Immunity_TxtBx.Text) > 250 Then Immunity_TxtBx.Text = 250
        If Int(Immunity_TxtBx.Text) < 1 Then Immunity_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Barter_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Barter_TxtBx.Leave
        If Int(Barter_TxtBx.Text) > 250 Then Barter_TxtBx.Text = 250
        If Int(Barter_TxtBx.Text) < 1 Then Barter_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Perc_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Perc_TxtBx.Leave
        If Int(Perc_TxtBx.Text) > 250 Then Perc_TxtBx.Text = 250
        If Int(Perc_TxtBx.Text) < 1 Then Perc_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Stealth_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Stealth_TxtBx.Leave
        If Int(Stealth_TxtBx.Text) > 250 Then Stealth_TxtBx.Text = 250
        If Int(Stealth_TxtBx.Text) < 1 Then Stealth_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Regen_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Regen_TxtBx.Leave
        If Int(Regen_TxtBx.Text) > 250 Then Regen_TxtBx.Text = 250
        If Int(Regen_TxtBx.Text) < 1 Then Regen_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub AS_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AS_TxtBx.Leave
        If Int(AS_TxtBx.Text) > 250 Then AS_TxtBx.Text = 250
        If Int(AS_TxtBx.Text) < 1 Then AS_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Body_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Body_TxtBx.Leave
        If Int(Body_TxtBx.Text) > 250 Then Body_TxtBx.Text = 250
        If Int(Body_TxtBx.Text) < 1 Then Body_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub Prof_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Prof_TxtBx.Leave
        If Int(Prof_TxtBx.Text) > 100 Then Prof_TxtBx.Text = 100
        If Int(Prof_TxtBx.Text) < 1 Then Prof_TxtBx.Text = 1

        SetExpValues()
    End Sub

    Private Sub HP_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HP_Mod_TxtBx.Leave
        If Int(HP_Mod_TxtBx.Text) > Math.Floor(Int(HP_TxtBx.Text) / 2) Then HP_Mod_TxtBx.ForeColor = Color.Red Else HP_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub End_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles End_Mod_TxtBx.Leave
        If Int(End_Mod_TxtBx.Text) > Math.Floor(Int(End_TxtBx.Text) / 2) Then End_Mod_TxtBx.ForeColor = Color.Red Else End_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Wis_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Wis_Mod_TxtBx.Leave
        If Int(Wis_Mod_TxtBx.Text) > Math.Floor(Int(Wis_TxtBx.Text) / 2) Then Wis_Mod_TxtBx.ForeColor = Color.Red Else Wis_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Int_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Int_Mod_TxtBx.Leave
        If Int(Int_Mod_TxtBx.Text) > Math.Floor(Int(Int_TxtBx.Text) / 2) Then Int_Mod_TxtBx.ForeColor = Color.Red Else Int_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Agi_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Agi_Mod_TxtBx.Leave
        If Int(Agi_Mod_TxtBx.Text) > Math.Floor(Int(Agi_TxtBx.Text) / 2) Then Agi_Mod_TxtBx.ForeColor = Color.Red Else Agi_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Str_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Str_Mod_TxtBx.Leave
        If Int(Str_Mod_TxtBx.Text) > Math.Floor(Int(Str_TxtBx.Text) / 2) Then Str_Mod_TxtBx.ForeColor = Color.Red Else Str_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Dagger_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Dagger_Mod_TxtBx.Leave
        If Int(Dagger_Mod_TxtBx.Text) > Math.Floor(Int(Dagger_TxtBx.Text) / 2) Then Dagger_Mod_TxtBx.ForeColor = Color.Red Else Dagger_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub H2H_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles H2H_Mod_TxtBx.Leave
        If Int(H2H_Mod_TxtBx.Text) > Math.Floor(Int(H2H_TxtBx.Text) / 2) Then H2H_Mod_TxtBx.ForeColor = Color.Red Else H2H_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Sword_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Sword_Mod_TxtBx.Leave
        If Int(Sword_Mod_TxtBx.Text) > Math.Floor(Int(Sword_TxtBx.Text) / 2) Then Sword_Mod_TxtBx.ForeColor = Color.Red Else Sword_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub TwoHand_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TwoHand_Mod_TxtBx.Leave
        If Int(TwoHand_Mod_TxtBx.Text) > Math.Floor(Int(TwoHand_TxtBx.Text) / 2) Then TwoHand_Mod_TxtBx.ForeColor = Color.Red Else TwoHand_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Rage_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Rage_Mod_TxtBx.Leave
        If Int(Rage_Mod_TxtBx.Text) > Math.Floor(Int(Rage_TxtBx.Text) / 2) Then Rage_Mod_TxtBx.ForeColor = Color.Red Else Rage_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Attack_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Attack_Mod_TxtBx.Leave
        If Int(Attack_Mod_TxtBx.Text) > Math.Floor(Int(Attack_TxtBx.Text) / 2) Then Attack_Mod_TxtBx.ForeColor = Color.Red Else Attack_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Parry_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Parry_Mod_TxtBx.Leave
        If Int(Parry_Mod_TxtBx.Text) > Math.Floor(Int(Parry_TxtBx.Text) / 2) Then Parry_Mod_TxtBx.ForeColor = Color.Red Else Parry_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Tactics_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Tactics_Mod_TxtBx.Leave
        If Int(Tactics_Mod_TxtBx.Text) > Math.Floor(Int(Tactics_TxtBx.Text) / 2) Then Tactics_Mod_TxtBx.ForeColor = Color.Red Else Tactics_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Warcry_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Warcry_Mod_TxtBx.Leave
        If Int(Warcry_Mod_TxtBx.Text) > Math.Floor(Int(Warcry_TxtBx.Text) / 2) Then Warcry_Mod_TxtBx.ForeColor = Color.Red Else Warcry_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Surround_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Surround_Mod_TxtBx.Leave
        If Int(Surround_Mod_TxtBx.Text) > Math.Floor(Int(Surround_TxtBx.Text) / 2) Then Surround_Mod_TxtBx.ForeColor = Color.Red Else Surround_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Speed_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Speed_Mod_TxtBx.Leave
        If Int(Speed_Mod_TxtBx.Text) > Math.Floor(Int(Speed_TxtBx.Text) / 2) Then Speed_Mod_TxtBx.ForeColor = Color.Red Else Speed_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Immunity_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Immunity_Mod_TxtBx.Leave
        If Int(Immunity_Mod_TxtBx.Text) > Math.Floor(Int(Immunity_TxtBx.Text) / 2) Then Immunity_Mod_TxtBx.ForeColor = Color.Red Else Immunity_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Barter_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Barter_Mod_TxtBx.Leave
        If Int(Barter_Mod_TxtBx.Text) > Math.Floor(Int(Barter_TxtBx.Text) / 2) Then Barter_Mod_TxtBx.ForeColor = Color.Red Else Barter_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Perc_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Perc_Mod_TxtBx.Leave
        If Int(Perc_Mod_TxtBx.Text) > Math.Floor(Int(Perc_TxtBx.Text) / 2) Then Perc_Mod_TxtBx.ForeColor = Color.Red Else Perc_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Stealth_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Stealth_Mod_TxtBx.Leave
        If Int(Stealth_Mod_TxtBx.Text) > Math.Floor(Int(Stealth_TxtBx.Text) / 2) Then Stealth_Mod_TxtBx.ForeColor = Color.Red Else Stealth_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Regen_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Regen_Mod_TxtBx.Leave
        If Int(Regen_Mod_TxtBx.Text) > Math.Floor(Int(Regen_TxtBx.Text) / 2) Then Regen_Mod_TxtBx.ForeColor = Color.Red Else Regen_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub Body_Mod_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Body_Mod_TxtBx.Leave
        If Int(Body_Mod_TxtBx.Text) > Math.Floor(Int(Body_TxtBx.Text) / 2) Then Body_Mod_TxtBx.ForeColor = Color.Red Else Body_Mod_TxtBx.ForeColor = Color.Black

        SetExpValues()
    End Sub

    Private Sub LDW_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles LDW_TxtBx.Leave
        If Int(LDW_TxtBx.Text) > 30 Then LDW_TxtBx.Text = 30
        If Int(LDW_TxtBx.Text) < 0 Then LDW_TxtBx.Text = 0

        SetExpValues()
    End Sub

    Private Sub CW_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CW_TxtBx.Leave
        If Int(CW_TxtBx.Text) > 30 Then CW_TxtBx.Text = 30
        If Int(CW_TxtBx.Text) < 0 Then CW_TxtBx.Text = 0

        SetExpValues()
    End Sub

    Private Sub Ath_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Ath_TxtBx.Leave
        If Int(Ath_TxtBx.Text) > 30 Then Ath_TxtBx.Text = 30
        If Int(Ath_TxtBx.Text) < 0 Then Ath_TxtBx.Text = 0

        SetExpValues()
    End Sub

    Private Sub Thief_TxtBx_Leave(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Thief_TxtBx.Leave
        If Int(Thief_TxtBx.Text) > 30 Then Thief_TxtBx.Text = 30
        If Int(Thief_TxtBx.Text) < 0 Then Thief_TxtBx.Text = 0

        SetExpValues()
    End Sub

    Private Sub Hardcore_Chk_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Hardcore_Chk.CheckedChanged
        If Hardcore_Chk.Checked = False And Int(HP_TxtBx.Text) > 115 Then HP_TxtBx.Text = 115
        If Hardcore_Chk.Checked = False And Int(End_TxtBx.Text) > 115 Then End_TxtBx.Text = 115
        SetExpValues()
    End Sub

    Private Sub Max_Bless_Chk_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Max_Bless_Chk.CheckedChanged

        SetExpValues()
    End Sub

    Private Sub ShowRage_Chk_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ShowRage_Chk.CheckedChanged

        SetExpValues()
    End Sub

    Private Sub ShowTact_Chk_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ShowTact_Chk.CheckedChanged

        SetExpValues()
    End Sub

    Private Sub Arch_Chk_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Arch_Chk.CheckedChanged

        SetExpValues()
    End Sub

    Private Sub Save_Btn_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Save_Btn.Click
        Dim sFD As New SaveFileDialog

        sFD.Filter = "Warrior File|*.war"
        sFD.FileName = FileName
        sFD.ShowDialog()

        If sFD.FileName = "" Then MsgBox("File not saved, Invalid file name.") : Exit Sub

        FileName = System.IO.Path.GetFileName(sFD.FileName)

        Using sw As StreamWriter = File.CreateText(sFD.FileName)
            sw.WriteLine(HP_TxtBx.Text)
            sw.WriteLine(End_TxtBx.Text)
            sw.WriteLine(Wis_TxtBx.Text)
            sw.WriteLine(Int_TxtBx.Text)
            sw.WriteLine(Agi_TxtBx.Text)
            sw.WriteLine(Str_TxtBx.Text)
            sw.WriteLine(Dagger_TxtBx.Text)
            sw.WriteLine(H2H_TxtBx.Text)
            sw.WriteLine(Sword_TxtBx.Text)
            sw.WriteLine(TwoHand_TxtBx.Text)
            sw.WriteLine(Rage_TxtBx.Text)
            sw.WriteLine(AS_TxtBx.Text)
            sw.WriteLine(Attack_TxtBx.Text)
            sw.WriteLine(Parry_TxtBx.Text)
            sw.WriteLine(Tactics_TxtBx.Text)
            sw.WriteLine(Warcry_TxtBx.Text)
            sw.WriteLine(Surround_TxtBx.Text)
            sw.WriteLine(Body_TxtBx.Text)
            sw.WriteLine(Speed_TxtBx.Text)
            sw.WriteLine(Barter_TxtBx.Text)
            sw.WriteLine(Perc_TxtBx.Text)
            sw.WriteLine(Stealth_TxtBx.Text)
            sw.WriteLine(Regen_TxtBx.Text)
            sw.WriteLine(Immunity_TxtBx.Text)
            sw.WriteLine(Prof_TxtBx.Text)
            sw.WriteLine(LDW_TxtBx.Text)
            sw.WriteLine(CW_TxtBx.Text)
            sw.WriteLine(Ath_TxtBx.Text)
            sw.WriteLine(Thief_TxtBx.Text)
            sw.WriteLine(HP_Mod_TxtBx.Text)
            sw.WriteLine(End_Mod_TxtBx.Text)
            sw.WriteLine(Wis_Mod_TxtBx.Text)
            sw.WriteLine(Int_Mod_TxtBx.Text)
            sw.WriteLine(Agi_Mod_TxtBx.Text)
            sw.WriteLine(Str_Mod_TxtBx.Text)
            sw.WriteLine(Dagger_Mod_TxtBx.Text)
            sw.WriteLine(H2H_Mod_TxtBx.Text)
            sw.WriteLine(Sword_Mod_TxtBx.Text)
            sw.WriteLine(TwoHand_Mod_TxtBx.Text)
            sw.WriteLine(Rage_Mod_TxtBx.Text)
            sw.WriteLine(Attack_Mod_TxtBx.Text)
            sw.WriteLine(Parry_Mod_TxtBx.Text)
            sw.WriteLine(Tactics_Mod_TxtBx.Text)
            sw.WriteLine(Warcry_Mod_TxtBx.Text)
            sw.WriteLine(Surround_Mod_TxtBx.Text)
            sw.WriteLine(Body_Mod_TxtBx.Text)
            sw.WriteLine(Speed_Mod_TxtBx.Text)
            sw.WriteLine(Barter_Mod_TxtBx.Text)
            sw.WriteLine(Perc_Mod_TxtBx.Text)
            sw.WriteLine(Stealth_Mod_TxtBx.Text)
            sw.WriteLine(Regen_Mod_TxtBx.Text)
            sw.WriteLine(Immunity_Mod_TxtBx.Text)
            sw.WriteLine(ShowTact_Chk.Checked)
            sw.WriteLine(ShowRage_Chk.Checked)
            sw.WriteLine(Hardcore_Chk.Checked)
            sw.WriteLine(Max_Bless_Chk.Checked)
            sw.WriteLine(Arch_Chk.Checked)
            sw.Close()
        End Using

    End Sub

    Private Sub Load_Btn_Click(ByVal sender As System.Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Load_Btn.MouseUp
        Dim oFD As New OpenFileDialog

        Select Case e.Button
            Case Windows.Forms.MouseButtons.Left

                oFD.Filter = "Warrior File|*.war"
                oFD.ShowDialog()

                If oFD.FileName = "" Then MsgBox("File not Loaded, Invalid File Name") : Exit Sub

                FileName = System.IO.Path.GetFileName(oFD.FileName)

                Using sr As StreamReader = File.OpenText(oFD.FileName)
                    HP_TxtBx.Text = sr.ReadLine()
                    End_TxtBx.Text = sr.ReadLine()
                    Wis_TxtBx.Text = sr.ReadLine()
                    Int_TxtBx.Text = sr.ReadLine()
                    Agi_TxtBx.Text = sr.ReadLine()
                    Str_TxtBx.Text = sr.ReadLine()
                    Dagger_TxtBx.Text = sr.ReadLine()
                    H2H_TxtBx.Text = sr.ReadLine()
                    Sword_TxtBx.Text = sr.ReadLine()
                    TwoHand_TxtBx.Text = sr.ReadLine()
                    Rage_TxtBx.Text = sr.ReadLine()
                    AS_TxtBx.Text = sr.ReadLine()
                    Attack_TxtBx.Text = sr.ReadLine()
                    Parry_TxtBx.Text = sr.ReadLine()
                    Tactics_TxtBx.Text = sr.ReadLine()
                    Warcry_TxtBx.Text = sr.ReadLine()
                    Surround_TxtBx.Text = sr.ReadLine()
                    Body_TxtBx.Text = sr.ReadLine()
                    Speed_TxtBx.Text = sr.ReadLine()
                    Immunity_TxtBx.Text = sr.ReadLine()
                    Barter_TxtBx.Text = sr.ReadLine()
                    Perc_TxtBx.Text = sr.ReadLine()
                    Stealth_TxtBx.Text = sr.ReadLine()
                    Regen_TxtBx.Text = sr.ReadLine()
                    Prof_TxtBx.Text = sr.ReadLine()
                    LDW_TxtBx.Text = sr.ReadLine()
                    CW_TxtBx.Text = sr.ReadLine()
                    Ath_TxtBx.Text = sr.ReadLine()
                    Thief_TxtBx.Text = sr.ReadLine()
                    HP_Mod_TxtBx.Text = sr.ReadLine()
                    End_Mod_TxtBx.Text = sr.ReadLine()
                    Wis_Mod_TxtBx.Text = sr.ReadLine()
                    Int_Mod_TxtBx.Text = sr.ReadLine()
                    Agi_Mod_TxtBx.Text = sr.ReadLine()
                    Str_Mod_TxtBx.Text = sr.ReadLine()
                    Dagger_Mod_TxtBx.Text = sr.ReadLine()
                    H2H_Mod_TxtBx.Text = sr.ReadLine()
                    Sword_Mod_TxtBx.Text = sr.ReadLine()
                    TwoHand_Mod_TxtBx.Text = sr.ReadLine()
                    Rage_Mod_TxtBx.Text = sr.ReadLine()
                    Attack_Mod_TxtBx.Text = sr.ReadLine()
                    Parry_Mod_TxtBx.Text = sr.ReadLine()
                    Tactics_Mod_TxtBx.Text = sr.ReadLine()
                    Warcry_Mod_TxtBx.Text = sr.ReadLine()
                    Surround_Mod_TxtBx.Text = sr.ReadLine()
                    Body_Mod_TxtBx.Text = sr.ReadLine()
                    Speed_Mod_TxtBx.Text = sr.ReadLine()
                    Barter_Mod_TxtBx.Text = sr.ReadLine()
                    Perc_Mod_TxtBx.Text = sr.ReadLine()
                    Stealth_Mod_TxtBx.Text = sr.ReadLine()
                    Regen_Mod_TxtBx.Text = sr.ReadLine()
                    Immunity_Mod_TxtBx.Text = sr.ReadLine()
                    ShowTact_Chk.Checked = sr.ReadLine()
                    ShowRage_Chk.Checked = sr.ReadLine()
                    Hardcore_Chk.Checked = sr.ReadLine()
                    Max_Bless_Chk.Checked = sr.ReadLine()
                    Arch_Chk.Checked = sr.ReadLine()
                    sr.Close()
                End Using
            Case Windows.Forms.MouseButtons.Right
                oFD.Filter = "Widget Stat Calc file|*"
                oFD.ShowDialog()

                If oFD.FileName = "" Then MsgBox("File not Loaded, Invalid File Name") : Exit Sub

                FileName = System.IO.Path.GetFileName(oFD.FileName)

                Using sr As StreamReader = File.OpenText(oFD.FileName)
                    sr.ReadLine()
                    Hardcore_Chk.Checked = sr.ReadLine()
                    Max_Bless_Chk.Checked = sr.ReadLine()
                    Ath_TxtBx.Text = Int(sr.ReadLine())
                    HP_TxtBx.Text = sr.ReadLine()
                    HP_Mod_TxtBx.Text = sr.ReadLine()
                    End_TxtBx.Text = sr.ReadLine()
                    End_Mod_TxtBx.Text = sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    Wis_TxtBx.Text = sr.ReadLine()
                    Wis_Mod_TxtBx.Text = sr.ReadLine()
                    Int_TxtBx.Text = sr.ReadLine()
                    Int_Mod_TxtBx.Text = sr.ReadLine()
                    Agi_TxtBx.Text = sr.ReadLine()
                    Agi_Mod_TxtBx.Text = sr.ReadLine()
                    Str_TxtBx.Text = sr.ReadLine()
                    Str_Mod_TxtBx.Text = sr.ReadLine()
                    Dagger_TxtBx.Text = sr.ReadLine()
                    Dagger_Mod_TxtBx.Text = sr.ReadLine()
                    H2H_TxtBx.Text = sr.ReadLine()
                    H2H_Mod_TxtBx.Text = sr.ReadLine()
                    Sword_TxtBx.Text = sr.ReadLine()
                    Sword_Mod_TxtBx.Text = sr.ReadLine()
                    TwoHand_TxtBx.Text = sr.ReadLine()
                    TwoHand_Mod_TxtBx.Text = sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    Rage_TxtBx.Text = sr.ReadLine()
                    Rage_Mod_TxtBx.Text = sr.ReadLine()
                    AS_TxtBx.Text = sr.ReadLine()
                    sr.ReadLine()
                    Attack_TxtBx.Text = sr.ReadLine()
                    Attack_Mod_TxtBx.Text = sr.ReadLine()
                    Parry_TxtBx.Text = sr.ReadLine()
                    Parry_Mod_TxtBx.Text = sr.ReadLine()
                    Warcry_TxtBx.Text = sr.ReadLine()
                    Warcry_Mod_TxtBx.Text = sr.ReadLine()
                    Tactics_TxtBx.Text = sr.ReadLine()
                    Tactics_Mod_TxtBx.Text = sr.ReadLine()
                    Surround_TxtBx.Text = sr.ReadLine()
                    Surround_Mod_TxtBx.Text = sr.ReadLine()
                    Body_TxtBx.Text = sr.ReadLine()
                    Body_Mod_TxtBx.Text = sr.ReadLine()
                    Speed_TxtBx.Text = sr.ReadLine()
                    Speed_Mod_TxtBx.Text = sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    Barter_TxtBx.Text = sr.ReadLine()
                    Barter_Mod_TxtBx.Text = sr.ReadLine()
                    Perc_TxtBx.Text = sr.ReadLine()
                    Perc_Mod_TxtBx.Text = sr.ReadLine()
                    Stealth_TxtBx.Text = sr.ReadLine()
                    Stealth_Mod_TxtBx.Text = sr.ReadLine()
                    Regen_TxtBx.Text = sr.ReadLine()
                    Regen_Mod_TxtBx.Text = sr.ReadLine()
                    sr.ReadLine()
                    sr.ReadLine()
                    Immunity_TxtBx.Text = sr.ReadLine()
                    Immunity_Mod_TxtBx.Text = sr.ReadLine()
                    Prof_TxtBx.Text = sr.ReadLine()
                End Using
        End Select

        SetExpValues()

    End Sub


    Private Sub frmWarriorCalc_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        SetExpValues()
    End Sub
End Class
