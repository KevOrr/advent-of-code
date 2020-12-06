;;; A1 = ! = pop fs
;;; A9 = © = pop gs
;;; pop {e,c,s,d}s are in the C0,C1 categories
;;; jump has to be >= 43
    bits 16
    org 7600h

%macro jump 1
    push 'ac'
    pop ax
    cmp ax, 'ac'
    je short %1
%endmacro

%define line01 `                \n`
%define line02 `                                   @@\n`
%define line03 `                                  @@@@\n`
%define line04 `                                 @*@,@\n`
%define line05 `                                @@,#,@@\n`
%define line06 `                     .@@@@@@@@@@@,,,,,@@@@@@@@@@,\n`
%define line07 `                      '@@@@@,,,,,,,,,,,,@@@@@#\n`
%define line08 `                           ,@@@,,,(,,,,,@@@\n`
%define line09 `                             @,,%#,,,,@,,@@\n`
%define line10 `                            @*,@,@@@@@@,@@,@\n`
%define line11 `                           @@@@@@@,,,@. @@@@@@\n`
%define line12 `                           @@  @@,,@*@@\n`
%define line13 `                              .@@,,@@,@@\n`
%define line14 `                              @@//(////@@\n`
%define line15 `                             @@////////(@@@@\n`
%define line16 `                        @###&@///@@,,@@/@###@\n`
%define line17 `                        @@##@@@@@@,,,,@@@#@@&\n`
%define line18 `                      @@(&. //@  @*@@@***  @*/@ @@\n`
%define line19 `                       @@@@@@@@@@@@@@@@@#@@@@@@@@\n`
%define line20 `                       (@@/////@////////@@////@@@@\n`
%define line21 `                      @@///@@@@@@/////@@@@@@@@@@%@@@\n`
%define line22 `                     @@@@@@@////@@(/@@@(/%@@///@@@@@@@\n`
%define line23 `                   @@@ @@@///////@@@@@///@&@@///@@@\n`
%define line24 `                    ,@@@@/@####@@/@@/////&@#///@,,,,@\n`
%define line25 `                   @,,,@@/@####@@//////@%,,,,@//@@@@@\n`
%define line26 `                  @@@@@@&//(@@@//@@@@@@@@,,,*@@@@@@@@@@@@@@.\n`
%define line27 `             .@@@@  @/*  @@@@@@@/ @//( ,&@@@@/@  @/@  @//@@@\n`
%define line28 `             @@@@@@@(@@@@//@  *//@@@//@@@@ ,@@@////@@@@@@@@\n`
%define line29 `              @@@/////////@@@@@@////@@(/////////@@////////@@@\n`
%define line30 `            @@@////&@@@@@////////&@@@@@@@//////#@@@@@@@@@@@@@@@@\n`
%define line31 `          @@@@@@@@@@///@@@@///(@@@@/////@@@@///@@&////(@@@@@@@@@@@\n`
%define line32 `               @@@@//(&//@@@@@@@//////////&@@@@@//(@////@@@&@@\n`
%define line33 `           @@@@@@//@&###@//@@////@@,,,@@/////@@//@,,,@/@@####@@\n`
%define line34 `         @@,,,,,@@#/@@@@////////#@,,,,,@@////@@///(@////@@@@@@@\n`
%define line35 `         @@,,,,,@@@@@///@@#%@/////@@@@@@@@@@####@&@@@@@@@@@@@@@@@@@@@\n`
%define line36 `      @@@@@@@@@@(/@  @@@%####@@@@/ //%. @//@@##@@ @*%@@@*/@  //@   @@\n`
%define line37 `      @@. @//@@@@@@@@@* @@@@@ &*/@@@@///@@@@ @//@@@///////@@@@@@@@@@@\n`
%define line38 `      @@@/////////////(@@@@@@@@@%////@@///////////////@////////////@@@@\n`
%define line39 `    @@@////////(@@@@//////////////@@@@@@@@///////////@@@@@@//////////(@@@&\n`
%define line40 `  @@@/////@@@@@@@@@@@@/////////@@@@@.   @@@@@///////@@@@@@@@@@@@@@@@@@@@@@@@\n`
%define line41 `@@@@@@@@@@@        @@@@@////%@@@@@        @@@@@&///@@@@           ,#%&&&&#/.\n`
%define line42 `                     @@@@@@@@@@              @@@@@#@@@\n`
%define line43 `                       @@@@@                   @@@@@/\n`
%define line44 `                        &                         @\n`

%strlen len01 line01
%strlen len02 line02
%strlen len03 line03
%strlen len04 line04
%strlen len05 line05
%strlen len06 line06
%strlen len07 line07
%strlen len08 line08
%strlen len09 line09
%strlen len10 line10
%strlen len11 line11
%strlen len12 line12
%strlen len13 line13
%strlen len14 line14
%strlen len15 line15
%strlen len16 line16
%strlen len17 line17
%strlen len18 line18
%strlen len19 line19
%strlen len20 line20
%strlen len21 line21
%strlen len22 line22
%strlen len23 line23
%strlen len24 line24
%strlen len25 line25
%strlen len26 line26
%strlen len27 line27
%strlen len28 line28
%strlen len29 line29
%strlen len30 line30
%strlen len31 line31
%strlen len32 line32
%strlen len33 line33
%strlen len34 line34
%strlen len35 line35
%strlen len36 line36
%strlen len37 line37
%strlen len38 line38
%strlen len39 line39
%strlen len40 line40
%strlen len41 line41
%strlen len42 line42
%strlen len43 line43
%strlen len44 line44

l01:
.i1:    push byte 0x30
        jump l03
        %substr .s1 line01 ($ - .i1 + 1),(len01 - ($ - .i1))
        db .s1

l02:    db line02
l03:    db line03
l04:    je
