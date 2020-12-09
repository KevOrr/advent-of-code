;;; A1 = ! = pop fs
;;; A9 = © = pop gs
;;; pop {e,c,s,d}s are in the C0,C1 categories
;;; jump has to be >= 43
    bits 16
    org 7600h

%macro jump 1
    push '{}'
    pop ax
    cmp ax, '{}'
    je short %1
%endmacro

;; %subs dest, src, start (0-indexed), end (0-indexed, exclusive)
%macro subs 4
    %substr %1 %2 (%3 + 1),(%4 - %3)
%endmacro

;; %subs dest, src, start (0-indexed)
%macro subs 3
    %strlen %%len %2
    subs %1, %2, %3, %%len
%endmacro

%macro putsub 3
    subs %%s, %1, %2, %3
    db %%s
%endmacro

%macro putsub 2
    subs %%s, %1, %2
    db %%s
%endmacro

;;; Indented so the columns are 20+x
%define line01      `123456789\n`
%define line02      `                                   @@\n`
%define line03      `                                  @@@@\n`
%define line04      `                                 @*@,@@\n`
%define line05      `                               @@@,#,@@@\n`
%define line06      `                     .@@@@@@@@@@@,,,,,@@@@@@@@@@,\n`
%define line07      `                      '@@@@@,,,,,,,,,,,,@@@@@#\n`
%define line08      `                           ,@@@,,,(,,,,,@@@\n`
%define line09      `                             @,,%#,,,,@,,@@\n`
%define line10      `                            @*,@,@@@@@@,@@,@\n`
%define line11      `                           @@@@@@@,,,@. @@@@@@\n`
%define line12      `                           @@  @@,,@*@@\n`
%define line13      `                              .@@,,@@,@@\n`
%define line14      `                              @@//(////@@\n`
%define line15      `                             @@////////(@@@@\n`
%define line16      `                        @###&@///@@,,@@/@###@\n`
%define line17      `                        @@##@@@@@@,,,,@@@#@@&\n`
%define line18      `                      @@(&. //@  @*@@@***  @*/@ @@\n`
%define line19      `                       @@@@@@@@@@@@@@@@@#@@@@@@@@\n`
%define line20      `                       (@@/////@////////@@////@@@@\n`
%define line21      `                      @@///@@@@@@/////@@@@@@@@@@%@@@\n`
%define line22      `                     @@@@@@@////@@(/@@@(/%@@///@@@@@@@\n`
%define line23      `                   @@@ @@@///////@@@@@///@&@@///@@@\n`
%define line24      `                    ,@@@@/@####@@/@@/////&@#///@,,,,@\n`
%define line25      `                   @,,,@@/@####@@//////@%,,,,@//@@@@@\n`
%define line26      `                  @@@@@@&//(@@@//@@@@@@@@,,,*@@@@@@@@@@@@@@.\n`
%define line27      `             .@@@@  @/*  @@@@@@@/ @//( ,&@@@@/@  @/@  @//@@@\n`
%define line28      `             @@@@@@@(@@@@//@  *//@@@//@@@@ ,@@@////@@@@@@@@\n`
%define line29      `              @@@/////////@@@@@@////@@(/////////@@////////@@@\n`
%define line30      `            @@@////&@@@@@////////&@@@@@@@//////#@@@@@@@@@@@@@@@@\n`
%define line31      `          @@@@@@@@@@///@@@@///(@@@@/////@@@@///@@&////(@@@@@@@@@@@\n`
%define line32      `               @@@@//(&//@@@@@@@//////////&@@@@@//(@////@@@&@@\n`
%define line33      `           @@@@@@//@&###@//@@////@@,,,@@/////@@//@,,,@/@@####@@\n`
%define line34      `         @@,,,,,@@#/@@@@////////#@,,,,,@@////@@///(@////@@@@@@@\n`
%define line35      `         @@,,,,,@@@@@///@@#%@/////@@@@@@@@@@####@&@@@@@@@@@@@@@@@@@@@\n`
%define line36      `      @@@@@@@@@@(/@  @@@%####@@@@/ //%. @//@@##@@ @*%@@@*/@  //@   @@\n`
%define line37      `      @@. @//@@@@@@@@@* @@@@@ &*/@@@@///@@@@ @//@@@///////@@@@@@@@@@@\n`
%define line38      `      @@@/////////////(@@@@@@@@@%////@@///////////////@////////////@@@@\n`
%define line39      `    @@@////////(@@@@//////////////@@@@@@@@///////////@@@@@@//////////(@@@&\n`
%define line40      `  @@@/////@@@@@@@@@@@@/////////@@@@@.   @@@@@///////@@@@@@@@@@@@@@@@@@@@@@@@\n`
%define line41      `@@@@@@@@@@@        @@@@@////%@@@@@        @@@@@&///@@@@           ,#%&&&&#/.\n`
%define line42      `                     @@@@@@@@@@              @@@@@#@@@\n`
%define line43      `                       @@@@@                   @@@@@/\n`
%define line44      `                        &                         @\n`

;; %strlen len01 line01
;; %strlen len02 line02
;; %strlen len03 line03
;; %strlen len04 line04
;; %strlen len05 line05
;; %strlen len06 line06
;; %strlen len07 line07
;; %strlen len08 line08
;; %strlen len09 line09
;; %strlen len10 line10
;; %strlen len11 line11
;; %strlen len12 line12
;; %strlen len13 line13
;; %strlen len14 line14
;; %strlen len15 line15
;; %strlen len16 line16
;; %strlen len17 line17
;; %strlen len18 line18
;; %strlen len19 line19
;; %strlen len20 line20
;; %strlen len21 line21
;; %strlen len22 line22
;; %strlen len23 line23
;; %strlen len24 line24
;; %strlen len25 line25
;; %strlen len26 line26
;; %strlen len27 line27
;; %strlen len28 line28
;; %strlen len29 line29
;; %strlen len30 line30
;; %strlen len31 line31
;; %strlen len32 line32
;; %strlen len33 line33
;; %strlen len34 line34
;; %strlen len35 line35
;; %strlen len36 line36
;; %strlen len37 line37
;; %strlen len38 line38
;; %strlen len39 line39
;; %strlen len40 line40
;; %strlen len41 line41
;; %strlen len42 line42
;; %strlen len43 line43
;; %strlen len44 line44

;;; Tip of the tree doesn't have enough space for instructions, so just jump to line 6
l01:
    jump l04_target             ; TODO can this be shorter?
    putsub line01, ($-l01)

    db line02
    db line03

l04:
    putsub line04, 0, 35
l04_target:
    je l06_target
    putsub line04, ($-l04)

    db line05

;;; Set ds to 0x7e00
l06:
    putsub line06, 0, 22
l06_target:
    push ']a'
    pop ax
    xor ax, '3a'                ; '5d61' ^ '3361' = '7e00'
    cmp ax, `e\xc3`             ; utf-8 alignment nop
    mov ds, ax
    mov di, [0x6c6c]            ; utf-8 alignment nop
    jump l07_target

    putsub line06, ($-l06)

l07:
    putsub line07, 0, 30
l07_target:
