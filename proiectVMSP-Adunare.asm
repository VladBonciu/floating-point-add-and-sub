.model small
.stack 200h
.data
    a dd ?
    b dd ?
    mesInceput db "Introduceti numerele pentru efectuarea 'a+b' (valori intre [-32,768, 32,767]) :", 10, 13, '$'
    mesA db "a = $"
    mesB db "b = $"
    mesFinal db "a+b = $"
.code

    ;se stocheaza in ax numarul citit in CC 
    ;poate fi utilizat caracterul '-' pentru introducerea unei valori negative
    ;observatie: CC pe 16 biti poate acoperi valori intre [-32,768, 32,767]
    ;input: de la tastatura
    citireNumarCC PROC
        ;initializam dx cu 10 ca sa facem inmlutirea
        mov dx, 10

        ;golim bx
        xor bx, bx

        ;punem pe stiva val initiala a numarului = 0
        push 0

        ;verificam semnul

            ;citim caracterul
            mov ah, 01h
            int 21h

            ;daca e enter nu mai citeste numar
            cmp al, 13
            je finalNumar

            ;daca caracterul e - schimbam bit ul de semn
            cmp al, 45
            je numarNegativ

            ;daca caracterul nu e -
            jmp numarPozitiv

        numarNegativ:
            mov cx, 1
            jmp citireCifra

        numarPozitiv:
            mov cx, 0
            jmp prelucrareCifra

        citireCifra:
            ;citim caracterul
            mov ah, 01h
            int 21h

            ;daca e enter nu mai citeste numar
            cmp al, 13
            je transformareCDinCC

            prelucrareCifra:

            ;transf numarul citit din ascii in baza 10
            sub al, 30h
            
            ;copiem nr citit in bl
            mov bl, al

            ;scoatem din ax nr initial
            pop ax

            ;inmultim nr initial cu 10
            mov dx, 10
            mul dx

            ;adaugam nr citit la nr precedent
            add ax, bx

            ;punem pe stiva nr nou
            push ax

            jmp citireCifra
        
        transformareCDinCC:
            ;daca numarul e pozitiv il afisam
            cmp cx, 0
            je finalNumar

            ;daca e negativ il modificam sa fie in CC

            ;scoatem nr de pe stiva
            pop ax

            ;inversam toti bitii din ax (tranf in CI)
            not ax

            ;adaugam 1 (transf in CC)
            inc ax

            ;punem ax pe stiva ca sa il scoata la final
            push ax

            jmp finalNumar

        finalNumar:
            pop ax
            ret

    citireNumarCC ENDP
    
    ;transforma nr din CC in Virgula Mobila Simpla Precizie
    ;input : ax - nr in CC
    ;output: cx:dx
    transformareCCInVMSP PROC
        
        ;punem pe stiva ax ca sa il utilizam mai tarziu
        push ax

        cmp ax, 0
        je cazSpecialZero
        jmp verificareCazSpecial2

        cazSpecialZero:
            pop ax
            mov cx, 0000h
            mov dx, 0000h
            ret

        verificareCazSpecial2:

        ;pentru caz special -32768
        cmp ax, 8000h
        je cazSpecialNrMinim
        jmp prelucrareNumar

        cazSpecialNrMinim:
            pop ax
            mov cx, 0c700h
            mov dx, 0000h
            ret

        prelucrareNumar:
        
        ;construim bit ul de semn

        ;in cx am pus 0
        xor cx, cx 

        ;shift o pozitie la stanga a nr din ax pt a izola bitul de semn in carry flag
        shl ax, 1
        
        ;add-with-carry - adaugam 0 in cx si bitul de semn
        adc cx, 0 

        cmp cx,0 
        je nrPoz
        jmp nrNeg

        nrPoz:
            ;ax ajunge la forma initiala
            shr ax, 1 
            
            ;in cx punem bitul de semn 0
            mov cx, 0000h 

            jmp finalSemn
        nrNeg:
            ;scoatem din stiva pt prelucrare
            pop ax  
            
            ;scadem 1 (aducem din CC in CI)
            dec ax

            ;negam bitii (ajungem in CD)
            not ax

            ;il punem pe stiva
            push ax 

            ;in cx punem bitul de semn 1
            mov cx, 8000h 

            ; nu avem nevoie de jmp finalSemn
            
        finalSemn:

            ;punem pe stiva prima juatate a nr in VMSP
            push cx

            ;copiem in bx nr in CD
            mov bx, ax

            ;intializam dx cu 0 (acesta va fi contorul de exponent
            xor dx,dx

        determinareExp:

            ;daca exponentul este aflat (am determinat cate val sunt dupa , )
            cmp bx, 1
            je expAflat
            
            ;crestem contorul de exp
            inc dx

            ;eliminam bit ul del mai putin semnificativ
            shr bx, 1

        jmp determinareExp

        expAflat:

            ;am calculat 16-(bitii se afla dupa virgula) 
            mov cx, 16
            sub cx, dx

            ;mutam bitii la stanga sa ca sa scapam de bitii care nu fac parte din mantisa
            shl ax, cl

            ;mutam bitii inapoi
            shr ax, cl

            ;copiem exponentul inainte sa adaugam 127 
            mov bx, dx
            
            ;adaugam la exponentul aflat 127 ca sa ajungem la standardul IEEE 754
            add dx, 127

            ;aliniem bitii cu pozitia pe care trebuie sa fie exponentul
            shl dx, 7

            ;scoatem jumatatea din VMSP
            pop cx

            ;punem exp in VMSP
            add cx, dx

            ;punem pe stiva jumatatea in VMSP cu exp si bit de semn
            push cx

            ;verificam cati biti contine exponentul
            cmp bx, 7
            jle expMaiMicSau7
            jg expMaiMareDe7

            expMaiMicSau7:
                ;mutam in cx numarul de pozitii pentru diferenta de biti
                mov cx, 7

                ;scadem exponentul pentru a vedea cate pozitii trebuie sa mutam bitii la stanga
                sub cx, bx

                aliniazaBitiiStanga:
                    ;nu mai avem biti de mutat => bitii au fost aliniati
                    cmp cx, 0
                        je bitiAliniatiStanga

                    ;scadem contorul de biti care trebuie aliniati
                    dec cx

                    ;mutam bitii la stanga cu o pozitie
                    shl ax, 1
                jmp aliniazaBitiiStanga
            
            bitiAliniatiStanga:

                ;scoatem din stiva jumatatea in VMSP cu exp si bit de semn
                pop cx

                ;adaugam bitii din mantisa
                add cx, ax 

                ;punem in dx 0 pt ca sunt toate val nule
                mov dx, 0000h

                ;scoatem nr in CD (ca sa avem stiva golita cand dam return)
                pop ax

                ret
            
            expMaiMareDe7:

                ;mutam in cx numarul de pozitii pentru diferenta de biti
                mov cx, bx

                ;scadem exponentul pentru a vedea cate pozitii trebuie sa mutam bitii la stanga
                sub cx, 7

                ;mutam bitii la dreapta cu cl pozitii
                shr ax, cl 

                ;scoatem juamtatea cu exp in VMSP
                pop dx

                ;adaugam primii 7 biti din mantisa
                add dx,ax

                ;scoatem in ax nr in CD
                pop ax

                ;punem prima jumatate din nr in VMSP pe stiva
                push dx

                ;golim bx pt a pune exp ramas
                xor bx, bx

                ;punem nr de biti ramasi in bl
                mov bl, cl
                
                mov cx, 16

                ;efectuam calculul 16 - (nr de biti ramasi)
                sub cx, bx

                ;mutam bitii care au mai ramas din mantisa la pozitiile corespunzatoare
                shl ax, cl

                ;a doua jumatate a nr in VMSP se afla in dx
                mov dx, ax

                ;punem in cx prima jumatate a nr in VMSP
                pop cx

                ret
    transformareCCInVMSP ENDP 

    ;input: ax:bx si cx:dx
    ;output: nr interschimbate
    interschimbareNrVMSP PROC
        push ax
        push bx

        mov ax, cx
        mov bx, dx

        pop dx
        pop cx

        ret
    interschimbareNrVMSP ENDP

    ; input: ax:bx si cx:dx (ax:bx - cx:dx)
    ; output: cx:dx valoarea calculata
    sunaVMSP PROC

        push bx
        push dx

        push ax
        push cx

        analizaExponentului:

        ;punem in variabile numerele pe care urmeaza sa efectuam calculul

        mov word ptr a, ax
        mov word ptr a + 2, bx

        mov word ptr b, cx
        mov word ptr b + 2, dx

        ;punem masca de biti pentru exponent (0111111110000000) 
        mov dx, 7F80h

        ;aplicam masca pe ambele jumatati de VMSP care contin exponentul
        and ax, dx
        and cx, dx

        ;aliniem bitii la ca sa corespunda cu valoarea reprezentata
        shr ax, 7
        shr cx, 7

        ;scoatem 127 pentru a evalua nr fara standardul IEEE 754
        sub ax, 127
        sub cx, 127

        cmp ax, cx
        jl mergiLaInterschimbare
        je exponentiEgali
        jg mergiLaEgalareExponenti


        exponentiEgali:

            ;cream masca pentru extragere mantisei din prima jumatate a numerelor (0000 0000 0111 1111)
            mov dx, 007Fh

            ;scoatem valorile de pe stiva si le punem la loc pentru a le prelucra mai tarziu
            pop ax
            pop cx

            ;aplicam masca cu operatia and
            and ax, dx
            and cx, dx

            ;punem 1-ul care se afla inainte de , (0000 0000 1000 0000)
            mov dx, 0080h

            ;adaugam 1 ul la fiecare jumatate de nr
            add ax, dx
            add cx, dx

            ;punem in bx, valoarea lui cx pt a putea folosi cl in mutarea bitiilor (*)
            mov bx, cx

            ;scoatem prima jumatate din un nr din VMSP
            mov dx, word ptr a

            ;izolan exponenetul prim mutarea bitiilor
            shl dx, 1
            shr dx, 8

            ;scadem 127 din standardul IEEE 754 ca sa ajungem la valoarea adevarata a exponentului
            sub dx, 127

            cmp dx, 7
            jle expEgalMaiMicSau7
            jg expEgalMaiMareDe7

            expEgalMaiMicSau7:

                mov cx, 7
                sub cx, dx

                shr ax, cl

                ;in bx avem val lui cx din nr in VMSP cx:dx
                shr bx, cl

                pop dx
                pop dx

                jmp calculareAdunare

            expEgalMaiMareDe7:
                
                ;punem inapoi valoarea lui cx (*)
                mov cx, bx

                ;scoatem a doua jumatate din primul numar 
                pop bx

                ;punem jum lui cx pe stiva ca sa o calculam mai tarziu
                push cx

                ;punem pe stiva exponentul sa il folosim si la construirea celui de al doilea numar
                push dx

                ;scoatem din cx primii 7 biti care sunt in prima jumatate din numaratoare
                sub dx, 7

                construiestePrimulNumar:

                    ;daca contorul de exp ramasi e 0 trecem la calcularea celuilalt numar
                    cmp dx, 0
                    je terminatConstruirePrimulNumar

                    ; mutam bitii din ax cu o poz la stanga
                    shl ax, 1

                    ;mutam din a doua jum la stanga (si e retinut bit ul de carry)
                    shl bx, 1

                    ;adaugam bit ul de carry in pozitia lui
                    adc ax, 0

                    dec dx

                    jmp construiestePrimulNumar

                terminatConstruirePrimulNumar:

                    ;scoate exponentul
                    pop bx

                    ;scoatem prima jum din al doilea nr
                    pop cx

                    ;scoatem a doua jum din al doilea nr
                    pop dx

                    ;scoatem din cx primii 7 biti care sunt in prima jumatate din numaratoare
                    sub bx, 7

                construiesteCelalaltNumar:
                
                    ;daca contorul de exp ramasi e 0 trecem la calcularea celuilalt numar
                    cmp bx, 0
                    je terminatConstruireNr

                    ; mutam bitii din ax cu o poz la stanga
                    shl cx, 1

                    ;mutam din a doua jum la stanga (si e retinut bit ul de carry)
                    shl dx, 1

                    ;adaugam bit ul de carry in pozitia lui
                    adc cx, 0

                    dec bx

                    jmp construiesteCelalaltNumar

                terminatConstruireNr:
                    
                    ;mutam in bx pentru efectuarea scaderii
                    mov bx, cx

                jmp calculareAdunare
        
        mergiLaInterschimbare:
            jmp interschimbareValori

        mergiLaEgalareExponenti:
            jmp egalareExponenti

        calculareAdunare:

            ;aici cx si dx o sa aiba primele jumatati din fiecare nr
            mov cx, word ptr a
            mov dx, word ptr b
        

            ;punem bitii de semn penru a efectua comparatiile
            shr cx, 15
            shr dx, 15

            cmp cx, 0
            je evaluareaCelalaltSemn
            jmp semnPrimulNrNeg

            evaluareaCelalaltSemn:

            cmp dx, 0
            je efectuareCalcul
            jmp semnCelalaltNrNeg

            semnPrimulNrNeg:
                ;negam valoarea lui ax (primul nr)
                neg ax

                jmp evaluareaCelalaltSemn

            semnCelalaltNrNeg:
                ;negam valoarea lui bx (celalalt nr)
                neg bx

                jmp efectuareCalcul

            efectuareCalcul:

            add ax, bx
            
            ; call transformareCCInVMSP

            ret 
        

        interschimbareValori:
            ;scoatem de pe sttiva valorile ca sa le punem inapoi dupa interschimbare
            pop cx
            pop ax

            pop dx
            pop bx

            call interschimbareNrVMSP

            ;punem la loc valorile
            push bx
            push dx

            push ax
            push cx

            jmp analizaExponentului

        egalareExponenti:
            cmp ax, 7
            jle expPrimuluiNrMaiMicSau7
            jg expPrimuluiNrMaiMareDe7

            expPrimuluiNrMaiMicSau7:

                ; calculam diferenta dintre cele doua valori
                sub ax, cx
                
                ; copiem diferenta in dx
                mov dx, ax

                ;scoatem de pe stiva valoarea lui cx
                pop cx

                ;scoatem de pe stiva valoarea lui ax
                pop ax

                ;punem diferenta dintre exponenti pe stiva
                push dx

                ;facem masca de biti pentru exponent (0000000001111111) 
                mov dx, 007Fh

                ;aplicam masca de biti
                and ax, dx
                and cx, dx

                ;punem diferenta de biti inapoi in dx
                pop dx

                ; mutam biti din mantisa cu o pozitie la dreapta
                shr cx, 1

                ;adaugam bit ul de 1
                add cx, 0040h

                ;scadem nr de biti care mai trebuie adaugati
                dec dx

                ajusteazaExponent:

                    cmp dx, 0
                    je terminaAjustarea

                    ;mutam biti de semn la stanga
                    shr cx, 1

                    ;scadem nr de biti care mai trebuie adaugati
                    dec dx

                jmp ajusteazaExponent

                terminaAjustarea:

                ;adaugam bit ul de 1 de la nr 
                add ax, 0080h

                ;scoatem prima jumatate din un nr din VMSP (acum au exponentii egalati
                mov dx, word ptr a

                ;izolan exponenetul prim mutarea bitiilor
                shl dx, 1
                shr dx, 8

                ;scadem 127 din standardul IEEE 754 ca sa ajungem la valoarea adevarata a exponentului
                sub dx, 127
                
                ; punem valoarea lui cx in bx ca sa folosim cx pentru shift de biti
                mov bx, cx

                mov cx, 7
                
                ;efectuam 7-(exponent) pentru a sti cu cate pozitii mutam la dreapta
                sub cx, dx

                ;mutam bitii cu cl pozitii
                shr ax, cl
                shr bx, cl

                ;scoatem de pe stiva valorile ramase (de care nu mai avem nevoie)
                pop dx
                pop dx

                jmp calculareAdunare

            expPrimuluiNrMaiMareDe7:
                ;scoatem de pe stiva valoarea lui cx ca sa extragem cx
                pop cx

                ;scoatem de pe stiva valoarea lui ax
                pop ax

                push cx

                ;facem masca de biti pentru matnisa (0000 0000 0111 1111) 
                mov bx, 007Fh

                ;aplicam masca de biti
                and ax, bx

                ;adaugam bit ul de 1 de inainte de ,
                add ax, 0080h

                ;scatem valoarea lui cx (din cx:dx) in dx ca sa putem extrage bx
                pop cx

                ;scatem valoarea lui dx (din cx:dx) in dx ca sa putem extrage bx
                pop dx

                ;extragem bx
                pop bx

                ;punem la loc pe stiva dx si cx pt a le folosi mai tarziu
                push dx
                push cx

                ;facem masca de biti pentru exponent (0111 1111 1000 0000) 
                mov dx, 7F80h

                ;punem in cx prima jumatate din primul nr (ax:bx)
                mov cx, word ptr a
                
                ;aplicam masca de biti
                and cx, dx

                ;punem exponentul pe pozitiile care reprezinta valoarea acestuia
                shr cx, 7

                ;scoatem constanta din standardul IEEE 754
                sub cx, 127

                ;scoatem 7 din exponentul aflat ca sa vedem cate pozitii mai avem de luat din cealalta jumatate  nr in VMSP
                sub cx, 7

                ajusteazaExponentPrimulNumar:

                    cmp cx, 0
                    je terminaAjustareaPrimulNumar

                    ;mutam biti din nr pe pare il formam la stanga
                    shl ax, 1

                    ;mutam la stanga valorile care trb extrase si bit ul extras apare in carry flag
                    shl bx, 1

                    ;adaugam acest bit pe pozitia nou eliberata in ax
                    adc ax, 0

                    ;scadem nr de biti care mai trebuie adaugati
                    dec cx

                jmp ajusteazaExponentPrimulNumar

                terminaAjustareaPrimulNumar:

                ;scoatem de pe stiva a doua jumatate a celui de al doilea numar
                pop cx

                ;scoatem de pe stiva prima jumatate a celui de al doilea numar
                pop dx
                
                ;punem pe stiva primul numar format
                push ax

                ;facem masca de biti pentru mantisa (0000 0000 0111 1111) 
                mov bx, 007Fh

                ;aplicam masca pentru mantsia
                and cx, bx

                ;adaugam 1 de inainte de ,
                add cx, 0080h

                ;facem masca de biti pentru exponent (0111 1111 1000 0000) 
                mov bx, 7F80h

                ;punem in ax prima jumatate din primul nr (ax:bx)
                mov ax, word ptr b
                
                ;aplicam masca de biti
                and ax, bx

                ;punem exponentul pe pozitiile care reprezinta valoarea acestuia
                shr ax, 7

                ;scoatem constanta din standardul IEEE 754
                sub ax, 127
                
                cmp ax, 7
                jle expAlDoileaNumarMaiMicSau7
                jmp expAlDoileaNumarMaiMareSau7

                expAlDoileaNumarMaiMicSau7:

                    ;punem in bx val lui cx ca sa facem shiftare de biti cu ajutorul lui cx
                    mov bx, cx

                    mov cx, 7

                    ; efectual calculul 7 - (nr de biti care se afla pe mantisa)
                    sub cx, ax

                    ;mutam cu 7-exp biti la dreapta pentru a arata valoarea corect a lui cx
                    shr bx, cl

                    ;scoatem primul nr format
                    pop ax

                    jmp calculareAdunare

                expAlDoileaNumarMaiMareSau7:

                    sub ax, 7

                    ajusteazaExponentAlDoileaNumar:

                        cmp ax, 0
                        je terminaAjustareaAlDoileaNumar

                        ;mutam biti din nr pe pare il formam la stanga
                        shl cx, 1

                        ;mutam la stanga valorile care trb extrase si bit ul extras apare in carry flag
                        shl dx, 1

                        ;adaugam acest bit pe pozitia nou eliberata in ax
                        adc cx, 0

                        ;scadem nr de biti care mai trebuie adaugati
                        dec ax

                    jmp ajusteazaExponentAlDoileaNumar

                terminaAjustareaAlDoileaNumar:

                pop ax

                mov bx, cx

                jmp calculareAdunare



    sunaVMSP ENDP
    
    ;input: ax - nr in CC
    ;output: afisare nr pe ecran
    afisareNumar proc
        ;punem pe stiva nr
        push ax

        ;cream o masca de biti pentru bit ul de semn
        mov bx, 8000h

        ;aplicam masca de biti
        and bx, ax

        ;punem in cx 10 pentru a efectua inmultirile
        mov cx, 10

        cmp bx, 8000h
            je AfisMinus
            jmp startDescompunere

        AfisMinus:
            ;afisam -
            mov ah, 02h
            mov dl, "-"
            int 21h

            ;scoatem de pe stiva nr si il transformam in cod direct
            pop ax
            sub ax, 1
            not ax

            ;apoi il repunem pe stiva
            push ax
        jmp startDescompunere

        startDescompunere:
            ;scoatem nr de pe stiva ca sa efectuam descopunerea
            pop ax

            ;punem pe stiva 10 (primul nr de 2 cifre => nu poate fi o cifra afisabila)
            push 10
        
        descompune:
            ;golim dx ca sa nu intervina in impartirea lui dx:ax la cx
            xor dx, dx

            div cx

            ;punem zecimala obtinuta prin extragerea restului din impartire pentru a o afisa mai tarziu
            push dx

            cmp ax, 0
                je afiseaza
        jmp descompune
        afiseaza:
            ;scoatem zecimala de pe stiva
            pop dx

            ;daca aceasta e 10 (nr pe care l am pus pe stiva la inceput) s a terminat afisarea
            cmp dx,10
                je gata
            
            ;adaugam 48 pentru a ajunge la codul cifrei in ASCII
            add dx, 48

            ;afisam
            mov ah, 02h
            int 21h
        jmp afiseaza
        
        gata:
            ret 
        
    afisareNumar endp

    ;input: dx - mesajul stocat in offsetul din dx
    ;afiseaza mesajul stocat in offsetul din dx
    afiseazaMesaj PROC
        mov ah, 09h
        int 21h
        ret
    afiseazaMesaj ENDP

    main:
        mov ax, @data
        mov ds, ax

        mov dx, offset mesInceput
        call afiseazaMesaj

        mov dx, offset mesA
        call afiseazaMesaj

        call citireNumarCC
        call transformareCCInVMSP
        
        push cx
        push dx

        mov dx, offset mesB
        call afiseazaMesaj

        call citireNumarCC

        call transformareCCInVMSP

        pop bx
        pop ax
    
        call sunaVMSP

        ;mutam val lui ax in bx ca sa nu intervina cu afisarea mesajului de final
        mov bx, ax

        mov dx, offset mesFinal
        call afiseazaMesaj

        ;mutam val lui ax la loc
        mov ax, bx

        call afisareNumar

        mov ah, 4ch
        int 21h
    end main