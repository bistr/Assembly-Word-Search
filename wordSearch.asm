;KARH PRAKTIKUM 2018-2019

masm
model	small
.stack	256
.data

;file things
filename    db  'hehehe.txt',0
filepointer dd filename
handle  dw  0
buffer db 1001 dup('$') 
fileLength dw ?

;settings
wholeWordsOnly db 1

;word to look for
targetWord db 20 dup('$')
wordLength dw 0

;global variables
isEndOfWord db 0
currentWordLength dw 0
currentLine dw 1
currentWord dw 1
currentPos dw 0
sentencesCount dw ?
occurrences dw 0
hasPunctuation db 0

;messages
lineMessage db 'Line:',9,'$'
wordMessage db 'Word:',9,'$'
positionMessage db 'Position:',9,'$'
startMessage db '1 Search for Word',10,'2 Get Sentence Count',10,'3 Exit',10,'$'
inputWordMessage db 'Input word',10,'$'
sentenceCountMessage db 'Sentences in document: ','$'
setWholeWordsMessage db '0 Substrings Allowed',10,'1 Whole Words Only',10,'$'
wrongChoiceMessage db 'Wrong choice! Back to main menu.',10,'$'
occurrencesMessage db 'Word appears ','$'
occurrencesMessage2 db 'times',10,'$'
badInputMessage db 'Cannot check for punctuation when whole words flag is set :(',10,'$'


;store things
storeBX dw ?
storeCX dw ?
storeDX dw ?
storeSI dw ?
storeDI dw ?
storeAX dw ?

.code

;these two are so we don't lose any info when using functions
storeRegisters:
    mov storeAX,ax
    mov storeBX,bx
    mov storeCX,cx
    mov storeDX,dx
    mov storeSI,si
    mov storeDI,di
    ret

recoverRegisters:
    mov dx, storeDX
    mov di, storeDI
    mov si, storeSI
    mov bx, storeBX
    mov cx, storeCX
    mov ax, storeAX
    ret

;prints out contents of ax
print:
    call storeRegisters
    mov bx,10
    xor cx,cx
    a:
        xor dx,dx
        div bx
        push dx
        inc cx
        cmp ax,0
        jne a
        b:
            pop dx
            add dl,"0"
            mov ah,02h
            int 21h
            loop b
        mov dx,' '
        mov ah,02h
        int 21h
        call recoverRegisters
        ret

printNewLine:
    call storeRegisters
    mov dx,10
    mov ah,02h
    int 21h
    call recoverRegisters
    ret

countSentences:
    call storeRegisters
    ;using si to count sentences
    xor si,si
    ;using di to count chars in sentence
    xor di,di
    ;using bx as indexing of letter in file
    xor bx,bx
    xor dx,dx
    xor cx,cx
    printLoop:
        mov dl,[buffer+bx]
        ;we count these as ends of sentences
        cmp dl,'.'
        je increaseCounter
        cmp dl,'?'
        je increaseCounter
        cmp dl,'!'
        je increaseCounter
        ;these are useless (can't have a sentence with only punctuation)
        cmp dl,','
        je loopEnd
        cmp dl,' '
        je loopEnd
        cmp dl,'-'
        je loopEnd
        ;if it's not punctuation - add to length of sentence
        inc di
        jmp loopEnd
        increaseCounter:
            ;if sentence is 0 chars long - not a sentence :(
            cmp di, 0
            je loopEnd
            ;not 0 long - therefore it is a real sentence
            inc si
            xor di,di
            jmp loopEnd
    loopEnd:
        inc bx
        cmp bx,fileLength
        je endOfCounting
        loop printLoop
    endOfCounting:
        ; i am not sure if we count the final words(even without a punctuation mark) as a sentence
        ; let's assume we do
        ;
        ; i put 1 here because of $
        
        ;??????????
        ;uncomment?
        ;cmp di,1
        ;je resultFromCounting
        ;inc si
    resultFromCounting:
        mov sentencesCount, si
        call recoverRegisters
    ret

;checks if symbol in dl is punctuation
;doesnt mess with registers
checkIfEndOfWord:
    cmp dl,'.'
    je returnTrue
    cmp dl,'?'
    je returnTrue
    cmp dl,'!'
    je returnTrue
    cmp dl,','
    je returnTrue
    cmp dl,'-'
    je returnTrue
    cmp dl,' '
    je returnTrue
    cmp dl, 10
    je returnTrue
    mov isEndOfWord,0
    jmp endCheck
    returnTrue:
        mov isEndOfWord,1
    endCheck:
    ret

countWords:
;modifies currentWordLength and currentWord and currentLine
;using bx to count currentWordLength
;using si to count wordsTotal
;using di to count currentLine
    call storeRegisters
    mov bx,currentWordLength
    mov si,currentWord
    mov di,currentLine
    ;we count these as ends of words
    cmp dl,10
    je increaseLineCounter
    call checkIfEndOfWord
    cmp isEndOfWord,1
    je increaseWordCounter

    inc bx
    jmp endOfCountWords
    increaseLineCounter:
        inc di
        ;if using gedit - 1
        mov si,1
    increaseWordCounter:
        cmp bx,0
        je endOfCountWords
        inc si
        xor bx,bx
    endOfCountWords:
        mov currentWordLength,bx
        mov currentWord,si
        mov currentLine,di
        call recoverRegisters
    ret

displayCurrentWord:
    call storeRegisters
    mov bx,currentWordLength
    mov di,currentPos
    sub di,bx
    inc di

    
    displayLoop:
        cmp di,fileLength
        je exitLoop
        mov dl,[buffer+di]
        call checkIfEndOfWord
        cmp isEndOfWord,1
        je exitLoop

        mov ah,02h
        int 21h
        inc di
        loop displayLoop
    exitLoop:
        call recoverRegisters
        ret

searchForWord:

call storeRegisters
mov currentWordLength,0
mov currentWord,1
mov currentLine,1
mov currentPos, 0
mov occurrences,0

xor bx,bx
xor di,di
xor dx,dx
xor ax,ax
xor si,si
;iterate word with bx
;iterate text with di
;count words with si
iterateTextLoop:
    xor cx,cx
    mov dl,[buffer+bx]
    call countWords
    cmp dl,[targetWord+di]
    jne badLoopEnd
goodLoopEnd:
   inc di
   cmp di,wordLength
   jl endLoop
   cmp wholeWordsOnly,0
   je displayRes
   mov ax,currentWordLength
   cmp ax,wordLength
   jne badLoopEnd
   inc bx
   mov dl,[buffer+bx]
   call checkIfEndOfWord
   dec bx
   cmp isEndOfWord,1
   jne badLoopEnd
   jmp displayRes
badLoopEnd:
    cmp wordLength,1
    je stop
    cmp di,0
    je stop

    ;this is added because word might be wrong so far, but still contain target
    xor di,di
    jmp iterateTextLoop

    stop:
    xor di,di
    jmp endLoop    
    ;xor di,di
    ;jmp endLoop
endLoop:
    inc bx
    cmp bx,fileLength
    je endOfSearching
    loop iterateTextLoop
displayRes:
    mov cx,occurrences
    inc cx
    mov occurrences,cx
    xor cx,cx
    mov currentPos,bx
    mov ah,9
    mov dx,offset lineMessage
    int 21h
    mov ax,currentLine
    call print
    mov ah,9
    mov dx,offset positionMessage
    int 21h
    ;checking for punctuation at end of word!
    mov dl,[buffer+bx]
    mov ax,currentWord
    call checkIfEndOfWord
    cmp isEndOfWord,0
    je continue
    dec ax
    continue:
    call print
    ;IF YOU WANT DISPLAY WORD - UNCOMMENT HERE
    ;CAUTION - WORKS NOT GREAT
    ;mov ah,9
    ;mov dx,offset wordMessage
    ;int 21h
    ;call displayCurrentWord

    call printNewLine
    jmp badLoopEnd
endOfSearching:
    mov ah,09h
    mov dx, offset occurrencesMessage
    int 21h
    mov ax,occurrences
    call print
    mov ah,09h
    mov dx, offset occurrencesMessage2
    int 21h
    call recoverRegisters
    mov currentPos,0
    ret

checkForPunctuationInTarget:
    call storeRegisters
    mov hasPunctuation,0
    xor bx,bx
    checkLoop:
    mov dl,[targetWord+bx]
    inc bx
    call checkIfEndOfWord
    cmp isEndOfWord,1
    je badEnd
    cmp bx,wordLength
    je endCheckLoop
    loop checkLoop

    badEnd:
    mov ah,09h
    mov dx, offset badInputMessage
    int 21h
    mov hasPunctuation,1
    endCheckLoop:
        call recoverRegisters
        ret

inputWord:
    call storeRegisters
    mov ah,9
    mov dx,offset inputWordMessage
    int 21h
    mov targetWord,'$'
    mov ax,0
    mov wordLength,ax
    xor bx,bx
    addCharacter:
        mov ah,01h
        int 21h
        cmp al,13
        je endOfInput
        mov [targetWord+bx],al
        inc bx
        loop addCharacter
        endOfInput:
            mov dx,10
            mov ah,02h
            int 21h
            mov wordLength,bx
            call recoverRegisters
            ret
setFlags:
call storeRegisters
call printNewLine
mov ah,9
mov dx,offset setWholeWordsMessage
int 21h
mov ah,01h
int 21h
call printNewLine
cmp al,'1'
jg wrongChoice
cmp al,'0'
jl wrongChoice
sub al,48
mov wholeWordsOnly,al
call recoverRegisters
ret

main:
    mov ax, @data  ; initialization of data segment
    mov ds, ax
open:
    xor cx,cx
    lea dx, filename
    mov al, 02h
    mov ah,3dh
    int 21h
    mov handle, ax
read:
    mov ah,3fh
    mov bx, handle
    mov cx, 3E9h ; <------------ how many symbols to write (1001)
    lea dx, buffer 
    int 21h
    push dx
    mov handle, ax
    mov fileLength, ax
close:
    mov ah, 3eh
    mov bx, handle
    int 21h
call countSentences

startScreen:
call printNewLine
mov ah,9
mov dx,offset startMessage
int 21h

mov ah,01h
int 21h
call printNewLine
cmp al,'1'
jl wrongChoice
cmp al,'3'
jg wrongChoice

cmp al,'2'
jl search
je getSentences
jg exit

wrongChoice:
call printNewLine
mov ah,9
mov dx,offset wrongChoiceMessage
int 21h
jmp startScreen

search:
call setFlags
call inputWord
cmp wholeWordsOnly,0
je next
call checkForPunctuationInTarget
cmp hasPunctuation,0
je next
jmp startScreen
next:
call searchForWord
jmp startScreen

getSentences:
call printNewLine
mov ah,9
mov dx, offset sentenceCountMessage
int 21h
mov ax,sentencesCount
call print
call printNewLine
jmp startScreen

exit:
    mov AX, 4c00h
    int 21h
    end main

