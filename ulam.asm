; $040f  => The screen location of the first cell of the spiral.
; $0200  => The screen location of the last cell of the spiral.
; $00-01 => A memory location representing the screen location of the current
;           cell.
; $02    => A memory location representing the current direction, i.e. the
;           direction of the next cell relative to the current cell.
; $03    => A memory location representing the current line lenght, i.e. the
;           number of steps from the last change of direction to the next one.
; $04    => A memory location representing the current line steps, i.e. the
;           number of steps traversed from the last change of direction.
; $05    => A memory location representing the current prime, i.e. the number
;           of which multiples will be "crossed out".
;
; Directions:
; 1 => up    (0b0001)
; 2 => right (0b0010)
; 4 => down  (0b0100)
; 8 => left  (0b1000)

jsr init
jsr generate
jsr end

init:
  jsr move_to_origin
  lda #$02
  sta $05                         ; Set the current prime to 2.
  rts

; Move to the first cell of the spiral.
move_to_origin:
  lda #$0f
  sta $00                         ; Set the low byte of the current cell.
  lda #$04
  sta $01                         ; Set the high byte of the current cell.
  lda #$02
  sta $02                         ; Set the current direction to right.
  lda #$01
  sta $03                         ; Set the current line lenght to 3.
  lda #$00
  sta $04                         ; Set the current line steps to 0.
  rts

; Generate the Ulam spiral.
generate:
  jsr cross_out_current_cell      ; Cross out the first cell of the spiral.
  jsr step
loop:
  jsr cross_out_multiples
  jsr next_prime
  lda $05                         ; Load the current prime to the A register.
  cmp #$21                        ; Compare the current prime with the number
                                  ; one greater than the square root of the
                                  ; number of cells of the screen.
  bcc loop                        ; Branch if the current prime is less than
                                  ; the root.
  rts

; Cross out (i.e. colorize with the background color) the multiples of the
; current prime. This routine assumes that the current position is equal to
; the position of the current prime.
cross_out_multiples:
  jsr at_finish
  cmp #$00
  beq cross_out_multiples_end
  ldy $05                         ; Load the current prime to the Y register.
  jsr step_n_times
  jsr cross_out_current_cell
  jmp cross_out_multiples
cross_out_multiples_end:
  rts

; Colorize the current cell with the background color.
cross_out_current_cell:
  ldy #$00
  lda #$01
  sta ($00), y
  rts

; Check if the current cell is the one at the top-left corner of the screen.
; If that is the case, set the A register to 0. Otherwise, set the A register
; to 1.
at_finish:
  lda #$01
  ldx $00                         ; Load the low byte of the current cell to
                                  ; the X register.
  cpx #$00                        ; Compare it with the low byte of the cell
                                  ; at top-left corner of the screen.
  bne at_finish_end
  ldx $01                         ; Load the high byte of the current cell to
                                  ; the X register.
  cpx #$02                        ; Compare it with the high byte of the cell
                                  ; at top-left corner of the screen.
  bne at_finish_end
  lda #$00
at_finish_end:
  rts

; Move to the first from origin non crossed out cell.
next_prime:
  jsr move_to_origin
  ldy $05                         ; Load the current prime to the Y register.
  dey
  jsr step_n_times                ; Move to the position of the current prime.
next_prime_loop:
  jsr step
  inc $05                         ; Increment the current prime.
  ldy #$00
  lda ($00), y                    ; Compare the current cell with the value
                                  ; of the foreground color.
  bne next_prime_loop
next_prime_end:
  rts

; Turn 90 degree anti-clockwise.
turn:
  lda $02                         ; Load the current direction to the A
                                  ; register.
  lsr
  bcs turn_left                   ; If the Carry Flag is set then the current
                                  ; direction is up (0b0001) so change it to
                                  ; left.
  jmp change_direction
turn_left:
  lda #$08
change_direction:
  sta $02                         ; Store the value of the A register in the
                                  ; memory location representing the current
                                  ; direction.
  rts

; Move to the next cell of the spriral.
step:
  lda $04                         ; Load the current line steps to the A
                                  ; register.
  cmp $03                         ; Compare it with the current line lenght.
  bne update_current_cell
  lda #$00
  sta $04                         ; Set the current line steps to 0.
update_current_direction:
  jsr turn
update_current_line_lenght:
  lda $02                         ; Load the current direction to the A
                                  ; register.
  and #$0a                        ; Mask out all the bits except 2nd and 4th.
  cmp #$00                        ; If the A register is zero then the
                                  ; current direction is other than right
                                  ; (0b0010) or left (0b1000).
  beq update_current_cell
  inc $03                         ; Increment the current line lenght.
update_current_cell:
  lda $02                         ; Load the current direction to the A
                                  ; register.
  lsr
  bcs up
  lsr
  bcs right
  lsr
  bcs down
  lsr
  bcs left
right:
  inc $00                         ; Increment the low byte of the current cell.
  jmp update_current_line_steps
up:
  lda $00                         ; Load the low byte of the current cell to
                                  ; the A register.
  sec
  sbc #$20
  sta $00                         ; Store the value of the A register in the
                                  ; memory location representing the low byte
                                  ; of the current cell.
  bcc upup                        ; Handle underflow if occured.
  jmp update_current_line_steps
upup:
  dec $01                         ; Decrement the high byte of the current
                                  ; cell.
  jmp update_current_line_steps
left:
  dec $00                         ; Decrement the low byte of the current cell.
  jmp update_current_line_steps
down:
  lda $00                         ; Load the current cell to the A register.
  clc
  adc #$20
  sta $00                         ; Store the value of the A register in the
                                  ; memory location representing the low byte
                                  ; of the current cell.
  bcs downdown                    ; Handle overflow if occured.
  jmp update_current_line_steps
downdown:
  inc $01                         ; Increment the high byte of the current
                                  ; cell.
  jmp update_current_line_steps
update_current_line_steps:
  inc $04                         ; Increment the current line steps.
  rts

; Step as many times as specified by the number stored in the Y register.
step_n_times:
  jsr at_finish
  cmp #$00
  beq step_n_times_end
  jsr step
  dey
  beq step_n_times_end
  jmp step_n_times
step_n_times_end:
  rts

end:
