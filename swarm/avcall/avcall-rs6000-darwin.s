.text
	.align 2
	.globl ___builtin_avcall
___builtin_avcall:
	mflr r0
	stmw r26,-24(r1)
	stw r0,8(r1)
	stwu r1,-1152(r1)
	mr r30,r1
	stw r3,1176(r30)
	addi r0,r1,56
	stw r0,1088(r30)
	lwz r9,1176(r30)
	lwz r9,20(r9)
	lwz r0,1176(r30)
	subf r9,r0,r9
	addi r0,r9,-36
	srawi r0,r0,2
	stw r0,1092(r30)
	lwz r9,1176(r30)
	lwz r9,1060(r9)
	lwz r0,1176(r30)
	subf r9,r0,r9
	addi r0,r9,-1064
	srawi r0,r0,3
	stw r0,1096(r30)
	lwz r0,1096(r30)
	subfic r0,r0,8
	stw r0,1100(r30)
L2:
	lwz r0,1100(r30)
	lwz r9,1092(r30)
	cmpw cr0,r0,r9
	blt cr0,L5
	b L3
L5:
	lwz r9,1100(r30)
	lwz r0,1096(r30)
	add r0,r9,r0
	slwi r9,r0,2
	lwz r0,1088(r30)
	add r9,r9,r0
	addi r8,r9,-32
	lwz r10,1176(r30)
	lwz r0,1100(r30)
	slwi r9,r0,2
	addi r11,r9,32
	addi r0,r10,4
	mr r9,r0
	lwzx r0,r9,r11
	stw r0,0(r8)
	lwz r9,1100(r30)
	addi r0,r9,1
	stw r0,1100(r30)
	b L2
L3:
	lwz r9,1176(r30)
	lwz r9,1060(r9)
	lwz r0,1176(r30)
	subf r9,r0,r9
	addi r0,r9,-1064
	srawi r0,r0,3
	stw r0,1092(r30)
	lwz r0,1092(r30)
	cmpwi cr0,r0,0
	bne cr0,L6
	b L7
L6:
	lwz r0,1092(r30)
	cmpwi cr0,r0,1
	bne cr0,L9
	b L10
L9:
	lwz r0,1092(r30)
	cmpwi cr0,r0,2
	bne cr0,L12
	b L13
L12:
	lwz r0,1092(r30)
	cmpwi cr0,r0,3
	bne cr0,L15
	b L16
L15:
	lwz r0,1092(r30)
	cmpwi cr0,r0,4
	bne cr0,L18
	b L19
L18:
	lwz r0,1092(r30)
	cmpwi cr0,r0,5
	bne cr0,L21
	b L22
L21:
	lwz r0,1092(r30)
	cmpwi cr0,r0,6
	bne cr0,L24
	b L25
L24:
	lwz r0,1092(r30)
	cmpwi cr0,r0,7
	bne cr0,L27
	b L28
L27:
	lwz r0,1092(r30)
	cmpwi cr0,r0,8
	bne cr0,L30
	b L31
L30:
	lwz r0,1092(r30)
	cmpwi cr0,r0,9
	bne cr0,L33
	b L34
L33:
	lwz r0,1092(r30)
	cmpwi cr0,r0,10
	bne cr0,L36
	b L37
L36:
	lwz r0,1092(r30)
	cmpwi cr0,r0,11
	bne cr0,L39
	b L40
L39:
	lwz r0,1092(r30)
	cmpwi cr0,r0,12
	bne cr0,L42
	b L43
L42:
L46:
	lwz r9,1176(r30)
	lfd f13,1160(r9)
L43:
	lwz r9,1176(r30)
	lfd f12,1152(r9)
L40:
	lwz r9,1176(r30)
	lfd f11,1144(r9)
L37:
	lwz r9,1176(r30)
	lfd f10,1136(r9)
L34:
	lwz r9,1176(r30)
	lfd f9,1128(r9)
L31:
	lwz r9,1176(r30)
	lfd f8,1120(r9)
L28:
	lwz r9,1176(r30)
	lfd f7,1112(r9)
L25:
	lwz r9,1176(r30)
	lfd f6,1104(r9)
L22:
	lwz r9,1176(r30)
	lfd f5,1096(r9)
L19:
	lwz r9,1176(r30)
	lfd f4,1088(r9)
L16:
	lwz r9,1176(r30)
	lfd f3,1080(r9)
L13:
	lwz r9,1176(r30)
	lfd f2,1072(r9)
L10:
	lwz r9,1176(r30)
	lfd f1,1064(r9)
L7:
	lwz r9,1176(r30)
	lwz r11,1176(r30)
	lwz r10,1176(r30)
	lwz r8,1176(r30)
	lwz r7,1176(r30)
	lwz r29,1176(r30)
	lwz r28,1176(r30)
	lwz r27,1176(r30)
	lwz r26,1176(r30)
	lwz r0,0(r9)
	lwz r3,36(r11)
	lwz r4,40(r10)
	lwz r5,44(r8)
	lwz r6,48(r7)
	lwz r7,52(r29)
	lwz r8,56(r28)
	lwz r9,60(r27)
	lwz r10,64(r26)
	mtlr r0
	blrl
	mr r0,r3
	stw r0,1100(r30)
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,1
	bne cr0,L47
	b L48
L47:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,0
	bne cr0,L49
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lwz r0,1100(r30)
	stw r0,0(r9)
	b L48
L49:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,2
	bne cr0,L51
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lbz r0,1103(r30)
	stb r0,0(r9)
	b L48
L51:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,3
	bne cr0,L53
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lbz r0,1103(r30)
	stb r0,0(r9)
	b L48
L53:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,4
	bne cr0,L55
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lbz r0,1103(r30)
	stb r0,0(r9)
	b L48
L55:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,5
	bne cr0,L57
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lhz r0,1102(r30)
	sth r0,0(r9)
	b L48
L57:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,6
	bne cr0,L59
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lhz r0,1102(r30)
	sth r0,0(r9)
	b L48
L59:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,7
	bne cr0,L61
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lwz r0,1100(r30)
	stw r0,0(r9)
	b L48
L61:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,8
	bne cr0,L63
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lwz r0,1100(r30)
	stw r0,0(r9)
	b L48
L63:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,9
	bne cr0,L65
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lwz r0,1100(r30)
	stw r0,0(r9)
	b L48
L65:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,10
	bne cr0,L67
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lwz r0,1100(r30)
	stw r0,0(r9)
	b L48
L67:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,11
	beq cr0,L70
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,12
	beq cr0,L70
	b L69
L70:
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lwz r0,1100(r30)
	stw r0,0(r9)
	lwz r9,1176(r30)
	lwz r9,8(r9)
	addi r9,r9,4
	stw r4,0(r9)
	b L48
L69:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,13
	bne cr0,L72
	lwz r9,1176(r30)
	lwz r9,8(r9)
	stfs f1,0(r9)
	b L48
L72:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,14
	bne cr0,L74
	lwz r9,1176(r30)
	lwz r9,8(r9)
	stfd f1,0(r9)
	b L48
L74:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,15
	bne cr0,L76
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lwz r0,1100(r30)
	stw r0,0(r9)
	b L48
L76:
	lwz r9,1176(r30)
	lwz r0,12(r9)
	cmpwi cr0,r0,16
	bne cr0,L48
	lwz r9,1176(r30)
	lwz r0,4(r9)
	rlwinm r0,r0,0,31,31
	cmpwi cr0,r0,0
	beq cr0,L79
	lwz r9,1176(r30)
	lwz r0,16(r9)
	cmpwi cr0,r0,1
	bne cr0,L80
	lwz r9,1176(r30)
	lwz r11,8(r9)
	lwz r9,1100(r30)
	lbz r0,0(r9)
	stb r0,0(r11)
	b L48
L80:
	lwz r9,1176(r30)
	lwz r0,16(r9)
	cmpwi cr0,r0,2
	bne cr0,L82
	lwz r9,1176(r30)
	lwz r11,8(r9)
	lwz r9,1100(r30)
	lhz r0,0(r9)
	sth r0,0(r11)
	b L48
L82:
	lwz r9,1176(r30)
	lwz r0,16(r9)
	cmpwi cr0,r0,4
	bne cr0,L84
	lwz r9,1176(r30)
	lwz r11,8(r9)
	lwz r9,1100(r30)
	lwz r0,0(r9)
	stw r0,0(r11)
	b L48
L84:
	lwz r9,1176(r30)
	lwz r0,16(r9)
	cmpwi cr0,r0,8
	bne cr0,L86
	lwz r9,1176(r30)
	lwz r11,8(r9)
	lwz r9,1100(r30)
	lwz r0,0(r9)
	stw r0,0(r11)
	lwz r9,1176(r30)
	lwz r9,8(r9)
	addi r11,r9,4
	lwz r9,1100(r30)
	addi r9,r9,4
	lwz r0,0(r9)
	stw r0,0(r11)
	b L48
L86:
	lwz r9,1176(r30)
	lwz r9,16(r9)
	addi r0,r9,3
	srwi r0,r0,2
	stw r0,1104(r30)
L88:
	lwz r9,1104(r30)
	addi r0,r9,-1
	stw r0,1104(r30)
	cmpwi cr0,r0,0
	bge cr0,L90
	b L48
L90:
	lwz r11,1176(r30)
	lwz r0,1104(r30)
	slwi r9,r0,2
	lwz r0,8(r11)
	add r11,r9,r0
	lwz r0,1104(r30)
	slwi r9,r0,2
	lwz r0,1100(r30)
	add r9,r9,r0
	lwz r0,0(r9)
	stw r0,0(r11)
	b L88
L79:
	lwz r9,1176(r30)
	lwz r0,4(r9)
	rlwinm r0,r0,0,22,22
	cmpwi cr0,r0,0
	beq cr0,L48
	lwz r9,1176(r30)
	lwz r0,16(r9)
	cmpwi cr0,r0,1
	bne cr0,L93
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lbz r0,1103(r30)
	stb r0,0(r9)
	b L48
L93:
	lwz r9,1176(r30)
	lwz r0,16(r9)
	cmpwi cr0,r0,2
	bne cr0,L95
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lhz r0,1102(r30)
	sth r0,0(r9)
	b L48
L95:
	lwz r9,1176(r30)
	lwz r0,16(r9)
	cmpwi cr0,r0,4
	bne cr0,L97
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lwz r0,1100(r30)
	stw r0,0(r9)
	b L48
L97:
	lwz r9,1176(r30)
	lwz r0,16(r9)
	cmpwi cr0,r0,8
	bne cr0,L48
	lwz r9,1176(r30)
	lwz r9,8(r9)
	lwz r0,1100(r30)
	stw r0,0(r9)
	lwz r9,1176(r30)
	lwz r9,8(r9)
	addi r9,r9,4
	stw r4,0(r9)
L48:
	li r0,0
	mr r3,r0
	lwz r1,0(r1)
	lwz r0,8(r1)
	mtlr r0
	lmw r26,-24(r1)
	blr
