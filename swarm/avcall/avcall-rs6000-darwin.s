.text
	.align 2
	.globl ___builtin_avcall
___builtin_avcall:
	mflr r0
	stmw r29,-12(r1)
	stw r0,8(r1)
	mr r29,r3
	stwu r1,-1104(r1)
	lwz r9,20(r3)
	mr r30,r1
	addi r0,r1,56
	subf r9,r3,r9
	addi r9,r9,-32
	srawi r9,r9,2
	cmpwi cr0,r9,8
	ble- cr0,L102
	addi r9,r9,-8
	mr r11,r0
	mtctr r9
	addi r9,r3,64
L104:
	lwz r0,0(r9)
	addi r9,r9,4
	stw r0,0(r11)
	addi r11,r11,4
	bdnz L104
L102:
	lwz r9,1056(r29)
	subf r9,r29,r9
	addi r9,r9,-1060
	srawi. r9,r9,3
	beq- cr0,L8
	cmpwi cr0,r9,1
	beq- cr0,L11
	cmpwi cr0,r9,2
	beq- cr0,L14
	cmpwi cr0,r9,3
	beq- cr0,L17
	cmpwi cr0,r9,4
	beq- cr0,L20
	cmpwi cr0,r9,5
	beq- cr0,L23
	cmpwi cr0,r9,6
	beq- cr0,L26
	cmpwi cr0,r9,7
	beq- cr0,L29
	cmpwi cr0,r9,8
	beq- cr0,L32
	cmpwi cr0,r9,9
	beq- cr0,L35
	cmpwi cr0,r9,10
	beq- cr0,L38
	cmpwi cr0,r9,11
	beq- cr0,L41
	cmpwi cr0,r9,12
	beq- cr0,L44
L47:
	lfd f13,1156(r29)
L44:
	lfd f12,1148(r29)
L41:
	lfd f11,1140(r29)
L38:
	lfd f10,1132(r29)
L35:
	lfd f9,1124(r29)
L32:
	lfd f8,1116(r29)
L29:
	lfd f7,1108(r29)
L26:
	lfd f6,1100(r29)
L23:
	lfd f5,1092(r29)
L20:
	lfd f4,1084(r29)
L17:
	lfd f3,1076(r29)
L14:
	lfd f2,1068(r29)
L11:
	lfd f1,1060(r29)
L8:
	lwz r12,0(r29)
	lwz r9,56(r29)
	lwz r3,32(r29)
	mtctr r12
	lwz r4,36(r29)
	lwz r5,40(r29)
	lwz r6,44(r29)
	lwz r7,48(r29)
	lwz r8,52(r29)
	lwz r10,60(r29)
	bctrl
	lwz r9,12(r29)
	cmpwi cr0,r9,1
	beq- cr0,L49
	cmpwi cr0,r9,0
	beq- cr0,L105
	cmpwi cr0,r9,2
	beq- cr0,L108
	cmpwi cr0,r9,3
	beq- cr0,L108
	cmpwi cr0,r9,4
	beq- cr0,L108
	cmpwi cr0,r9,5
	beq- cr0,L107
	cmpwi cr0,r9,6
	beq- cr0,L107
	cmpwi cr0,r9,7
	beq- cr0,L105
	cmpwi cr0,r9,8
	beq- cr0,L105
	cmpwi cr0,r9,9
	beq- cr0,L105
	cmpwi cr0,r9,10
	beq- cr0,L105
	addi r0,r9,-11
	cmplwi cr0,r0,1
	ble- cr0,L106
	cmpwi cr0,r9,13
	bne+ cr0,L72
	lwz r9,8(r29)
	stfs f1,0(r9)
	b L49
L72:
	cmpwi cr0,r9,14
	bne+ cr0,L74
	lwz r9,8(r29)
	stfd f1,0(r9)
	b L49
L74:
	cmpwi cr0,r9,15
	beq- cr0,L105
	cmpwi cr0,r9,16
	bne+ cr0,L49
	lwz r0,4(r29)
	andi. r9,r0,1
	beq- cr0,L79
	lwz r9,16(r29)
	cmpwi cr0,r9,1
	bne+ cr0,L80
	lwz r9,8(r29)
	lbz r0,0(r3)
	stb r0,0(r9)
	b L49
L80:
	cmpwi cr0,r9,2
	bne+ cr0,L82
	lwz r9,8(r29)
	lhz r0,0(r3)
	sth r0,0(r9)
	b L49
L82:
	cmpwi cr0,r9,4
	bne+ cr0,L84
	lwz r9,8(r29)
	lwz r0,0(r3)
	stw r0,0(r9)
	b L49
L84:
	cmpwi cr0,r9,8
	bne+ cr0,L86
	lwz r0,0(r3)
	lwz r9,8(r29)
	stw r0,0(r9)
	lwz r11,8(r29)
	lwz r0,4(r3)
	stw r0,4(r11)
	b L49
L86:
	addi r0,r9,3
	srwi r10,r0,2
	addic. r10,r10,-1
	blt- cr0,L49
L91:
	slwi r9,r10,2
	lwz r11,8(r29)
	lwzx r0,r9,r3
	addic. r10,r10,-1
	stwx r0,r9,r11
	bge+ cr0,L91
	b L49
L79:
	andi. r9,r0,512
	beq- cr0,L49
	lwz r0,16(r29)
	cmpwi cr0,r0,1
	bne+ cr0,L94
L108:
	lwz r9,8(r29)
	stb r3,0(r9)
	b L49
L94:
	cmpwi cr0,r0,2
	bne+ cr0,L96
L107:
	lwz r9,8(r29)
	sth r3,0(r9)
	b L49
L96:
	cmpwi cr0,r0,4
	bne+ cr0,L98
L105:
	lwz r9,8(r29)
	stw r3,0(r9)
	b L49
L98:
	cmpwi cr0,r0,8
	bne+ cr0,L49
L106:
	lwz r9,8(r29)
	stw r3,0(r9)
	lwz r11,8(r29)
	stw r4,4(r11)
L49:
	lwz r1,0(r1)
	li r3,0
	lwz r0,8(r1)
	lmw r29,-12(r1)
	mtlr r0
	blr
