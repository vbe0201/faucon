call 0xDEAD
call 0xF0
call $r5
lcall 0x123456
lbra 0x123456

bra $r5
bra 0xf0
bra 0x47
bra 0x100
bra 0xDEAD
bra e -0x5
bra ae 0x1234
bra b8 $r0 0x0 ne 0x1234
bra b8 $r15 0x3 e 0x12

// 00000016: f5 0e da 00           bra 0xf0
// 00000016: f4 0e 31              bra 0x47
// 00000016: f5 0e ea 00           bra 0x100

/*
#	{ 0x000000f4, 0x000020fe, T(ol0), N("bra"), T(p), T(bt) },
#	{ 0x000020f4, 0x00003ffe, T(ol0), N("bra"), T(abt), ENDMARK },
:bra addr16 is raw_opcode=0xf5; subopcodeb1_0_7=0x20; addr16 {
  goto addr16;
}
*/

/*
00000010: f9 54                 bra $r5
00000012: f5 0e 9b de         B bra 0xffffffffffffdead
00000016: f4 0e fc              bra 0x12
00000019: f4 0b e2              bra e 0xfffffffffffffffb
0000001c: f5 18 18 12           bra ae 0x1234
00000020: 33 0d 00 14 12        bra b8 $r0 0x0 ne 0x1234
00000025: 33 f0 03 ed           bra b8 $r15 0x3 e 0x12
*/

/*
	{ 0x000000f4, 0x000020fe, T(ol0), N("bra"), T(p), T(bt) },
	{ 0x000020f4, 0x00003ffe, T(ol0), N("bra"), T(abt), ENDMARK },
	{ 0x000004f9, 0x00000fff, OP2B, N("bra"), REG1 },

	{ 0x00000033, 0x00000f3f, OP4B, N("bra"), T(sz), REG1, IMM8, N("e"), SFBTARG, .fmask = F_FUC5P },
	{ 0x00000433, 0x00000f3f, OP4B, N("bra"), T(sz), REG1, IMM8, N("ne"), SFBTARG, .fmask = F_FUC5P },
	{ 0x00000933, 0x00000f3f, OP5B, N("bra"), T(sz), REG1, IMM8, N("e"), LFBTARG, .fmask = F_FUC5P },
	{ 0x00000a33, 0x00000f3f, OP5B, N("bra"), T(sz), REG1, IMM16, N("e"), SFFBTARG, .fmask = F_FUC5P },
	{ 0x00000b33, 0x00000f3f, OP6B, N("bra"), T(sz), REG1, IMM16, N("e"), LFFBTARG, .fmask = F_FUC5P },
	{ 0x00000d33, 0x00000f3f, OP5B, N("bra"), T(sz), REG1, IMM8, N("ne"), LFBTARG, .fmask = F_FUC5P },
	{ 0x00000e33, 0x00000f3f, OP5B, N("bra"), T(sz), REG1, IMM16, N("ne"), SFFBTARG, .fmask = F_FUC5P },
	{ 0x00000f33, 0x00000f3f, OP6B, N("bra"), T(sz), REG1, IMM16, N("ne"), LFFBTARG, .fmask = F_FUC5P },
*/
