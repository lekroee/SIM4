/*
 * sim4.c
 *
 *  Created on: Oct 17, 2022
 *      Author: lekro
 *      Sim4.c
 *      CSC 252
 *      This class represents a single cycle processor that performs a set
 *      of basic functions: add, addu, sub, subu, and, or, xor, slt, j,
 *      beq, addi, addiu, slti, lw, and sw.
 *
 */

#include "sim4.h"

WORD getInstruction(WORD curPC, WORD *instructionMemory) {
	/*
	 * Returns the instruction located at the index, PC, from
	 * the instructionMemory array
	 */
	return instructionMemory[curPC/4];
}

void extract_instructionFields(WORD instruction, InstructionFields *fieldsOut) {
	/*
	 * Sets the "control wires" in fieldsOut using the given instruction, bit-shifting
	 * and masking. The bits are broken down as follows depending on instruction type:
	 *
	 * R
	 * 31-26:	opcode
	 * 25-21:	rs
	 * 20-16:	rt
	 * 15-11:	rd
	 * 10-6: 	shamt
	 * 5-0:		funct
	 *
	 * I
	 * 31-26:	opcode
	 * 25-21:	rs
	 * 20-16:	rt
	 * 15-0:	imm16
	 *
	 * j
	 * 31-26:	opcode
	 * 25-0:	address
	 *
	 * imm32 is sign-extended imm16
	 */
	fieldsOut -> opcode = (instruction >> 26) & 0x3f;
	fieldsOut -> rs = (instruction >> 21) & 0x1f;
	fieldsOut -> rt = (instruction >> 16) & 0x1f;
	fieldsOut -> rd = (instruction >> 11) & 0x1f;
	fieldsOut -> shamt = (instruction >> 6) & 0x1f;
	fieldsOut -> funct = instruction & 0x3f;
	fieldsOut -> imm16 = instruction & 0xffff;
	fieldsOut -> imm32 = signExtend16to32(fieldsOut -> imm16);
	fieldsOut -> address = instruction & 0x3ffffff;
}

int  fill_CPUControl(InstructionFields *fields, CPUControl *controlOut) {
	/*
	 * Sets the "control wires" for the CPU based on the opcode and funct
	 * fields. Uses the fields from the InstructionFields struct and sets
	 * the wires in the CPUControl struct
	 */
	switch(fields -> opcode) {
		case(0):
				switch(fields -> funct) {
					case(32): // add
							controlOut -> ALUsrc = 0;
							controlOut -> ALU.op = 2;
							controlOut -> ALU.bNegate = 0;
							controlOut -> memRead = 0;
							controlOut -> memWrite = 0;
							controlOut -> memToReg = 0;
							controlOut -> regDst = 1;
							controlOut -> regWrite = 1;
							controlOut -> branch = 0;
							controlOut -> jump = 0;
							return 1;
					case(33): // addu
							controlOut -> ALUsrc = 0;
							controlOut -> ALU.op = 2;
							controlOut -> ALU.bNegate = 0;
							controlOut -> memRead = 0;
							controlOut -> memWrite = 0;
							controlOut -> memToReg = 0;
							controlOut -> regDst = 1;
							controlOut -> regWrite = 1;
							controlOut -> branch = 0;
							controlOut -> jump = 0;
						return 1;
					case(34): // sub
							controlOut -> ALUsrc = 0;
							controlOut -> ALU.op = 2;
							controlOut -> ALU.bNegate = 1;
							controlOut -> memRead = 0;
							controlOut -> memWrite = 0;
							controlOut -> memToReg = 0;
							controlOut -> regDst = 1;
							controlOut -> regWrite = 1;
							controlOut -> branch = 0;
							controlOut -> jump = 0;
							return 1;
					case(35): // subu
							controlOut -> ALUsrc = 0;
							controlOut -> ALU.op = 2;
							controlOut -> ALU.bNegate = 1;
							controlOut -> memRead = 0;
							controlOut -> memWrite = 0;
							controlOut -> memToReg = 0;
							controlOut -> regDst = 1;
							controlOut -> regWrite = 1;
							controlOut -> branch = 0;
							controlOut -> jump = 0;
							return 1;
					case(36): // and
							controlOut -> ALUsrc = 0;
							controlOut -> ALU.op = 0;
							controlOut -> ALU.bNegate = 0;
							controlOut -> memRead = 0;
							controlOut -> memWrite = 0;
							controlOut -> memToReg = 0;
							controlOut -> regDst = 1;
							controlOut -> regWrite = 1;
							controlOut -> branch = 0;
							controlOut -> jump = 0;
							return 1;
					case(37): // or
							controlOut -> ALUsrc = 0;
							controlOut -> ALU.op = 1;
							controlOut -> ALU.bNegate = 0;
							controlOut -> memRead = 0;
							controlOut -> memWrite = 0;
							controlOut -> memToReg = 0;
							controlOut -> regDst = 1;
							controlOut -> regWrite = 1;
							controlOut -> branch = 0;
							controlOut -> jump = 0;
							return 1;
					case(38): // xor
							controlOut -> ALUsrc = 0;
							controlOut -> ALU.op = 4;
							controlOut -> ALU.bNegate = 0;
							controlOut -> memRead = 0;
							controlOut -> memWrite = 0;
							controlOut -> memToReg = 0;
							controlOut -> regDst = 1;
							controlOut -> regWrite = 1;
							controlOut -> branch = 0;
							controlOut -> jump = 0;
						return 1;
					case(42): // slt
							controlOut -> ALUsrc = 0;
							controlOut -> ALU.op = 3;
							controlOut -> ALU.bNegate = 1;
							controlOut -> memRead = 0;
							controlOut -> memWrite = 0;
							controlOut -> memToReg = 0;
							controlOut -> regDst = 1;
							controlOut -> regWrite = 1;
							controlOut -> branch = 0;
							controlOut -> jump = 0;
							return 1;
				}
		case(2): // j
				controlOut -> ALUsrc = 0;
				controlOut -> ALU.op = 0;
				controlOut -> ALU.bNegate = 0;
				controlOut -> memRead = 0;
				controlOut -> memWrite = 0;
				controlOut -> memToReg = 0;
				controlOut -> regDst = 0;
				controlOut -> regWrite = 0;
				controlOut -> branch = 0;
				controlOut -> jump = 1;
				return 1;
		case(4): // beq
				controlOut -> ALUsrc = 0;
				controlOut -> ALU.op = 2;
				controlOut -> ALU.bNegate = 1;
				controlOut -> memRead = 0;
				controlOut -> memWrite = 0;
				controlOut -> memToReg = 0;
				controlOut -> regDst = 0;
				controlOut -> regWrite = 0;
				controlOut -> branch = 1;
				controlOut -> jump = 0;
				return 1;
		case(8): // addi
				controlOut -> ALUsrc = 1;
				controlOut -> ALU.op = 2;
				controlOut -> ALU.bNegate = 0;
				controlOut -> memRead = 0;
				controlOut -> memWrite = 0;
				controlOut -> memToReg = 0;
				controlOut -> regDst = 0;
				controlOut -> regWrite = 1;
				controlOut -> branch = 0;
				controlOut -> jump = 0;
				return 1;
		case(9): // addiu
				controlOut -> ALUsrc = 1;
				controlOut -> ALU.op = 2;
				controlOut -> ALU.bNegate = 0;
				controlOut -> memRead = 0;
				controlOut -> memWrite = 0;
				controlOut -> memToReg = 0;
				controlOut -> regDst = 0;
				controlOut -> regWrite = 1;
				controlOut -> branch = 0;
				controlOut -> jump = 0;
				return 1;
		case(10): // slti
				controlOut -> ALUsrc = 1;
				controlOut -> ALU.op = 3;
				controlOut -> ALU.bNegate = 1;
				controlOut -> memRead = 0;
				controlOut -> memWrite = 0;
				controlOut -> memToReg = 0;
				controlOut -> regDst = 0;
				controlOut -> regWrite = 1;
				controlOut -> branch = 0;
				controlOut -> jump = 0;
				return 1;
		case(12): // andi
				controlOut -> ALUsrc = 1;
				controlOut -> ALU.op = 0;
				controlOut -> ALU.bNegate = 0;
				controlOut -> memRead = 0;
				controlOut -> memWrite = 0;
				controlOut -> memToReg = 0;
				controlOut -> regDst = 0;
				controlOut -> regWrite = 1;
				controlOut -> branch = 0;
				controlOut -> jump = 0;
				return 1;
		case(35): // lw
				controlOut -> ALUsrc = 1;
				controlOut -> ALU.op = 2;
				controlOut -> ALU.bNegate = 0;
				controlOut -> memRead = 1;
				controlOut -> memWrite = 0;
				controlOut -> memToReg = 1;
				controlOut -> regDst = 0;
				controlOut -> regWrite = 1;
				controlOut -> branch = 0;
				controlOut -> jump = 0;
				return 1;
		case(43): // sw
				controlOut -> ALUsrc = 1;
				controlOut -> ALU.op = 2;
				controlOut -> ALU.bNegate = 0;
				controlOut -> memRead = 0;
				controlOut -> memWrite = 1;
				controlOut -> memToReg = 0;
				controlOut -> regDst = 0;
				controlOut -> regWrite = 0;
				controlOut -> branch = 0;
				controlOut -> jump = 0;
				return 1;
	}
	return 0;
}

WORD getALUinput1(CPUControl *controlIn,
                  InstructionFields *fieldsIn,
                  WORD rsVal, WORD rtVal, WORD reg32, WORD reg33,
                  WORD oldPC) {
	return rsVal;
}

WORD getALUinput2(CPUControl *controlIn,
                  InstructionFields *fieldsIn,
                  WORD rsVal, WORD rtVal, WORD reg32, WORD reg33,
                  WORD oldPC) {
	switch(controlIn -> ALUsrc){
	// Check if it's an R or I instruction to give correct input to ALU 2
	case(0):
			return rtVal;
	}
	return fieldsIn -> imm32;
}

void execute_ALU(CPUControl *controlIn,
                 WORD input1, WORD input2,
                 ALUResult  *aluResultOut) {
	if(controlIn -> ALU.bNegate) {
		input2 = -input2;
	}
	switch(controlIn -> ALU.op) {
	case(0): // and
		aluResultOut -> result = input1 & input2;
		aluResultOut -> zero = 0;
		break;
	case(1): // or
		aluResultOut -> result = input1 | input2;
		aluResultOut -> zero = 0;
		break;
	case(2): // add
		aluResultOut -> result = input1 + input2;
		if(aluResultOut -> result == 0) {
			aluResultOut -> zero = 1;
		} else {
			aluResultOut -> zero = 0;
		}
		break;
	case(3): // slt
		aluResultOut -> result = input1 + input2;
		if(aluResultOut <= 0) {
			aluResultOut -> result = 1;
			aluResultOut -> zero = 0;
		} else {
			aluResultOut -> result = 0;
			aluResultOut -> zero = 1;
		}
		break;
	case(4): // xor
		aluResultOut -> result = input1 ^ input2;
		aluResultOut -> zero = 0;
	}
	if(aluResultOut -> zero == 1) {
		aluResultOut -> extra = 1;
	} else {
		aluResultOut -> extra = 0;
	}
}

void execute_MEM(CPUControl *controlIn,
                 ALUResult  *aluResultIn,
                 WORD        rsVal, WORD rtVal,
                 WORD       *memory,
                 MemResult  *resultOut) {
	if(controlIn -> memWrite == 1) {
		memory[aluResultIn -> result / 4] = rtVal;
		resultOut -> readVal = 0;
	} else if(controlIn -> memRead == 1) {
		resultOut -> readVal = memory[aluResultIn -> result / 4];
	}
}

WORD getNextPC(InstructionFields *fields, CPUControl *controlIn, int aluZero,
               WORD rsVal, WORD rtVal,
               WORD oldPC) {
	if(controlIn -> branch == 1) {
		return fields -> imm32 + oldPC;
	}
	if(controlIn -> jump == 1) { // take address, sll by 2, add top four PC
		return (fields -> address << 2) + (oldPC & 0xf0000000);
	}
	return oldPC + 4;
}

void execute_updateRegs(InstructionFields *fields, CPUControl *controlIn,
                        ALUResult  *aluResultIn, MemResult *memResultIn,
                        WORD       *regs) {

}
