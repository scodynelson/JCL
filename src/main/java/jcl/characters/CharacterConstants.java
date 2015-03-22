/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters;

import java.math.BigInteger;

import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.Constant;

/**
 * Defines the standard character constants for the system.
 */
@SuppressWarnings("all")
public interface CharacterConstants {

	int EOF = -1;

	Character NULL = 0;
	Character START_OF_HEADER = 1;
	Character START_OF_TEXT = 2;
	Character END_OF_TEXT = 3;
	Character END_OF_TRANSMISSION = 4;
	Character ENQUIRY = 5;
	Character ACKNOWLEDGE = 6;
	Character BELL = 7;
	Character BACKSPACE = 8;
	Character TAB = 9;
	Character LINE_FEED = 10;
	Character NEWLINE = 10;
	Character VERTICAL_TAB = 11;
	Character FORM_FEED = 12;
	Character PAGE = 12;
	Character CARRIAGE_RETURN = 13;
	Character RETURN = 13;
	Character SHIFT_OUT = 14;
	Character SHIFT_IN = 15;
	Character DATA_LINK_ESCAPE = 16;
	Character DEVICE_CONTROL_1 = 17;
	Character DEVICE_CONTROL_2 = 18;
	Character DEVICE_CONTROL_3 = 19;
	Character DEVICE_CONTROL_4 = 20;
	Character NEGATIVE_ACKNOWLEDGE = 21;
	Character SYNCHRONOUS_IDLE = 22;
	Character END_OF_TRANSMISSION_BLOCK = 23;
	Character CANCEL = 24;
	Character END_OF_MEDIUM = 25;
	Character SUBSTITUTE = 26;
	Character ESCAPE = 27;
	Character FILE_SEPARATOR = 28;
	Character GROUP_SEPARATOR = 29;
	Character RECORD_SEPARATOR = 30;
	Character UNIT_SEPARATOR = 31;
	Character SPACE = 32;
	Character EXCLAMATION_MARK = 33;
	Character QUOTATION_MARK = 34;
	Character NUMBER_SIGN = 35;
	Character DOLLAR_SIGN = 36;
	Character PERCENT_SIGN = 37;
	Character AMPERSAND = 38;
	Character APOSTROPHE = 39;
	Character LEFT_PARENTHESIS = 40;
	Character RIGHT_PARENTHESIS = 41;
	Character ASTERISK = 42;
	Character PLUS_SIGN = 43;
	Character COMMA = 44;
	Character HYPHEN_MINUS = 45;
	Character FULL_STOP = 46;
	Character SLASH = 47;
	Character DIGIT_ZERO = 48;
	Character DIGIT_ONE = 49;
	Character DIGIT_TWO = 50;
	Character DIGIT_THREE = 51;
	Character DIGIT_FOUR = 52;
	Character DIGIT_FIVE = 53;
	Character DIGIT_SIX = 54;
	Character DIGIT_SEVEN = 55;
	Character DIGIT_EIGHT = 56;
	Character DIGIT_NINE = 57;
	Character COLON = 58;
	Character SEMICOLON = 59;
	Character LESS_THAN_SIGN = 60;
	Character EQUALS_SIGN = 61;
	Character GREATER_THAN_SIGN = 62;
	Character QUESTION_MARK = 63;
	Character AT_SIGN = 64;
	Character LATIN_CAPITAL_LETTER_A = 65;
	Character LATIN_CAPITAL_LETTER_B = 66;
	Character LATIN_CAPITAL_LETTER_C = 67;
	Character LATIN_CAPITAL_LETTER_D = 68;
	Character LATIN_CAPITAL_LETTER_E = 69;
	Character LATIN_CAPITAL_LETTER_F = 70;
	Character LATIN_CAPITAL_LETTER_G = 71;
	Character LATIN_CAPITAL_LETTER_H = 72;
	Character LATIN_CAPITAL_LETTER_I = 73;
	Character LATIN_CAPITAL_LETTER_J = 74;
	Character LATIN_CAPITAL_LETTER_K = 75;
	Character LATIN_CAPITAL_LETTER_L = 76;
	Character LATIN_CAPITAL_LETTER_M = 77;
	Character LATIN_CAPITAL_LETTER_N = 78;
	Character LATIN_CAPITAL_LETTER_O = 79;
	Character LATIN_CAPITAL_LETTER_P = 80;
	Character LATIN_CAPITAL_LETTER_Q = 81;
	Character LATIN_CAPITAL_LETTER_R = 82;
	Character LATIN_CAPITAL_LETTER_S = 83;
	Character LATIN_CAPITAL_LETTER_T = 84;
	Character LATIN_CAPITAL_LETTER_U = 85;
	Character LATIN_CAPITAL_LETTER_V = 86;
	Character LATIN_CAPITAL_LETTER_W = 87;
	Character LATIN_CAPITAL_LETTER_X = 88;
	Character LATIN_CAPITAL_LETTER_Y = 89;
	Character LATIN_CAPITAL_LETTER_Z = 90;
	Character LEFT_SQUARE_BRACKET = 91;
	Character BACKSLASH = 92;
	Character RIGHT_SQUARE_BRACKET = 93;
	Character CIRCUMFLEX_ACCENT = 94;
	Character LOW_LINE = 95;
	Character GRAVE_ACCENT = 96;
	Character LATIN_SMALL_LETTER_A = 97;
	Character LATIN_SMALL_LETTER_B = 98;
	Character LATIN_SMALL_LETTER_C = 99;
	Character LATIN_SMALL_LETTER_D = 100;
	Character LATIN_SMALL_LETTER_E = 101;
	Character LATIN_SMALL_LETTER_F = 102;
	Character LATIN_SMALL_LETTER_G = 103;
	Character LATIN_SMALL_LETTER_H = 104;
	Character LATIN_SMALL_LETTER_I = 105;
	Character LATIN_SMALL_LETTER_J = 106;
	Character LATIN_SMALL_LETTER_K = 107;
	Character LATIN_SMALL_LETTER_L = 108;
	Character LATIN_SMALL_LETTER_M = 109;
	Character LATIN_SMALL_LETTER_N = 110;
	Character LATIN_SMALL_LETTER_O = 111;
	Character LATIN_SMALL_LETTER_P = 112;
	Character LATIN_SMALL_LETTER_Q = 113;
	Character LATIN_SMALL_LETTER_R = 114;
	Character LATIN_SMALL_LETTER_S = 115;
	Character LATIN_SMALL_LETTER_T = 116;
	Character LATIN_SMALL_LETTER_U = 117;
	Character LATIN_SMALL_LETTER_V = 118;
	Character LATIN_SMALL_LETTER_W = 119;
	Character LATIN_SMALL_LETTER_X = 120;
	Character LATIN_SMALL_LETTER_Y = 121;
	Character LATIN_SMALL_LETTER_Z = 122;
	Character LEFT_CURLY_BRACKET = 123;
	Character VERTICAL_LINE = 124;
	Character RIGHT_CURLY_BRACKET = 125;
	Character TILDE = 126;
	Character DELETE = 127;
	Character RUBOUT = 127;

	int EXIT_CHAR = 0xFFFF;

	Constant<IntegerStruct> CHAR_CODE_LIMIT = new Constant<>("CHAR-CODE-LIMIT", GlobalPackageStruct.COMMON_LISP, new IntegerStruct(BigInteger.valueOf(Character.MAX_VALUE)));
}
