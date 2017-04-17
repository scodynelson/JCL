/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.util;

import lombok.experimental.UtilityClass;

/**
 * Defines the standard character constants for the system.
 */
@SuppressWarnings("all")
@UtilityClass
public class CodePointConstants {

	// Integer/Character Standard-Char Constants

	public static final Integer EOF = -1;

	public static final Character NULL = 0;

	public static final Character START_OF_HEADER = 1;

	public static final Character START_OF_TEXT = 2;

	public static final Character END_OF_TEXT = 3;

	public static final Character END_OF_TRANSMISSION = 4;

	public static final Character ENQUIRY = 5;

	public static final Character ACKNOWLEDGE = 6;

	public static final Character BELL = 7;

	public static final Character BACKSPACE = 8;

	public static final Character TAB = 9;

	public static final Character LINE_FEED = 10;

	public static final Character NEWLINE = 10;

	public static final Character VERTICAL_TAB = 11;

	public static final Character FORM_FEED = 12;

	public static final Character PAGE = 12;

	public static final Character CARRIAGE_RETURN = 13;

	public static final Character RETURN = 13;

	public static final Character SHIFT_OUT = 14;

	public static final Character SHIFT_IN = 15;

	public static final Character DATA_LINK_ESCAPE = 16;

	public static final Character DEVICE_CONTROL_1 = 17;

	public static final Character DEVICE_CONTROL_2 = 18;

	public static final Character DEVICE_CONTROL_3 = 19;

	public static final Character DEVICE_CONTROL_4 = 20;

	public static final Character NEGATIVE_ACKNOWLEDGE = 21;

	public static final Character SYNCHRONOUS_IDLE = 22;

	public static final Character END_OF_TRANSMISSION_BLOCK = 23;

	public static final Character CANCEL = 24;

	public static final Character END_OF_MEDIUM = 25;

	public static final Character SUBSTITUTE = 26;

	public static final Character ESCAPE = 27;

	public static final Character FILE_SEPARATOR = 28;

	public static final Character GROUP_SEPARATOR = 29;

	public static final Character RECORD_SEPARATOR = 30;

	public static final Character UNIT_SEPARATOR = 31;

	public static final Character SPACE = 32;

	public static final Character EXCLAMATION_MARK = 33;

	public static final Character QUOTATION_MARK = 34;

	public static final Character NUMBER_SIGN = 35;

	public static final Character DOLLAR_SIGN = 36;

	public static final Character PERCENT_SIGN = 37;

	public static final Character AMPERSAND = 38;

	public static final Character APOSTROPHE = 39;

	public static final Character LEFT_PARENTHESIS = 40;

	public static final Character RIGHT_PARENTHESIS = 41;

	public static final Character ASTERISK = 42;

	public static final Character PLUS_SIGN = 43;

	public static final Character COMMA = 44;

	public static final Character HYPHEN_MINUS = 45;

	public static final Character FULL_STOP = 46;

	public static final Character SLASH = 47;

	public static final Character DIGIT_ZERO = 48;

	public static final Character DIGIT_ONE = 49;

	public static final Character DIGIT_TWO = 50;

	public static final Character DIGIT_THREE = 51;

	public static final Character DIGIT_FOUR = 52;

	public static final Character DIGIT_FIVE = 53;

	public static final Character DIGIT_SIX = 54;

	public static final Character DIGIT_SEVEN = 55;

	public static final Character DIGIT_EIGHT = 56;

	public static final Character DIGIT_NINE = 57;

	public static final Character COLON = 58;

	public static final Character SEMICOLON = 59;

	public static final Character LESS_THAN_SIGN = 60;

	public static final Character EQUALS_SIGN = 61;

	public static final Character GREATER_THAN_SIGN = 62;

	public static final Character QUESTION_MARK = 63;

	public static final Character AT_SIGN = 64;

	public static final Character LATIN_CAPITAL_LETTER_A = 65;

	public static final Character LATIN_CAPITAL_LETTER_B = 66;

	public static final Character LATIN_CAPITAL_LETTER_C = 67;

	public static final Character LATIN_CAPITAL_LETTER_D = 68;

	public static final Character LATIN_CAPITAL_LETTER_E = 69;

	public static final Character LATIN_CAPITAL_LETTER_F = 70;

	public static final Character LATIN_CAPITAL_LETTER_G = 71;

	public static final Character LATIN_CAPITAL_LETTER_H = 72;

	public static final Character LATIN_CAPITAL_LETTER_I = 73;

	public static final Character LATIN_CAPITAL_LETTER_J = 74;

	public static final Character LATIN_CAPITAL_LETTER_K = 75;

	public static final Character LATIN_CAPITAL_LETTER_L = 76;

	public static final Character LATIN_CAPITAL_LETTER_M = 77;

	public static final Character LATIN_CAPITAL_LETTER_N = 78;

	public static final Character LATIN_CAPITAL_LETTER_O = 79;

	public static final Character LATIN_CAPITAL_LETTER_P = 80;

	public static final Character LATIN_CAPITAL_LETTER_Q = 81;

	public static final Character LATIN_CAPITAL_LETTER_R = 82;

	public static final Character LATIN_CAPITAL_LETTER_S = 83;

	public static final Character LATIN_CAPITAL_LETTER_T = 84;

	public static final Character LATIN_CAPITAL_LETTER_U = 85;

	public static final Character LATIN_CAPITAL_LETTER_V = 86;

	public static final Character LATIN_CAPITAL_LETTER_W = 87;

	public static final Character LATIN_CAPITAL_LETTER_X = 88;

	public static final Character LATIN_CAPITAL_LETTER_Y = 89;

	public static final Character LATIN_CAPITAL_LETTER_Z = 90;

	public static final Character LEFT_SQUARE_BRACKET = 91;

	public static final Character BACKSLASH = 92;

	public static final Character RIGHT_SQUARE_BRACKET = 93;

	public static final Character CIRCUMFLEX_ACCENT = 94;

	public static final Character LOW_LINE = 95;

	public static final Character GRAVE_ACCENT = 96;

	public static final Character LATIN_SMALL_LETTER_A = 97;

	public static final Character LATIN_SMALL_LETTER_B = 98;

	public static final Character LATIN_SMALL_LETTER_C = 99;

	public static final Character LATIN_SMALL_LETTER_D = 100;

	public static final Character LATIN_SMALL_LETTER_E = 101;

	public static final Character LATIN_SMALL_LETTER_F = 102;

	public static final Character LATIN_SMALL_LETTER_G = 103;

	public static final Character LATIN_SMALL_LETTER_H = 104;

	public static final Character LATIN_SMALL_LETTER_I = 105;

	public static final Character LATIN_SMALL_LETTER_J = 106;

	public static final Character LATIN_SMALL_LETTER_K = 107;

	public static final Character LATIN_SMALL_LETTER_L = 108;

	public static final Character LATIN_SMALL_LETTER_M = 109;

	public static final Character LATIN_SMALL_LETTER_N = 110;

	public static final Character LATIN_SMALL_LETTER_O = 111;

	public static final Character LATIN_SMALL_LETTER_P = 112;

	public static final Character LATIN_SMALL_LETTER_Q = 113;

	public static final Character LATIN_SMALL_LETTER_R = 114;

	public static final Character LATIN_SMALL_LETTER_S = 115;

	public static final Character LATIN_SMALL_LETTER_T = 116;

	public static final Character LATIN_SMALL_LETTER_U = 117;

	public static final Character LATIN_SMALL_LETTER_V = 118;

	public static final Character LATIN_SMALL_LETTER_W = 119;

	public static final Character LATIN_SMALL_LETTER_X = 120;

	public static final Character LATIN_SMALL_LETTER_Y = 121;

	public static final Character LATIN_SMALL_LETTER_Z = 122;

	public static final Character LEFT_CURLY_BRACKET = 123;

	public static final Character VERTICAL_LINE = 124;

	public static final Character RIGHT_CURLY_BRACKET = 125;

	public static final Character TILDE = 126;

	public static final Character DELETE = 127;

	public static final Character RUBOUT = 127;

	public static final Integer EXIT_CHAR = 0xFFFF;
}
