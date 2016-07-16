/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.characters;

import java.math.BigInteger;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import jcl.numbers.IntegerStruct;
import jcl.packages.GlobalPackageStruct;
import jcl.symbols.ConstantStruct;

/**
 * Defines the standard character constants for the system.
 */
@SuppressWarnings("all")
public final class CharacterConstants {

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

	// CharacterStruct Standard-Char Constants

	public static final CharacterStruct EOF_CHAR = CharacterStruct.valueOf(EOF);

	public static final CharacterStruct NULL_CHAR = CharacterStruct.valueOf(NULL);

	public static final CharacterStruct START_OF_HEADER_CHAR = CharacterStruct.valueOf(START_OF_HEADER);

	public static final CharacterStruct START_OF_TEXT_CHAR = CharacterStruct.valueOf(START_OF_TEXT);

	public static final CharacterStruct END_OF_TEXT_CHAR = CharacterStruct.valueOf(END_OF_TEXT);

	public static final CharacterStruct END_OF_TRANSMISSION_CHAR = CharacterStruct.valueOf(END_OF_TRANSMISSION);

	public static final CharacterStruct ENQUIRY_CHAR = CharacterStruct.valueOf(ENQUIRY);

	public static final CharacterStruct ACKNOWLEDGE_CHAR = CharacterStruct.valueOf(ACKNOWLEDGE);

	public static final CharacterStruct BELL_CHAR = CharacterStruct.valueOf(BELL);

	public static final CharacterStruct BACKSPACE_CHAR = CharacterStruct.valueOf(BACKSPACE);

	public static final CharacterStruct TAB_CHAR = CharacterStruct.valueOf(TAB);

	public static final CharacterStruct LINE_FEED_CHAR = CharacterStruct.valueOf(LINE_FEED);

	public static final CharacterStruct NEWLINE_CHAR = CharacterStruct.valueOf(NEWLINE);

	public static final CharacterStruct VERTICAL_TAB_CHAR = CharacterStruct.valueOf(VERTICAL_TAB);

	public static final CharacterStruct FORM_FEED_CHAR = CharacterStruct.valueOf(FORM_FEED);

	public static final CharacterStruct PAGE_CHAR = CharacterStruct.valueOf(PAGE);

	public static final CharacterStruct CARRIAGE_RETURN_CHAR = CharacterStruct.valueOf(CARRIAGE_RETURN);

	public static final CharacterStruct RETURN_CHAR = CharacterStruct.valueOf(RETURN);

	public static final CharacterStruct SHIFT_OUT_CHAR = CharacterStruct.valueOf(SHIFT_OUT);

	public static final CharacterStruct SHIFT_IN_CHAR = CharacterStruct.valueOf(SHIFT_IN);

	public static final CharacterStruct DATA_LINK_ESCAPE_CHAR = CharacterStruct.valueOf(DATA_LINK_ESCAPE);

	public static final CharacterStruct DEVICE_CONTROL_1_CHAR = CharacterStruct.valueOf(DEVICE_CONTROL_1);

	public static final CharacterStruct DEVICE_CONTROL_2_CHAR = CharacterStruct.valueOf(DEVICE_CONTROL_2);

	public static final CharacterStruct DEVICE_CONTROL_3_CHAR = CharacterStruct.valueOf(DEVICE_CONTROL_3);

	public static final CharacterStruct DEVICE_CONTROL_4_CHAR = CharacterStruct.valueOf(DEVICE_CONTROL_4);

	public static final CharacterStruct NEGATIVE_ACKNOWLEDGE_CHAR = CharacterStruct.valueOf(NEGATIVE_ACKNOWLEDGE);

	public static final CharacterStruct SYNCHRONOUS_IDLE_CHAR = CharacterStruct.valueOf(SYNCHRONOUS_IDLE);

	public static final CharacterStruct END_OF_TRANSMISSION_BLOCK_CHAR = CharacterStruct.valueOf(END_OF_TRANSMISSION_BLOCK);

	public static final CharacterStruct CANCEL_CHAR = CharacterStruct.valueOf(CANCEL);

	public static final CharacterStruct END_OF_MEDIUM_CHAR = CharacterStruct.valueOf(END_OF_MEDIUM);

	public static final CharacterStruct SUBSTITUTE_CHAR = CharacterStruct.valueOf(SUBSTITUTE);

	public static final CharacterStruct ESCAPE_CHAR = CharacterStruct.valueOf(ESCAPE);

	public static final CharacterStruct FILE_SEPARATOR_CHAR = CharacterStruct.valueOf(FILE_SEPARATOR);

	public static final CharacterStruct GROUP_SEPARATOR_CHAR = CharacterStruct.valueOf(GROUP_SEPARATOR);

	public static final CharacterStruct RECORD_SEPARATOR_CHAR = CharacterStruct.valueOf(RECORD_SEPARATOR);

	public static final CharacterStruct UNIT_SEPARATOR_CHAR = CharacterStruct.valueOf(UNIT_SEPARATOR);

	public static final CharacterStruct SPACE_CHAR = CharacterStruct.valueOf(SPACE);

	public static final CharacterStruct EXCLAMATION_MARK_CHAR = CharacterStruct.valueOf(EXCLAMATION_MARK);

	public static final CharacterStruct QUOTATION_MARK_CHAR = CharacterStruct.valueOf(QUOTATION_MARK);

	public static final CharacterStruct NUMBER_SIGN_CHAR = CharacterStruct.valueOf(NUMBER_SIGN);

	public static final CharacterStruct DOLLAR_SIGN_CHAR = CharacterStruct.valueOf(DOLLAR_SIGN);

	public static final CharacterStruct PERCENT_SIGN_CHAR = CharacterStruct.valueOf(PERCENT_SIGN);

	public static final CharacterStruct AMPERSAND_CHAR = CharacterStruct.valueOf(AMPERSAND);

	public static final CharacterStruct APOSTROPHE_CHAR = CharacterStruct.valueOf(APOSTROPHE);

	public static final CharacterStruct LEFT_PARENTHESIS_CHAR = CharacterStruct.valueOf(LEFT_PARENTHESIS);

	public static final CharacterStruct RIGHT_PARENTHESIS_CHAR = CharacterStruct.valueOf(RIGHT_PARENTHESIS);

	public static final CharacterStruct ASTERISK_CHAR = CharacterStruct.valueOf(ASTERISK);

	public static final CharacterStruct PLUS_SIGN_CHAR = CharacterStruct.valueOf(PLUS_SIGN);

	public static final CharacterStruct COMMA_CHAR = CharacterStruct.valueOf(COMMA);

	public static final CharacterStruct HYPHEN_MINUS_CHAR = CharacterStruct.valueOf(HYPHEN_MINUS);

	public static final CharacterStruct FULL_STOP_CHAR = CharacterStruct.valueOf(FULL_STOP);

	public static final CharacterStruct SLASH_CHAR = CharacterStruct.valueOf(SLASH);

	public static final CharacterStruct DIGIT_ZERO_CHAR = CharacterStruct.valueOf(DIGIT_ZERO);

	public static final CharacterStruct DIGIT_ONE_CHAR = CharacterStruct.valueOf(DIGIT_ONE);

	public static final CharacterStruct DIGIT_TWO_CHAR = CharacterStruct.valueOf(DIGIT_TWO);

	public static final CharacterStruct DIGIT_THREE_CHAR = CharacterStruct.valueOf(DIGIT_THREE);

	public static final CharacterStruct DIGIT_FOUR_CHAR = CharacterStruct.valueOf(DIGIT_FOUR);

	public static final CharacterStruct DIGIT_FIVE_CHAR = CharacterStruct.valueOf(DIGIT_FIVE);

	public static final CharacterStruct DIGIT_SIX_CHAR = CharacterStruct.valueOf(DIGIT_SIX);

	public static final CharacterStruct DIGIT_SEVEN_CHAR = CharacterStruct.valueOf(DIGIT_SEVEN);

	public static final CharacterStruct DIGIT_EIGHT_CHAR = CharacterStruct.valueOf(DIGIT_EIGHT);

	public static final CharacterStruct DIGIT_NINE_CHAR = CharacterStruct.valueOf(DIGIT_NINE);

	public static final CharacterStruct COLON_CHAR = CharacterStruct.valueOf(COLON);

	public static final CharacterStruct SEMICOLON_CHAR = CharacterStruct.valueOf(SEMICOLON);

	public static final CharacterStruct LESS_THAN_SIGN_CHAR = CharacterStruct.valueOf(LESS_THAN_SIGN);

	public static final CharacterStruct EQUALS_SIGN_CHAR = CharacterStruct.valueOf(EQUALS_SIGN);

	public static final CharacterStruct GREATER_THAN_SIGN_CHAR = CharacterStruct.valueOf(GREATER_THAN_SIGN);

	public static final CharacterStruct QUESTION_MARK_CHAR = CharacterStruct.valueOf(QUESTION_MARK);

	public static final CharacterStruct AT_SIGN_CHAR = CharacterStruct.valueOf(AT_SIGN);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_A_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_A);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_B_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_B);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_C_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_C);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_D_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_D);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_E_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_E);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_F_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_F);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_G_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_G);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_H_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_H);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_I_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_I);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_J_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_J);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_K_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_K);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_L_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_L);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_M_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_M);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_N_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_N);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_O_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_O);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_P_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_P);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_Q_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_Q);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_R_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_R);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_S_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_S);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_T_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_T);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_U_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_U);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_V_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_V);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_W_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_W);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_X_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_X);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_Y_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_Y);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_Z_CHAR = CharacterStruct.valueOf(LATIN_CAPITAL_LETTER_Z);

	public static final CharacterStruct LEFT_SQUARE_BRACKET_CHAR = CharacterStruct.valueOf(LEFT_SQUARE_BRACKET);

	public static final CharacterStruct BACKSLASH_CHAR = CharacterStruct.valueOf(BACKSLASH);

	public static final CharacterStruct RIGHT_SQUARE_BRACKET_CHAR = CharacterStruct.valueOf(RIGHT_SQUARE_BRACKET);

	public static final CharacterStruct CIRCUMFLEX_ACCENT_CHAR = CharacterStruct.valueOf(CIRCUMFLEX_ACCENT);

	public static final CharacterStruct LOW_LINE_CHAR = CharacterStruct.valueOf(LOW_LINE);

	public static final CharacterStruct GRAVE_ACCENT_CHAR = CharacterStruct.valueOf(GRAVE_ACCENT);

	public static final CharacterStruct LATIN_SMALL_LETTER_A_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_A);

	public static final CharacterStruct LATIN_SMALL_LETTER_B_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_B);

	public static final CharacterStruct LATIN_SMALL_LETTER_C_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_C);

	public static final CharacterStruct LATIN_SMALL_LETTER_D_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_D);

	public static final CharacterStruct LATIN_SMALL_LETTER_E_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_E);

	public static final CharacterStruct LATIN_SMALL_LETTER_F_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_F);

	public static final CharacterStruct LATIN_SMALL_LETTER_G_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_G);

	public static final CharacterStruct LATIN_SMALL_LETTER_H_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_H);

	public static final CharacterStruct LATIN_SMALL_LETTER_I_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_I);

	public static final CharacterStruct LATIN_SMALL_LETTER_J_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_J);

	public static final CharacterStruct LATIN_SMALL_LETTER_K_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_K);

	public static final CharacterStruct LATIN_SMALL_LETTER_L_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_L);

	public static final CharacterStruct LATIN_SMALL_LETTER_M_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_M);

	public static final CharacterStruct LATIN_SMALL_LETTER_N_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_N);

	public static final CharacterStruct LATIN_SMALL_LETTER_O_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_O);

	public static final CharacterStruct LATIN_SMALL_LETTER_P_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_P);

	public static final CharacterStruct LATIN_SMALL_LETTER_Q_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_Q);

	public static final CharacterStruct LATIN_SMALL_LETTER_R_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_R);

	public static final CharacterStruct LATIN_SMALL_LETTER_S_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_S);

	public static final CharacterStruct LATIN_SMALL_LETTER_T_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_T);

	public static final CharacterStruct LATIN_SMALL_LETTER_U_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_U);

	public static final CharacterStruct LATIN_SMALL_LETTER_V_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_V);

	public static final CharacterStruct LATIN_SMALL_LETTER_W_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_W);

	public static final CharacterStruct LATIN_SMALL_LETTER_X_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_X);

	public static final CharacterStruct LATIN_SMALL_LETTER_Y_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_Y);

	public static final CharacterStruct LATIN_SMALL_LETTER_Z_CHAR = CharacterStruct.valueOf(LATIN_SMALL_LETTER_Z);

	public static final CharacterStruct LEFT_CURLY_BRACKET_CHAR = CharacterStruct.valueOf(LEFT_CURLY_BRACKET);

	public static final CharacterStruct VERTICAL_LINE_CHAR = CharacterStruct.valueOf(VERTICAL_LINE);

	public static final CharacterStruct RIGHT_CURLY_BRACKET_CHAR = CharacterStruct.valueOf(RIGHT_CURLY_BRACKET);

	public static final CharacterStruct TILDE_CHAR = CharacterStruct.valueOf(TILDE);

	public static final CharacterStruct DELETE_CHAR = CharacterStruct.valueOf(DELETE);

	public static final CharacterStruct RUBOUT_CHAR = CharacterStruct.valueOf(RUBOUT);

	public static final CharacterStruct EXIT_CHAR_CHAR = CharacterStruct.valueOf(EXIT_CHAR);

	// Standard-Char Map Constant

	public static final Map<Integer, CharacterStruct> STANDARD_CHAR_MAP;

	static {
		final Map<Integer, CharacterStruct> tempStandardCharMap = new HashMap<>();
		tempStandardCharMap.put(EOF, EOF_CHAR);
		tempStandardCharMap.put((int) NULL, NULL_CHAR);
		tempStandardCharMap.put((int) START_OF_HEADER, START_OF_HEADER_CHAR);
		tempStandardCharMap.put((int) START_OF_TEXT, START_OF_TEXT_CHAR);
		tempStandardCharMap.put((int) END_OF_TEXT, END_OF_TEXT_CHAR);
		tempStandardCharMap.put((int) END_OF_TRANSMISSION, END_OF_TRANSMISSION_CHAR);
		tempStandardCharMap.put((int) ENQUIRY, ENQUIRY_CHAR);
		tempStandardCharMap.put((int) ACKNOWLEDGE, ACKNOWLEDGE_CHAR);
		tempStandardCharMap.put((int) BELL, BELL_CHAR);
		tempStandardCharMap.put((int) BACKSPACE, BACKSPACE_CHAR);
		tempStandardCharMap.put((int) TAB, TAB_CHAR);
		tempStandardCharMap.put((int) LINE_FEED, LINE_FEED_CHAR);
		tempStandardCharMap.put((int) NEWLINE, NEWLINE_CHAR);
		tempStandardCharMap.put((int) VERTICAL_TAB, VERTICAL_TAB_CHAR);
		tempStandardCharMap.put((int) FORM_FEED, FORM_FEED_CHAR);
		tempStandardCharMap.put((int) PAGE, PAGE_CHAR);
		tempStandardCharMap.put((int) CARRIAGE_RETURN, CARRIAGE_RETURN_CHAR);
		tempStandardCharMap.put((int) RETURN, RETURN_CHAR);
		tempStandardCharMap.put((int) SHIFT_OUT, SHIFT_OUT_CHAR);
		tempStandardCharMap.put((int) SHIFT_IN, SHIFT_IN_CHAR);
		tempStandardCharMap.put((int) DATA_LINK_ESCAPE, DATA_LINK_ESCAPE_CHAR);
		tempStandardCharMap.put((int) DEVICE_CONTROL_1, DEVICE_CONTROL_1_CHAR);
		tempStandardCharMap.put((int) DEVICE_CONTROL_2, DEVICE_CONTROL_2_CHAR);
		tempStandardCharMap.put((int) DEVICE_CONTROL_3, DEVICE_CONTROL_3_CHAR);
		tempStandardCharMap.put((int) DEVICE_CONTROL_4, DEVICE_CONTROL_4_CHAR);
		tempStandardCharMap.put((int) NEGATIVE_ACKNOWLEDGE, NEGATIVE_ACKNOWLEDGE_CHAR);
		tempStandardCharMap.put((int) SYNCHRONOUS_IDLE, SYNCHRONOUS_IDLE_CHAR);
		tempStandardCharMap.put((int) END_OF_TRANSMISSION_BLOCK, END_OF_TRANSMISSION_BLOCK_CHAR);
		tempStandardCharMap.put((int) CANCEL, CANCEL_CHAR);
		tempStandardCharMap.put((int) END_OF_MEDIUM, END_OF_MEDIUM_CHAR);
		tempStandardCharMap.put((int) SUBSTITUTE, SUBSTITUTE_CHAR);
		tempStandardCharMap.put((int) ESCAPE, ESCAPE_CHAR);
		tempStandardCharMap.put((int) FILE_SEPARATOR, FILE_SEPARATOR_CHAR);
		tempStandardCharMap.put((int) GROUP_SEPARATOR, GROUP_SEPARATOR_CHAR);
		tempStandardCharMap.put((int) RECORD_SEPARATOR, RECORD_SEPARATOR_CHAR);
		tempStandardCharMap.put((int) UNIT_SEPARATOR, UNIT_SEPARATOR_CHAR);
		tempStandardCharMap.put((int) SPACE, SPACE_CHAR);
		tempStandardCharMap.put((int) EXCLAMATION_MARK, EXCLAMATION_MARK_CHAR);
		tempStandardCharMap.put((int) QUOTATION_MARK, QUOTATION_MARK_CHAR);
		tempStandardCharMap.put((int) NUMBER_SIGN, NUMBER_SIGN_CHAR);
		tempStandardCharMap.put((int) DOLLAR_SIGN, DOLLAR_SIGN_CHAR);
		tempStandardCharMap.put((int) PERCENT_SIGN, PERCENT_SIGN_CHAR);
		tempStandardCharMap.put((int) AMPERSAND, AMPERSAND_CHAR);
		tempStandardCharMap.put((int) APOSTROPHE, APOSTROPHE_CHAR);
		tempStandardCharMap.put((int) LEFT_PARENTHESIS, LEFT_PARENTHESIS_CHAR);
		tempStandardCharMap.put((int) RIGHT_PARENTHESIS, RIGHT_PARENTHESIS_CHAR);
		tempStandardCharMap.put((int) ASTERISK, ASTERISK_CHAR);
		tempStandardCharMap.put((int) PLUS_SIGN, PLUS_SIGN_CHAR);
		tempStandardCharMap.put((int) COMMA, COMMA_CHAR);
		tempStandardCharMap.put((int) HYPHEN_MINUS, HYPHEN_MINUS_CHAR);
		tempStandardCharMap.put((int) FULL_STOP, FULL_STOP_CHAR);
		tempStandardCharMap.put((int) SLASH, SLASH_CHAR);
		tempStandardCharMap.put((int) DIGIT_ZERO, DIGIT_ZERO_CHAR);
		tempStandardCharMap.put((int) DIGIT_ONE, DIGIT_ONE_CHAR);
		tempStandardCharMap.put((int) DIGIT_TWO, DIGIT_TWO_CHAR);
		tempStandardCharMap.put((int) DIGIT_THREE, DIGIT_THREE_CHAR);
		tempStandardCharMap.put((int) DIGIT_FOUR, DIGIT_FOUR_CHAR);
		tempStandardCharMap.put((int) DIGIT_FIVE, DIGIT_FIVE_CHAR);
		tempStandardCharMap.put((int) DIGIT_SIX, DIGIT_SIX_CHAR);
		tempStandardCharMap.put((int) DIGIT_SEVEN, DIGIT_SEVEN_CHAR);
		tempStandardCharMap.put((int) DIGIT_EIGHT, DIGIT_EIGHT_CHAR);
		tempStandardCharMap.put((int) DIGIT_NINE, DIGIT_NINE_CHAR);
		tempStandardCharMap.put((int) COLON, COLON_CHAR);
		tempStandardCharMap.put((int) SEMICOLON, SEMICOLON_CHAR);
		tempStandardCharMap.put((int) LESS_THAN_SIGN, LESS_THAN_SIGN_CHAR);
		tempStandardCharMap.put((int) EQUALS_SIGN, EQUALS_SIGN_CHAR);
		tempStandardCharMap.put((int) GREATER_THAN_SIGN, GREATER_THAN_SIGN_CHAR);
		tempStandardCharMap.put((int) QUESTION_MARK, QUESTION_MARK_CHAR);
		tempStandardCharMap.put((int) AT_SIGN, AT_SIGN_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_A, LATIN_CAPITAL_LETTER_A_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_B, LATIN_CAPITAL_LETTER_B_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_C, LATIN_CAPITAL_LETTER_C_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_D, LATIN_CAPITAL_LETTER_D_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_E, LATIN_CAPITAL_LETTER_E_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_F, LATIN_CAPITAL_LETTER_F_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_G, LATIN_CAPITAL_LETTER_G_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_H, LATIN_CAPITAL_LETTER_H_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_I, LATIN_CAPITAL_LETTER_I_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_J, LATIN_CAPITAL_LETTER_J_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_K, LATIN_CAPITAL_LETTER_K_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_L, LATIN_CAPITAL_LETTER_L_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_M, LATIN_CAPITAL_LETTER_M_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_N, LATIN_CAPITAL_LETTER_N_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_O, LATIN_CAPITAL_LETTER_O_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_P, LATIN_CAPITAL_LETTER_P_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_Q, LATIN_CAPITAL_LETTER_Q_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_R, LATIN_CAPITAL_LETTER_R_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_S, LATIN_CAPITAL_LETTER_S_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_T, LATIN_CAPITAL_LETTER_T_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_U, LATIN_CAPITAL_LETTER_U_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_V, LATIN_CAPITAL_LETTER_V_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_W, LATIN_CAPITAL_LETTER_W_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_X, LATIN_CAPITAL_LETTER_X_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_Y, LATIN_CAPITAL_LETTER_Y_CHAR);
		tempStandardCharMap.put((int) LATIN_CAPITAL_LETTER_Z, LATIN_CAPITAL_LETTER_Z_CHAR);
		tempStandardCharMap.put((int) LEFT_SQUARE_BRACKET, LEFT_SQUARE_BRACKET_CHAR);
		tempStandardCharMap.put((int) BACKSLASH, BACKSLASH_CHAR);
		tempStandardCharMap.put((int) RIGHT_SQUARE_BRACKET, RIGHT_SQUARE_BRACKET_CHAR);
		tempStandardCharMap.put((int) CIRCUMFLEX_ACCENT, CIRCUMFLEX_ACCENT_CHAR);
		tempStandardCharMap.put((int) LOW_LINE, LOW_LINE_CHAR);
		tempStandardCharMap.put((int) GRAVE_ACCENT, GRAVE_ACCENT_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_A, LATIN_SMALL_LETTER_A_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_B, LATIN_SMALL_LETTER_B_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_C, LATIN_SMALL_LETTER_C_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_D, LATIN_SMALL_LETTER_D_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_E, LATIN_SMALL_LETTER_E_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_F, LATIN_SMALL_LETTER_F_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_G, LATIN_SMALL_LETTER_G_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_H, LATIN_SMALL_LETTER_H_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_I, LATIN_SMALL_LETTER_I_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_J, LATIN_SMALL_LETTER_J_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_K, LATIN_SMALL_LETTER_K_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_L, LATIN_SMALL_LETTER_L_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_M, LATIN_SMALL_LETTER_M_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_N, LATIN_SMALL_LETTER_N_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_O, LATIN_SMALL_LETTER_O_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_P, LATIN_SMALL_LETTER_P_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_Q, LATIN_SMALL_LETTER_Q_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_R, LATIN_SMALL_LETTER_R_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_S, LATIN_SMALL_LETTER_S_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_T, LATIN_SMALL_LETTER_T_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_U, LATIN_SMALL_LETTER_U_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_V, LATIN_SMALL_LETTER_V_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_W, LATIN_SMALL_LETTER_W_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_X, LATIN_SMALL_LETTER_X_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_Y, LATIN_SMALL_LETTER_Y_CHAR);
		tempStandardCharMap.put((int) LATIN_SMALL_LETTER_Z, LATIN_SMALL_LETTER_Z_CHAR);
		tempStandardCharMap.put((int) LEFT_CURLY_BRACKET, LEFT_CURLY_BRACKET_CHAR);
		tempStandardCharMap.put((int) VERTICAL_LINE, VERTICAL_LINE_CHAR);
		tempStandardCharMap.put((int) RIGHT_CURLY_BRACKET, RIGHT_CURLY_BRACKET_CHAR);
		tempStandardCharMap.put((int) TILDE, TILDE_CHAR);
		tempStandardCharMap.put((int) DELETE, DELETE_CHAR);
		tempStandardCharMap.put((int) RUBOUT, RUBOUT_CHAR);
		tempStandardCharMap.put(EXIT_CHAR, EXIT_CHAR_CHAR);

		STANDARD_CHAR_MAP = Collections.unmodifiableMap(tempStandardCharMap);
	}

	// Char-Code-Limit Constant

	public static final ConstantStruct<IntegerStruct> CHAR_CODE_LIMIT = new ConstantStruct<>("CHAR-CODE-LIMIT", GlobalPackageStruct.COMMON_LISP, IntegerStruct.valueOf(BigInteger.valueOf(Character.MAX_VALUE)));

	private CharacterConstants() {
	}
}
