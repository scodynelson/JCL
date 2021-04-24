/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.statics;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import jcl.lang.CharacterStruct;
import jcl.util.CodePointConstants;
import lombok.experimental.UtilityClass;

/**
 * Defines the standard character constants for the system.
 */
@SuppressWarnings("all")
@UtilityClass
public class CharacterConstants {

	// CharacterStruct Standard-Char Constants

	public static final CharacterStruct EOF_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.EOF);

	public static final CharacterStruct NULL_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.NULL);

	public static final CharacterStruct START_OF_HEADER_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.START_OF_HEADER);

	public static final CharacterStruct START_OF_TEXT_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.START_OF_TEXT);

	public static final CharacterStruct END_OF_TEXT_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.END_OF_TEXT);

	public static final CharacterStruct END_OF_TRANSMISSION_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.END_OF_TRANSMISSION);

	public static final CharacterStruct ENQUIRY_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.ENQUIRY);

	public static final CharacterStruct ACKNOWLEDGE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.ACKNOWLEDGE);

	public static final CharacterStruct BELL_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.BELL);

	public static final CharacterStruct BACKSPACE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.BACKSPACE);

	public static final CharacterStruct TAB_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.TAB);

	public static final CharacterStruct LINE_FEED_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LINE_FEED);

	public static final CharacterStruct NEWLINE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.NEWLINE);

	public static final CharacterStruct VERTICAL_TAB_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.VERTICAL_TAB);

	public static final CharacterStruct FORM_FEED_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.FORM_FEED);

	public static final CharacterStruct PAGE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.PAGE);

	public static final CharacterStruct CARRIAGE_RETURN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.CARRIAGE_RETURN);

	public static final CharacterStruct RETURN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.RETURN);

	public static final CharacterStruct SHIFT_OUT_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.SHIFT_OUT);

	public static final CharacterStruct SHIFT_IN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.SHIFT_IN);

	public static final CharacterStruct DATA_LINK_ESCAPE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DATA_LINK_ESCAPE);

	public static final CharacterStruct DEVICE_CONTROL_1_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DEVICE_CONTROL_1);

	public static final CharacterStruct DEVICE_CONTROL_2_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DEVICE_CONTROL_2);

	public static final CharacterStruct DEVICE_CONTROL_3_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DEVICE_CONTROL_3);

	public static final CharacterStruct DEVICE_CONTROL_4_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DEVICE_CONTROL_4);

	public static final CharacterStruct NEGATIVE_ACKNOWLEDGE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.NEGATIVE_ACKNOWLEDGE);

	public static final CharacterStruct SYNCHRONOUS_IDLE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.SYNCHRONOUS_IDLE);

	public static final CharacterStruct END_OF_TRANSMISSION_BLOCK_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.END_OF_TRANSMISSION_BLOCK);

	public static final CharacterStruct CANCEL_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.CANCEL);

	public static final CharacterStruct END_OF_MEDIUM_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.END_OF_MEDIUM);

	public static final CharacterStruct SUBSTITUTE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.SUBSTITUTE);

	public static final CharacterStruct ESCAPE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.ESCAPE);

	public static final CharacterStruct FILE_SEPARATOR_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.FILE_SEPARATOR);

	public static final CharacterStruct GROUP_SEPARATOR_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.GROUP_SEPARATOR);

	public static final CharacterStruct RECORD_SEPARATOR_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.RECORD_SEPARATOR);

	public static final CharacterStruct UNIT_SEPARATOR_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.UNIT_SEPARATOR);

	public static final CharacterStruct SPACE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.SPACE);

	public static final CharacterStruct EXCLAMATION_MARK_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.EXCLAMATION_MARK);

	public static final CharacterStruct QUOTATION_MARK_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.QUOTATION_MARK);

	public static final CharacterStruct NUMBER_SIGN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.NUMBER_SIGN);

	public static final CharacterStruct DOLLAR_SIGN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DOLLAR_SIGN);

	public static final CharacterStruct PERCENT_SIGN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.PERCENT_SIGN);

	public static final CharacterStruct AMPERSAND_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.AMPERSAND);

	public static final CharacterStruct APOSTROPHE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.APOSTROPHE);

	public static final CharacterStruct LEFT_PARENTHESIS_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LEFT_PARENTHESIS);

	public static final CharacterStruct RIGHT_PARENTHESIS_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.RIGHT_PARENTHESIS);

	public static final CharacterStruct ASTERISK_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.ASTERISK);

	public static final CharacterStruct PLUS_SIGN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.PLUS_SIGN);

	public static final CharacterStruct COMMA_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.COMMA);

	public static final CharacterStruct HYPHEN_MINUS_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.HYPHEN_MINUS);

	public static final CharacterStruct FULL_STOP_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.FULL_STOP);

	public static final CharacterStruct SLASH_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.SLASH);

	public static final CharacterStruct DIGIT_ZERO_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_ZERO);

	public static final CharacterStruct DIGIT_ONE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_ONE);

	public static final CharacterStruct DIGIT_TWO_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_TWO);

	public static final CharacterStruct DIGIT_THREE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_THREE);

	public static final CharacterStruct DIGIT_FOUR_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_FOUR);

	public static final CharacterStruct DIGIT_FIVE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_FIVE);

	public static final CharacterStruct DIGIT_SIX_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_SIX);

	public static final CharacterStruct DIGIT_SEVEN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_SEVEN);

	public static final CharacterStruct DIGIT_EIGHT_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_EIGHT);

	public static final CharacterStruct DIGIT_NINE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DIGIT_NINE);

	public static final CharacterStruct COLON_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.COLON);

	public static final CharacterStruct SEMICOLON_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.SEMICOLON);

	public static final CharacterStruct LESS_THAN_SIGN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LESS_THAN_SIGN);

	public static final CharacterStruct EQUALS_SIGN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.EQUALS_SIGN);

	public static final CharacterStruct GREATER_THAN_SIGN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.GREATER_THAN_SIGN);

	public static final CharacterStruct QUESTION_MARK_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.QUESTION_MARK);

	public static final CharacterStruct AT_SIGN_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.AT_SIGN);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_A_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_A);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_B_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_B);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_C_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_C);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_D_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_D);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_E_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_E);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_F_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_F);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_G_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_G);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_H_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_H);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_I_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_I);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_J_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_J);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_K_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_K);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_L_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_L);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_M_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_M);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_N_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_N);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_O_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_O);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_P_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_P);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_Q_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_Q);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_R_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_R);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_S_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_S);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_T_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_T);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_U_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_U);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_V_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_V);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_W_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_W);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_X_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_X);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_Y_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_Y);

	public static final CharacterStruct LATIN_CAPITAL_LETTER_Z_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_CAPITAL_LETTER_Z);

	public static final CharacterStruct LEFT_SQUARE_BRACKET_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LEFT_SQUARE_BRACKET);

	public static final CharacterStruct BACKSLASH_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.BACKSLASH);

	public static final CharacterStruct RIGHT_SQUARE_BRACKET_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.RIGHT_SQUARE_BRACKET);

	public static final CharacterStruct CIRCUMFLEX_ACCENT_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.CIRCUMFLEX_ACCENT);

	public static final CharacterStruct LOW_LINE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LOW_LINE);

	public static final CharacterStruct GRAVE_ACCENT_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.GRAVE_ACCENT);

	public static final CharacterStruct LATIN_SMALL_LETTER_A_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_A);

	public static final CharacterStruct LATIN_SMALL_LETTER_B_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_B);

	public static final CharacterStruct LATIN_SMALL_LETTER_C_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_C);

	public static final CharacterStruct LATIN_SMALL_LETTER_D_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_D);

	public static final CharacterStruct LATIN_SMALL_LETTER_E_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_E);

	public static final CharacterStruct LATIN_SMALL_LETTER_F_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_F);

	public static final CharacterStruct LATIN_SMALL_LETTER_G_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_G);

	public static final CharacterStruct LATIN_SMALL_LETTER_H_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_H);

	public static final CharacterStruct LATIN_SMALL_LETTER_I_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_I);

	public static final CharacterStruct LATIN_SMALL_LETTER_J_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_J);

	public static final CharacterStruct LATIN_SMALL_LETTER_K_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_K);

	public static final CharacterStruct LATIN_SMALL_LETTER_L_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_L);

	public static final CharacterStruct LATIN_SMALL_LETTER_M_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_M);

	public static final CharacterStruct LATIN_SMALL_LETTER_N_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_N);

	public static final CharacterStruct LATIN_SMALL_LETTER_O_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_O);

	public static final CharacterStruct LATIN_SMALL_LETTER_P_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_P);

	public static final CharacterStruct LATIN_SMALL_LETTER_Q_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_Q);

	public static final CharacterStruct LATIN_SMALL_LETTER_R_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_R);

	public static final CharacterStruct LATIN_SMALL_LETTER_S_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_S);

	public static final CharacterStruct LATIN_SMALL_LETTER_T_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_T);

	public static final CharacterStruct LATIN_SMALL_LETTER_U_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_U);

	public static final CharacterStruct LATIN_SMALL_LETTER_V_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_V);

	public static final CharacterStruct LATIN_SMALL_LETTER_W_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_W);

	public static final CharacterStruct LATIN_SMALL_LETTER_X_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_X);

	public static final CharacterStruct LATIN_SMALL_LETTER_Y_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_Y);

	public static final CharacterStruct LATIN_SMALL_LETTER_Z_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LATIN_SMALL_LETTER_Z);

	public static final CharacterStruct LEFT_CURLY_BRACKET_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.LEFT_CURLY_BRACKET);

	public static final CharacterStruct VERTICAL_LINE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.VERTICAL_LINE);

	public static final CharacterStruct RIGHT_CURLY_BRACKET_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.RIGHT_CURLY_BRACKET);

	public static final CharacterStruct TILDE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.TILDE);

	public static final CharacterStruct DELETE_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.DELETE);

	public static final CharacterStruct RUBOUT_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.RUBOUT);

	public static final CharacterStruct EXIT_CHAR_CHAR = CharacterStruct.toLispCharacter(CodePointConstants.EXIT_CHAR);

	// Standard-Char Map Constant

	public static final Map<Integer, CharacterStruct> STANDARD_CHAR_MAP;

	// Common-Lisp Special Graphic-Character Name Map Constant

	public static final Map<String, CharacterStruct> CL_GRAPHIC_CHAR_NAME_MAP;

	static {
		final Map<Integer, CharacterStruct> tempStandardCharMap = new HashMap<>();
		tempStandardCharMap.put(CodePointConstants.EOF, EOF_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.NULL, NULL_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.START_OF_HEADER, START_OF_HEADER_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.START_OF_TEXT, START_OF_TEXT_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.END_OF_TEXT, END_OF_TEXT_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.END_OF_TRANSMISSION, END_OF_TRANSMISSION_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.ENQUIRY, ENQUIRY_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.ACKNOWLEDGE, ACKNOWLEDGE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.BELL, BELL_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.BACKSPACE, BACKSPACE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.TAB, TAB_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LINE_FEED, LINE_FEED_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.NEWLINE, NEWLINE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.VERTICAL_TAB, VERTICAL_TAB_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.FORM_FEED, FORM_FEED_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.PAGE, PAGE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.CARRIAGE_RETURN, CARRIAGE_RETURN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.RETURN, RETURN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.SHIFT_OUT, SHIFT_OUT_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.SHIFT_IN, SHIFT_IN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DATA_LINK_ESCAPE, DATA_LINK_ESCAPE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DEVICE_CONTROL_1, DEVICE_CONTROL_1_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DEVICE_CONTROL_2, DEVICE_CONTROL_2_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DEVICE_CONTROL_3, DEVICE_CONTROL_3_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DEVICE_CONTROL_4, DEVICE_CONTROL_4_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.NEGATIVE_ACKNOWLEDGE, NEGATIVE_ACKNOWLEDGE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.SYNCHRONOUS_IDLE, SYNCHRONOUS_IDLE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.END_OF_TRANSMISSION_BLOCK, END_OF_TRANSMISSION_BLOCK_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.CANCEL, CANCEL_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.END_OF_MEDIUM, END_OF_MEDIUM_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.SUBSTITUTE, SUBSTITUTE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.ESCAPE, ESCAPE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.FILE_SEPARATOR, FILE_SEPARATOR_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.GROUP_SEPARATOR, GROUP_SEPARATOR_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.RECORD_SEPARATOR, RECORD_SEPARATOR_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.UNIT_SEPARATOR, UNIT_SEPARATOR_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.SPACE, SPACE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.EXCLAMATION_MARK, EXCLAMATION_MARK_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.QUOTATION_MARK, QUOTATION_MARK_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.NUMBER_SIGN, NUMBER_SIGN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DOLLAR_SIGN, DOLLAR_SIGN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.PERCENT_SIGN, PERCENT_SIGN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.AMPERSAND, AMPERSAND_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.APOSTROPHE, APOSTROPHE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LEFT_PARENTHESIS, LEFT_PARENTHESIS_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.RIGHT_PARENTHESIS, RIGHT_PARENTHESIS_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.ASTERISK, ASTERISK_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.PLUS_SIGN, PLUS_SIGN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.COMMA, COMMA_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.HYPHEN_MINUS, HYPHEN_MINUS_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.FULL_STOP, FULL_STOP_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.SLASH, SLASH_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_ZERO, DIGIT_ZERO_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_ONE, DIGIT_ONE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_TWO, DIGIT_TWO_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_THREE, DIGIT_THREE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_FOUR, DIGIT_FOUR_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_FIVE, DIGIT_FIVE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_SIX, DIGIT_SIX_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_SEVEN, DIGIT_SEVEN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_EIGHT, DIGIT_EIGHT_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DIGIT_NINE, DIGIT_NINE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.COLON, COLON_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.SEMICOLON, SEMICOLON_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LESS_THAN_SIGN, LESS_THAN_SIGN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.EQUALS_SIGN, EQUALS_SIGN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.GREATER_THAN_SIGN, GREATER_THAN_SIGN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.QUESTION_MARK, QUESTION_MARK_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.AT_SIGN, AT_SIGN_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_A, LATIN_CAPITAL_LETTER_A_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_B, LATIN_CAPITAL_LETTER_B_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_C, LATIN_CAPITAL_LETTER_C_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_D, LATIN_CAPITAL_LETTER_D_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_E, LATIN_CAPITAL_LETTER_E_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_F, LATIN_CAPITAL_LETTER_F_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_G, LATIN_CAPITAL_LETTER_G_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_H, LATIN_CAPITAL_LETTER_H_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_I, LATIN_CAPITAL_LETTER_I_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_J, LATIN_CAPITAL_LETTER_J_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_K, LATIN_CAPITAL_LETTER_K_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_L, LATIN_CAPITAL_LETTER_L_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_M, LATIN_CAPITAL_LETTER_M_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_N, LATIN_CAPITAL_LETTER_N_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_O, LATIN_CAPITAL_LETTER_O_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_P, LATIN_CAPITAL_LETTER_P_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_Q, LATIN_CAPITAL_LETTER_Q_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_R, LATIN_CAPITAL_LETTER_R_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_S, LATIN_CAPITAL_LETTER_S_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_T, LATIN_CAPITAL_LETTER_T_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_U, LATIN_CAPITAL_LETTER_U_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_V, LATIN_CAPITAL_LETTER_V_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_W, LATIN_CAPITAL_LETTER_W_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_X, LATIN_CAPITAL_LETTER_X_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_Y, LATIN_CAPITAL_LETTER_Y_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_CAPITAL_LETTER_Z, LATIN_CAPITAL_LETTER_Z_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LEFT_SQUARE_BRACKET, LEFT_SQUARE_BRACKET_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.BACKSLASH, BACKSLASH_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.RIGHT_SQUARE_BRACKET, RIGHT_SQUARE_BRACKET_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.CIRCUMFLEX_ACCENT, CIRCUMFLEX_ACCENT_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LOW_LINE, LOW_LINE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.GRAVE_ACCENT, GRAVE_ACCENT_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_A, LATIN_SMALL_LETTER_A_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_B, LATIN_SMALL_LETTER_B_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_C, LATIN_SMALL_LETTER_C_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_D, LATIN_SMALL_LETTER_D_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_E, LATIN_SMALL_LETTER_E_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_F, LATIN_SMALL_LETTER_F_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_G, LATIN_SMALL_LETTER_G_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_H, LATIN_SMALL_LETTER_H_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_I, LATIN_SMALL_LETTER_I_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_J, LATIN_SMALL_LETTER_J_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_K, LATIN_SMALL_LETTER_K_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_L, LATIN_SMALL_LETTER_L_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_M, LATIN_SMALL_LETTER_M_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_N, LATIN_SMALL_LETTER_N_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_O, LATIN_SMALL_LETTER_O_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_P, LATIN_SMALL_LETTER_P_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_Q, LATIN_SMALL_LETTER_Q_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_R, LATIN_SMALL_LETTER_R_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_S, LATIN_SMALL_LETTER_S_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_T, LATIN_SMALL_LETTER_T_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_U, LATIN_SMALL_LETTER_U_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_V, LATIN_SMALL_LETTER_V_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_W, LATIN_SMALL_LETTER_W_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_X, LATIN_SMALL_LETTER_X_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_Y, LATIN_SMALL_LETTER_Y_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LATIN_SMALL_LETTER_Z, LATIN_SMALL_LETTER_Z_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.LEFT_CURLY_BRACKET, LEFT_CURLY_BRACKET_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.VERTICAL_LINE, VERTICAL_LINE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.RIGHT_CURLY_BRACKET, RIGHT_CURLY_BRACKET_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.TILDE, TILDE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.DELETE, DELETE_CHAR);
		tempStandardCharMap.put((int) CodePointConstants.RUBOUT, RUBOUT_CHAR);
		tempStandardCharMap.put(CodePointConstants.EXIT_CHAR, EXIT_CHAR_CHAR);

		STANDARD_CHAR_MAP = Collections.unmodifiableMap(tempStandardCharMap);

		final Map<String, CharacterStruct> tempGraphicCharNameMap = new HashMap<>();
		tempGraphicCharNameMap.put("NULL", NULL_CHAR);
		tempGraphicCharNameMap.put("^@", NULL_CHAR);
		tempGraphicCharNameMap.put("BACKSPACE", BACKSPACE_CHAR);
		tempGraphicCharNameMap.put("TAB", TAB_CHAR);
		tempGraphicCharNameMap.put("NEWLINE", NEWLINE_CHAR);
		tempGraphicCharNameMap.put("LINEFEED", LINE_FEED_CHAR);
		tempGraphicCharNameMap.put("RETURN", RETURN_CHAR);
		tempGraphicCharNameMap.put("RUBOUT", RUBOUT_CHAR);

		CL_GRAPHIC_CHAR_NAME_MAP = Collections.unmodifiableMap(tempGraphicCharNameMap);
	}
}
