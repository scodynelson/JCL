/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * Defines the standard graphic characters that have names for the system.
 */
@SuppressWarnings("MagicNumber")
enum CharacterName {

	/**
	 * Null character name.
	 */
	NULL("NULL", 0x0000),

	/**
	 * Null sign character name.
	 */
	NULL_CHAR("^@", 0x0000),

	/**
	 * Backspace character name.
	 */
	BACKSPACE("BACKSPACE", 0x0008),

	/**
	 * Tab character name.
	 */
	TAB("TAB", 0x0009),

	/**
	 * Newline character name.
	 */
	NEWLINE("NEWLINE", 0x000A),

	/**
	 * Linefeed character name.
	 */
	LINEFEED("LINEFEED", 0x000A),

	/**
	 * Return character name.
	 */
	RETURN("RETURN", 0x000D),

	/**
	 * Page character name.
	 */
	PAGE("PAGE", 0x000C),

	/**
	 * Space character name.
	 */
	SPACE("SPACE", 0x0020),

	/**
	 * Rubout character name.
	 */
	RUBOUT("RUBOUT", 0x007F);

	/**
	 * String name of the character.
	 */
	private final String name;

	/**
	 * Character value.
	 */
	private final int codePoint;

	/**
	 * Public constructor.
	 *
	 * @param name
	 * 		the graphic character name
	 * @param codePoint
	 * 		the character code point value
	 */
	CharacterName(final String name, final int codePoint) {
		this.name = name;
		this.codePoint = codePoint;
	}

	/**
	 * Getter for {@link #name} property.
	 *
	 * @return {@link #name} property
	 */
	String getName() {
		return name;
	}

	/**
	 * Getter for {@link #codePoint} property.
	 *
	 * @return {@link #codePoint} property
	 */
	int getCodePoint() {
		return codePoint;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
