package jcl.readtables.reader.syntax;

/**
 * The {@code CharacterName} enumeration defines the standard graphic characters that have names for the system.
 */
public enum CharacterName {

	NULL("NULL", (char) 0x0000),
	NULL_CHAR("^@", (char) 0x0000),
	BACKSPACE("BACKSPACE", (char) 0x0008),
	TAB("TAB", (char) 0x0009),
	NEWLINE("NEWLINE", (char) 0x000A),
	LINEFEED("LINEFEED", (char) 0x000A),
	RETURN("RETURN", (char) 0x000D),
	PAGE("PAGE", (char) 0x000C),
	SPACE("SPACE", (char) 0x0020),
	RUBOUT("RUBOUT", (char) 0x007F);

	private final String name;
	private final char aChar;

	/**
	 * Public constructor.
	 *
	 * @param name  the graphic character name
	 * @param aChar the character value
	 */
	CharacterName(final String name, final char aChar) {
		this.name = name;
		this.aChar = aChar;
	}

	/**
	 * Getter for the graphic character name for the enumeration.
	 *
	 * @return the graphic character name
	 */
	public String getName() {
		return name;
	}

	/**
	 * Getter for the character value for the enumeration.
	 *
	 * @return the character value
	 */
	public char getChar() {
		return aChar;
	}
}
