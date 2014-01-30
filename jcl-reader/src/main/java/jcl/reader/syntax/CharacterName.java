package jcl.reader.syntax;

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

	CharacterName(final String name, final char aChar) {
		this.name = name;
		this.aChar = aChar;
	}

	public String getName() {
		return name;
	}

	public char getChar() {
		return aChar;
	}
}
