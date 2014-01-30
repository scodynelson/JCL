package jcl.reader.syntax;

/**
 * The {@code AttributeType} enumeration defines the different attributes types that will determine how the reader parses
 * tokens.
 */
public enum AttributeType {

	INVALID,
	ALPHABETIC,
	PLUS,
	MINUS,
	DECIMAL,
	RATIOMARKER,
	ALPHADIGIT,
	PACKAGEMARKER,
	EXPONENTMARKER
}
