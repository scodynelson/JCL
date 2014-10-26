package jcl.reader.syntax;

/**
 * The {@link SyntaxType} enumeration defines the different syntax types that will determine how the reader parses tokens.
 */
public enum SyntaxType {

	INVALID,
	WHITESPACE,
	NON_TERMINATING,
	CONSTITUENT,
	TERMINATING,
	SINGLE_ESCAPE,
	MULTIPLE_ESCAPE
}