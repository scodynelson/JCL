package jcl.reader.function.macrofunction;

import jcl.LispStruct;
import jcl.LispType;
import jcl.types.T;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The object representation of a Lisp 'comment' type.
 */
public class CommentStruct implements LispStruct {

	private final String commentString;

	/**
	 * Public constructor.
	 *
	 * @param commentString
	 * 		the {@link java.lang.String} comment value
	 */
	CommentStruct(final String commentString) {
		this.commentString = commentString;
	}

	/**
	 * Getter for character {@link #commentString} property.
	 *
	 * @return character {@link #commentString} property
	 */
	String getCommentString() {
		return commentString;
	}

	@Override
	public LispType getType() {
		return T.INSTANCE;
	}

	@Override
	public String printStruct() {
		return toString();
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
