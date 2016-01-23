/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.reader.macrofunction;

import jcl.LispStruct;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The object representation of a Lisp 'comment' type.
 */
public class CommentStruct implements LispStruct {

	/**
	 * The read in comment string.
	 */
	private final String commentString;

	/**
	 * Public constructor.
	 *
	 * @param commentString
	 * 		the {@link String} comment value
	 */
	public CommentStruct(final String commentString) {
		this.commentString = commentString;
	}

	/**
	 * Getter for character {@link #commentString} property.
	 *
	 * @return character {@link #commentString} property
	 */
	public String getCommentString() {
		return commentString;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(commentString)
		                            .toHashCode();
	}

	@Override
	public boolean equals(final Object obj) {
		if (obj == null) {
			return false;
		}
		if (obj == this) {
			return true;
		}
		if (obj.getClass() != getClass()) {
			return false;
		}
		final CommentStruct rhs = (CommentStruct) obj;
		return new EqualsBuilder().append(commentString, rhs.commentString)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(commentString)
		                                                                .toString();
	}
}
