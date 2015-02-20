/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.compiler.real.element.specialoperator;

import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import java.util.UUID;

public class ImmutableLoadTimeValueElement implements LoadTimeValueElement {

	private static final long serialVersionUID = 857211495712280441L;

	private final UUID uniqueLTVId;

	public ImmutableLoadTimeValueElement(final UUID uniqueLTVId) {
		this.uniqueLTVId = uniqueLTVId;
	}

	public UUID getUniqueLTVId() {
		return uniqueLTVId;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
