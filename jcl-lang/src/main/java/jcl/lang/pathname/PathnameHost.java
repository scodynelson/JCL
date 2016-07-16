/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.pathname;

import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameHost} is the object representation of the 'host' element of a Lisp 'pathname' type.
 */
public final class PathnameHost {

	/**
	 * The pathname host value.
	 */
	private final String host;

	/**
	 * The pathname host component type.
	 */
	private final PathnameComponentType componentType;

	/**
	 * Public constructor.
	 */
	public PathnameHost() {
		host = null;
		componentType = PathnameComponentType.UNSPECIFIC;
	}

	/**
	 * Public constructor.
	 *
	 * @param host
	 * 		the pathname host
	 */
	public PathnameHost(final String host) {
		this.host = host;

		if (StringUtils.isEmpty(host)) {
			componentType = PathnameComponentType.NIL;
		} else {
			componentType = null;
		}
	}

	/**
	 * Public constructor.
	 *
	 * @param componentType
	 * 		pathname host component type
	 */
	public PathnameHost(final PathnameComponentType componentType) {
		host = null;
		this.componentType = componentType;
	}

	/**
	 * Getter for pathname host {@link #host} property.
	 *
	 * @return pathname host {@link #host} property
	 */
	public String getHost() {
		return host;
	}

	/**
	 * Getter for pathname host {@link #componentType} property.
	 *
	 * @return pathname host {@link #componentType} property
	 */
	public PathnameComponentType getComponentType() {
		return componentType;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(host)
		                            .append(componentType)
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
		final PathnameHost rhs = (PathnameHost) obj;
		return new EqualsBuilder().append(host, rhs.host)
		                          .append(componentType, rhs.componentType)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(host)
		                                                                .append(componentType)
		                                                                .toString();
	}
}
