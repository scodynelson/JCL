/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.pathname;

import jcl.lang.condition.exception.FileErrorException;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectoryLevel} is the object representation of a specific directory level of the 'directory'
 * element of a Lisp 'pathname' type.
 */
public final class PathnameDirectoryLevel {

	/**
	 * The pathname directory level value.
	 */
	private final String directoryLevel;

	/**
	 * The pathname directory level type.
	 */
	private final PathnameDirectoryLevelType directoryLevelType;

	/**
	 * Public constructor.
	 *
	 * @param directoryLevel
	 * 		the directory level value
	 */
	public PathnameDirectoryLevel(final String directoryLevel) {
		this(directoryLevel, PathnameDirectoryLevelType.NULL);
	}

	/**
	 * Public constructor.
	 *
	 * @param directoryLevelType
	 * 		the directory level type (WILD, WILD_INFERIORS, BACK, UP, or NULL)
	 */
	public PathnameDirectoryLevel(final PathnameDirectoryLevelType directoryLevelType) {
		this(null, directoryLevelType);
	}

	/**
	 * Public constructor.
	 *
	 * @param directoryLevel
	 * 		the directory level value
	 * @param directoryLevelType
	 * 		the directory level type (WILD, WILD_INFERIORS, BACK, UP, or NULL)
	 */
	public PathnameDirectoryLevel(final String directoryLevel, final PathnameDirectoryLevelType directoryLevelType) {
		if (StringUtils.isEmpty(directoryLevel) && (PathnameDirectoryLevelType.NULL == directoryLevelType)) {
			// TODO: should this take a stream!??!?
			throw new FileErrorException("Directory level value cannot be null or empty.", null);
		}

		this.directoryLevel = directoryLevel;
		this.directoryLevelType = directoryLevelType;
	}

	/**
	 * Getter for pathname directory level {@link #directoryLevel} property.
	 *
	 * @return pathname directory level {@link #directoryLevel} property
	 */
	public String getDirectoryLevel() {
		return directoryLevel;
	}

	/**
	 * Getter for pathname directory level {@link #directoryLevelType} property.
	 *
	 * @return pathname directory level {@link #directoryLevelType} property
	 */
	public PathnameDirectoryLevelType getDirectoryLevelType() {
		return directoryLevelType;
	}

	@Override
	public int hashCode() {
		return new HashCodeBuilder().append(directoryLevel)
		                            .append(directoryLevelType)
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
		final PathnameDirectoryLevel rhs = (PathnameDirectoryLevel) obj;
		return new EqualsBuilder().append(directoryLevel, rhs.directoryLevel)
		                          .append(directoryLevelType, rhs.directoryLevelType)
		                          .isEquals();
	}

	@Override
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).append(directoryLevel)
		                                                                .append(directoryLevelType)
		                                                                .toString();
	}
}
