/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.pathnames;

import java.io.Serializable;
import java.util.List;

import jcl.conditions.exceptions.FileErrorException;
import org.apache.commons.collections4.CollectionUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

/**
 * The {@link PathnameDirectoryComponent} is the object representation of a directory component of the 'directory'
 * element of a Lisp 'pathname' type.
 */
public final class PathnameDirectoryComponent implements Serializable {

	/**
	 * Serializable Version Unique Identifier.
	 */
	private static final long serialVersionUID = -3157529987901934426L;

	/**
	 * The pathname list of directory level values.
	 */
	private final List<PathnameDirectoryLevel> directoryLevels;

	/**
	 * The pathname directory type.
	 */
	private final PathnameDirectoryType pathnameDirectoryType;

	/**
	 * Public constructor.
	 *
	 * @param directoryLevels
	 * 		the pathname directory levels
	 * @param pathnameDirectoryType
	 * 		the pathname directory type (ABSOLUTE or RELATIVE)
	 */
	public PathnameDirectoryComponent(final PathnameDirectoryType pathnameDirectoryType, final List<PathnameDirectoryLevel> directoryLevels) {
		if (CollectionUtils.isNotEmpty(directoryLevels)
				&& (pathnameDirectoryType == PathnameDirectoryType.ABSOLUTE)) {

			final PathnameDirectoryLevel firstElement = directoryLevels.get(0);
			if ((firstElement.getDirectoryLevelType() == PathnameDirectoryLevelType.BACK)
					|| (firstElement.getDirectoryLevelType() == PathnameDirectoryLevelType.UP)) {
				throw new FileErrorException(":ABSOLUTE must not be followed by :BACK or :UP.");
			}
		}

		this.directoryLevels = directoryLevels;
		this.pathnameDirectoryType = pathnameDirectoryType;
	}

	/**
	 * Getter for pathname directory component {@link #directoryLevels} property.
	 *
	 * @return pathname directory component {@link #directoryLevels} property
	 */
	public List<PathnameDirectoryLevel> getDirectoryLevels() {
		return directoryLevels;
	}

	/**
	 * Getter for pathname directory component {@link #pathnameDirectoryType} property.
	 *
	 * @return pathname directory component {@link #pathnameDirectoryType} property
	 */
	public PathnameDirectoryType getPathnameDirectoryType() {
		return pathnameDirectoryType;
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public int hashCode() {
		return HashCodeBuilder.reflectionHashCode(this);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public boolean equals(final Object obj) {
		return EqualsBuilder.reflectionEquals(this, obj);
	}

	@Override
	@SuppressWarnings("checkstyle:strictduplicatecodecheck")
	public String toString() {
		return new ToStringBuilder(this, ToStringStyle.MULTI_LINE_STYLE).toString();
	}
}
