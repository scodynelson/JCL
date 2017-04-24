/*
 * Copyright (C) 2011-2014 Cody Nelson - All rights reserved.
 */

package jcl.lang.pathname;

import jcl.lang.condition.exception.FileErrorException;
import org.apache.commons.lang3.StringUtils;

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
}
